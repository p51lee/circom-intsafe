use core::panic;
use std::hash::{Hash, Hasher};

use num_bigint::BigInt;
use program_structure::abstract_syntax_tree::ast::*;

#[derive(Hash, Clone, Copy)]
pub struct ALMeta {
    pub start: usize,
    pub end: usize,
    pub index: usize,
}

impl ALMeta {
    pub fn empty() -> ALMeta {
        ALMeta {
            start: 0,
            end: 0,
            index: 0,
        }
    }
    pub fn from_meta(meta: &Meta) -> ALMeta {
        ALMeta {
            start: meta.start,
            end: meta.end,
            index: meta.elem_id,
        }
    }
}

impl std::fmt::Debug for ALMeta {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Clone)]
// #[derive(Clone, Debug)]
pub enum ALStmt {
    ALEmpty {
        meta: ALMeta,
        next: Option<ALMeta>,
    },
    ALBlock {
        meta: ALMeta,
        stmts: Vec<ALStmt>,
        next: Option<ALMeta>,
    },
    ALIfThenElse {
        meta: ALMeta,
        cond: ALExpr,
        if_case: Box<ALStmt>,
        else_case: Option<Box<ALStmt>>,
        next_true: ALMeta,
        next_false: Option<ALMeta>,
    },
    ALWhile {
        meta: ALMeta,
        cond: ALExpr,
        body: Box<ALStmt>,
        next_true: ALMeta,
        next_false: Option<ALMeta>,
    },
    ALReturn {
        meta: ALMeta,
        value: ALExpr,
    },
    ALAssign {
        meta: ALMeta,
        var: String,
        value: ALExpr,
        next: Option<ALMeta>,
        // access: Vec<ALExpr>, TODO: More complex abstraction for array access
    },
    ALAssert {
        meta: ALMeta,
        arg: ALExpr,
        next: Option<ALMeta>,
    },
}

impl ALStmt {
    pub fn from_stmt(stmt: &Statement) -> ALStmt {
        let mut al_stmt = ALStmt::from_stmt_suppl(stmt);
        al_stmt.reorder();
        al_stmt.init_next(None);
        al_stmt
    }
    fn meta(&self) -> ALMeta {
        use ALStmt::*;
        match self {
            ALEmpty { meta, .. }
            | ALBlock { meta, .. }
            | ALIfThenElse { meta, .. }
            | ALWhile { meta, .. }
            | ALReturn { meta, .. }
            | ALAssign { meta, .. }
            | ALAssert { meta, .. } => meta.clone(),
        }
    }
    fn first_stmt(&self) -> &ALStmt {
        use ALStmt::*;
        match self {
            ALBlock { stmts, .. } => stmts.first().map_or(self, |stmt| stmt.first_stmt()),
            _ => self,
        }
    }

    fn init_next(&mut self, next: Option<ALMeta>) {
        let self_meta = self.meta();
        use ALStmt::*;
        match self {
            ALReturn { .. } => {}
            ALEmpty { next: n, .. } | ALAssign { next: n, .. } | ALAssert { next: n, .. } => {
                *n = next;
            }
            ALBlock { stmts, next: n, .. } => {
                if stmts.len() == 0 {
                    *n = next
                } else {
                    *n = Some(stmts[0].first_stmt().meta());
                    for i in 0..stmts.len() {
                        if i == stmts.len() - 1 {
                            stmts[i].init_next(next);
                        } else {
                            let (left, right) = stmts.split_at_mut(i + 1);
                            let current_stmt = &mut left[i]; // Mutable reference to the current statement
                            let next_stmt = &right[0].first_stmt(); // Immutable reference to the next statement
                            current_stmt.init_next(Some(next_stmt.meta()));
                        }
                    }
                }
            }
            ALIfThenElse {
                if_case,
                else_case,
                next_true,
                next_false,
                ..
            } => {
                if_case.init_next(next);
                *next_true = if_case.first_stmt().meta();
                if let Some(else_case) = else_case {
                    else_case.init_next(next);
                    *next_false = Some(else_case.first_stmt().meta());
                } else {
                    *next_false = next;
                }
            }
            ALWhile {
                body,
                next_true,
                next_false,
                ..
            } => {
                *next_true = body.first_stmt().meta();
                *next_false = next;
                body.init_next(Some(self_meta));
            }
        }
    }

    fn reorder(&mut self) {
        use ALStmt::*;
        match self {
            ALBlock { stmts, .. } => {
                let mut new_stmts = Vec::new();
                for stmt in stmts.iter_mut() {
                    stmt.reorder();
                    match stmt {
                        ALEmpty { .. } => {}
                        ALBlock {
                            stmts: child_stmt, ..
                        } => new_stmts.append(child_stmt),
                        _ => new_stmts.push(stmt.clone()),
                    }
                }
                if new_stmts.len() == 0 {
                    *self = ALEmpty {
                        meta: self.meta(),
                        next: None,
                    };
                } else if new_stmts.len() == 1 {
                    *self = new_stmts[0].clone();
                } else {
                    *stmts = new_stmts;
                }
            }
            ALIfThenElse {
                if_case, else_case, ..
            } => {
                if_case.reorder();
                if let Some(else_case) = else_case {
                    else_case.reorder();
                }
            }
            ALWhile { body, .. } => {
                body.reorder();
            }
            _ => {}
        }
    }

    fn from_stmt_suppl(stmt: &Statement) -> ALStmt {
        use Statement::*;
        match stmt {
            IfThenElse {
                meta,
                cond,
                if_case,
                else_case,
            } => {
                let if_stmt = ALStmt::from_stmt_suppl(&*if_case);
                let else_stmt = else_case
                    .as_ref()
                    .map(|stmt| ALStmt::from_stmt_suppl(&*stmt));
                ALStmt::ALIfThenElse {
                    meta: ALMeta::from_meta(meta),
                    cond: ALExpr::from_expr(cond),
                    if_case: Box::new(if_stmt),
                    else_case: else_stmt.map(|stmt| Box::new(stmt)),
                    next_true: ALMeta::empty(),
                    next_false: None,
                }
            }
            While {
                meta,
                cond,
                stmt: body,
            } => {
                let al_body = ALStmt::from_stmt_suppl(&*body);
                ALStmt::ALWhile {
                    meta: ALMeta::from_meta(meta),
                    cond: ALExpr::from_expr(cond),
                    body: Box::new(al_body),
                    next_true: ALMeta::empty(),
                    next_false: None,
                }
            }
            Return { meta, value } => ALStmt::ALReturn {
                meta: ALMeta::from_meta(meta),
                value: ALExpr::from_expr(value),
            },
            InitializationBlock {
                meta,
                initializations,
                ..
            } => {
                if initializations.len() == 0 {
                    ALStmt::ALEmpty {
                        meta: ALMeta::from_meta(meta),
                        next: None,
                    }
                } else {
                    ALStmt::ALBlock {
                        meta: ALMeta::from_meta(meta),
                        stmts: initializations
                            .iter()
                            .map(|stmt| ALStmt::from_stmt_suppl(stmt))
                            .collect(),
                        next: None,
                    }
                }
            }
            Declaration { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
                next: None,
            },
            Substitution { meta, var, rhe, .. } => ALStmt::ALAssign {
                meta: ALMeta::from_meta(meta),
                var: var.clone(),
                value: ALExpr::from_expr(rhe),
                next: None,
            },
            MultSubstitution { .. } => panic!("MultSubstitution not supported yet"),
            UnderscoreSubstitution { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
                next: None,
            },
            ConstraintEquality { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
                next: None,
            },
            LogCall { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
                next: None,
            },
            Block { meta, stmts } => {
                if stmts.len() == 1 {
                    ALStmt::from_stmt_suppl(&stmts[0])
                } else if stmts.len() == 0 {
                    ALStmt::ALEmpty {
                        meta: ALMeta::from_meta(meta),
                        next: None,
                    }
                } else {
                    ALStmt::ALBlock {
                        meta: ALMeta::from_meta(meta),
                        stmts: stmts
                            .iter()
                            .map(|stmt| ALStmt::from_stmt_suppl(stmt))
                            .collect(),
                        next: None,
                    }
                }
            }
            Assert { meta, arg } => ALStmt::ALAssert {
                meta: ALMeta::from_meta(meta),
                arg: ALExpr::from_expr(arg),
                next: None,
            },
        }
    }
}

impl ALStmt {
    // Helper function to format the statement with depth tracking
    fn fmt_with_depth(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        use ALStmt::*;

        let indent = "\t".repeat(depth); // Create an indentation string based on the current depth

        match self {
            ALEmpty { meta, .. } => write!(f, "{}Empty: {:?}", indent, meta),
            ALBlock { meta, stmts, .. } => {
                write!(f, "{}Block: {:?}", indent, meta)?;
                for stmt in stmts.iter() {
                    writeln!(f)?; // Newline for each statement
                    stmt.fmt_with_depth(f, depth + 1)?; // Increase the depth for nested statements
                }
                Ok(())
            }
            ALIfThenElse {
                meta,
                cond,
                if_case,
                else_case,
                ..
            } => {
                write!(f, "{}IfThenElse: {:?}", indent, meta)?;
                writeln!(f, "\n{}Cond: {:?}", indent, cond)?;
                writeln!(f, "{}If:", indent)?;
                if_case.fmt_with_depth(f, depth + 1)?;
                if let Some(else_case) = else_case {
                    writeln!(f, "\n{}Else:", indent)?;
                    else_case.fmt_with_depth(f, depth + 1)?;
                }
                Ok(())
            }
            ALWhile {
                meta, cond, body, ..
            } => {
                write!(f, "{}While: {:?}", indent, meta)?;
                writeln!(f, "\n{}Cond: {:?}", indent, cond)?;
                writeln!(f, "{}Body:", indent)?;
                body.fmt_with_depth(f, depth + 1)
            }
            ALReturn { meta, value } => {
                write!(f, "{}Return: {:?} {:?}", indent, meta, value)
            }
            ALAssign {
                meta, var, value, ..
            } => {
                write!(f, "{}Assign: {:?} {} = {:?}", indent, meta, var, value)
            }
            ALAssert { meta, arg, .. } => {
                write!(f, "{}Assert: {:?} {:?}", indent, meta, arg)
            }
        }
    }
}

// Implement the Debug trait using the fmt_with_depth function
impl std::fmt::Debug for ALStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_depth(f, 0) // Start with a depth of 0
    }
}

impl Hash for ALStmt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ALStmt::ALEmpty { meta, .. } => meta.hash(state),
            ALStmt::ALBlock { meta, .. } => meta.hash(state),
            ALStmt::ALIfThenElse { meta, .. } => meta.hash(state),
            ALStmt::ALWhile { meta, .. } => meta.hash(state),
            ALStmt::ALReturn { meta, .. } => meta.hash(state),
            ALStmt::ALAssign { meta, .. } => meta.hash(state),
            ALStmt::ALAssert { meta, .. } => meta.hash(state),
        }
    }
}

// #[derive(Clone, Debug)]
#[derive(Clone)]
pub enum ALExpr {
    ALBop {
        meta: ALMeta,
        op: ALBopCode,
        lhe: Box<ALExpr>,
        rhe: Box<ALExpr>,
    },
    ALCmp {
        meta: ALMeta,
        op: ALCmpCode,
        lhe: Box<ALExpr>,
        rhe: Box<ALExpr>,
    },
    ALUop {
        meta: ALMeta,
        op: ALUopCode,
        arg: Box<ALExpr>,
    },
    ALVariable {
        meta: ALMeta,
        name: String,
    },
    ALNumber {
        meta: ALMeta,
        value: BigInt,
    },
    ALArrayInLine {
        meta: ALMeta,
        values: Vec<ALExpr>,
    },
}

// Implement the Debug trait for ALExpr
impl std::fmt::Debug for ALExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ALExpr::*;

        match self {
            ALBop { op, lhe, rhe, .. } => {
                write!(f, "ALBop {{ {:?}, {:?}, {:?} }}", op, lhe, rhe)
            }
            ALCmp { op, lhe, rhe, .. } => {
                write!(f, "ALCmp {{ {:?}, {:?}, {:?} }}", op, lhe, rhe)
            }
            ALUop { op, arg, .. } => {
                write!(f, "ALUop {{ {:?}, {:?} }}", op, arg)
            }
            ALVariable { name, .. } => {
                write!(f, "ALVar{{ {:?} }}", name)
            }
            ALNumber { value, .. } => {
                write!(f, "ALNum{{ {} }}", value) // BigInt is formatted as a regular integer
            }
            ALArrayInLine { values, .. } => {
                write!(f, "ALArrayInLine {{ [",)?;
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", val)?;
                }
                write!(f, "] }}")
            }
        }
    }
}

impl ALExpr {
    pub fn from_expr(expr: &Expression) -> ALExpr {
        use Expression::*;
        match expr {
            InfixOp {
                meta,
                lhe,
                infix_op,
                rhe,
            } => {
                let new_meta = ALMeta::from_meta(meta);
                use ExpressionInfixOpcode::*;
                match infix_op {
                    Mul | Add | Sub | ShiftL | ShiftR | BoolAnd | BoolOr | BitAnd | BitOr
                    | BitXor => {
                        let op = match infix_op {
                            Mul => ALBopCode::ALMul,
                            Add => ALBopCode::ALAdd,
                            Sub => ALBopCode::ALSub,
                            ShiftL => ALBopCode::ALShl,
                            ShiftR => ALBopCode::ALShr,
                            BoolAnd => ALBopCode::ALAnd,
                            BoolOr => ALBopCode::ALOr,
                            BitAnd => ALBopCode::ALBitAnd,
                            BitOr => ALBopCode::ALBitOr,
                            BitXor => ALBopCode::ALBitXor,
                            _ => panic!("Unreachable"),
                        };
                        ALExpr::ALBop {
                            meta: new_meta,
                            op,
                            lhe: Box::new(ALExpr::from_expr(&*lhe)),
                            rhe: Box::new(ALExpr::from_expr(&*rhe)),
                        }
                    }
                    LesserEq | GreaterEq | Lesser | Greater | Eq | NotEq => {
                        let op = match infix_op {
                            LesserEq => ALCmpCode::ALLe,
                            GreaterEq => ALCmpCode::ALGe,
                            Lesser => ALCmpCode::ALLt,
                            Greater => ALCmpCode::ALGt,
                            Eq => ALCmpCode::ALEq,
                            NotEq => ALCmpCode::ALNe,
                            _ => panic!("Unreachable"),
                        };
                        ALExpr::ALCmp {
                            meta: new_meta,
                            op,
                            lhe: Box::new(ALExpr::from_expr(&*lhe)),
                            rhe: Box::new(ALExpr::from_expr(&*rhe)),
                        }
                    }
                    Div => panic!("Div not supported yet"),
                    Pow => panic!("Pow not supported yet"),
                    IntDiv => panic!("IntDiv not supported yet"),
                    Mod => panic!("Mod not supported yet"),
                }
            }
            PrefixOp {
                meta,
                prefix_op,
                rhe,
            } => {
                let new_meta = ALMeta::from_meta(meta);
                use ExpressionPrefixOpcode::*;
                match prefix_op {
                    Sub | BoolNot => {
                        let op = match prefix_op {
                            Sub => ALUopCode::ALNeg,
                            BoolNot => ALUopCode::ALBoolNot,
                            _ => panic!("Unreachable"),
                        };
                        ALExpr::ALUop {
                            meta: new_meta,
                            op,
                            arg: Box::new(ALExpr::from_expr(&*rhe)),
                        }
                    }
                    Complement => panic!("Complement operations not supported yet"),
                }
            }
            Variable { meta, name, .. } => ALExpr::ALVariable {
                meta: ALMeta::from_meta(meta),
                name: name.clone(),
            },
            Number(meta, value) => ALExpr::ALNumber {
                meta: ALMeta::from_meta(meta),
                value: value.clone(),
            },
            ArrayInLine { meta, values } => ALExpr::ALArrayInLine {
                meta: ALMeta::from_meta(meta),
                values: values.iter().map(|e| ALExpr::from_expr(e)).collect(),
            },
            InlineSwitchOp { .. } => panic!("InlineSwitchOp not supported yet"),
            ParallelOp { .. } => panic!("ParallelOp not supported yet"),
            Call { .. } => panic!("Call not supported yet"),
            AnonymousComp { .. } => panic!("AnonymousComp not supported yet"),
            Tuple { .. } => panic!("Tuple not supported yet"),
            UniformArray { .. } => panic!("UniformArray not supported yet"),
        }
    }
    fn from_access(accesses: &Vec<Access>) -> Vec<ALExpr> {
        accesses
            .iter()
            .map(|access| match access {
                Access::ComponentAccess(_) => panic!("ComponentAccess should be inlined"),
                Access::ArrayAccess(e) => ALExpr::from_expr(e),
            })
            .collect()
    }
}

#[derive(Clone, Debug)]
pub enum ALBopCode {
    ALMul,
    ALAdd,
    ALSub,
    ALAnd,
    ALOr,
    ALShl,
    ALShr,
    ALBitAnd,
    ALBitOr,
    ALBitXor,
}

#[derive(Clone, Debug)]
pub enum ALCmpCode {
    ALLe,
    ALGe,
    ALLt,
    ALGt,
    ALEq,
    ALNe,
}

#[derive(Clone, Debug)]
pub enum ALUopCode {
    ALNeg,
    ALBoolNot,
}
