use core::panic;
use std::hash::{Hash, Hasher};

use num_bigint::BigInt;
use program_structure::abstract_syntax_tree::ast::*;

#[derive(Hash, Clone, Copy, Eq, PartialEq)]
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
        write!(_f, "({}~{})", self.start, self.end)
    }
}

#[derive(Clone)]
pub enum ALAtomic {
    Var(String),
    Num(BigInt),
}

impl ALAtomic {
    pub fn from_expr(expr: &Expression) -> ALAtomic {
        use Expression::*;
        match expr {
            Variable { name, .. } => ALAtomic::Var(name.clone()),
            Number(_, value) => ALAtomic::Num(value.clone()),
            _ => panic!("Only variables and numbers are atomic"),
        }
    }
    pub fn to_al_expr(&self) -> ALExpr {
        use ALAtomic::*;
        match self {
            Var(name) => ALExpr::ALVariable {
                meta: ALMeta::empty(),
                name: name.clone(),
            },
            Num(value) => ALExpr::ALNumber {
                meta: ALMeta::empty(),
                value: value.clone(),
            },
        }
    }
}

impl std::fmt::Debug for ALAtomic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ALAtomic::*;
        match self {
            Var(name) => write!(f, "Var({})", name),
            Num(value) => write!(f, "Num({})", value.to_string()),
        }
    }
}

#[derive(Clone)]
pub struct ALCond {
    pub lhs: ALAtomic,
    pub rhs: ALAtomic,
    pub pred: ALCmpCode,
}

impl ALCond {
    pub fn from_expr(expr: &Expression) -> ALCond {
        use Expression::*;
        match expr {
            InfixOp {
                lhe, infix_op, rhe, ..
            } => {
                use ExpressionInfixOpcode::*;
                match infix_op {
                    Mul | Add | Sub | ShiftL | ShiftR | BoolAnd | BoolOr | BitAnd | BitOr
                    | BitXor | Div | IntDiv | Pow | Mod => {
                        panic!("Binary operations not supported in conditions yet")
                    }
                    LesserEq | GreaterEq | Lesser | Greater | Eq | NotEq => {
                        let pred = match infix_op {
                            LesserEq => ALCmpCode::ALLe,
                            GreaterEq => ALCmpCode::ALGe,
                            Lesser => ALCmpCode::ALLt,
                            Greater => ALCmpCode::ALGt,
                            Eq => ALCmpCode::ALEq,
                            NotEq => ALCmpCode::ALNe,
                            _ => panic!("Unreachable"),
                        };
                        ALCond {
                            lhs: ALAtomic::from_expr(lhe),
                            rhs: ALAtomic::from_expr(rhe),
                            pred,
                        }
                    }
                }
            }
            PrefixOp { .. } => panic!("Prefix operations not supported in conditions"),
            Variable { name, .. } => ALCond {
                lhs: ALAtomic::Var(name.clone()),
                rhs: ALAtomic::Num(BigInt::from(1)),
                pred: ALCmpCode::ALGe,
            },
            Number(_, value) => ALCond {
                lhs: ALAtomic::Num(value.clone()),
                rhs: ALAtomic::Num(BigInt::from(1)),
                pred: ALCmpCode::ALGe,
            },
            ArrayInLine { .. } => panic!("Array literals not supported in conditions"),
            InlineSwitchOp { .. } => panic!("InlineSwitchOp not supported in conditions"),
            ParallelOp { .. } => panic!("ParallelOp not supported in conditions"),
            Call { .. } => panic!("Call not supported in conditions"),
            AnonymousComp { .. } => panic!("AnonymousComp not supported in conditions"),
            Tuple { .. } => panic!("Tuple not supported in conditions"),
            UniformArray { .. } => panic!("UniformArray not supported in conditions"),
        }
    }
}

impl std::fmt::Debug for ALCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?} {:?}", self.pred, self.lhs, self.rhs)
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
    ALInput {
        meta: ALMeta,
        name: String,
        next: Option<ALMeta>,
    },
    ALIfThenElse {
        meta: ALMeta,
        cond: ALCond,
        if_case: Box<ALStmt>,
        else_case: Option<Box<ALStmt>>,
        next_true: ALMeta,
        next_false: Option<ALMeta>,
    },
    ALWhile {
        meta: ALMeta,
        cond: ALCond,
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
        cond: ALCond,
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
    pub fn get_next(&self) -> Vec<Option<ALMeta>> {
        use ALStmt::*;
        match self {
            ALEmpty { next, .. }
            | ALBlock { next, .. }
            | ALInput { next, .. }
            | ALAssign { next, .. }
            | ALAssert { next, .. } => vec![*next],
            ALIfThenElse {
                next_true,
                next_false,
                ..
            }
            | ALWhile {
                next_true,
                next_false,
                ..
            } => vec![*next_false, Some(*next_true)],
            ALReturn { .. } => vec![None],
        }
    }
    pub fn meta(&self) -> ALMeta {
        use ALStmt::*;
        match self {
            ALEmpty { meta, .. }
            | ALBlock { meta, .. }
            | ALInput { meta, .. }
            | ALIfThenElse { meta, .. }
            | ALWhile { meta, .. }
            | ALReturn { meta, .. }
            | ALAssign { meta, .. }
            | ALAssert { meta, .. } => meta.clone(),
        }
    }
    fn first_stmt(&self) -> &ALStmt {
        // use ALStmt::*;
        match self {
            // ALBlock { stmts, .. } => stmts.first().map_or(self, |stmt| stmt.first_stmt()),
            _ => self,
        }
    }

    fn init_next(&mut self, next: Option<ALMeta>) {
        let self_meta = self.meta();
        use ALStmt::*;
        match self {
            ALReturn { .. } => {}
            ALEmpty { next: n, .. }
            | ALInput { next: n, .. }
            | ALAssign { next: n, .. }
            | ALAssert { next: n, .. } => {
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
                    cond: ALCond::from_expr(cond),
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
                    cond: ALCond::from_expr(cond),
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
            Declaration {
                meta, xtype, name, ..
            } => match xtype {
                VariableType::Signal(SignalType::Input, _) => ALStmt::ALInput {
                    meta: ALMeta::from_meta(meta),
                    name: name.clone(),
                    next: None,
                },
                _ => ALStmt::ALEmpty {
                    meta: ALMeta::from_meta(meta),
                    next: None,
                },
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
                cond: ALCond::from_expr(arg),
                next: None,
            },
        }
    }

    pub fn all_instrs(&self) -> Vec<Self> {
        let mut instrs = Vec::new();
        self.all_instrs_suppl(&mut instrs);
        instrs
    }

    fn all_instrs_suppl(&self, instrs: &mut Vec<Self>) {
        use ALStmt::*;
        instrs.push(self.clone());
        match self {
            ALBlock { stmts, .. } => {
                for stmt in stmts.iter() {
                    stmt.all_instrs_suppl(instrs);
                }
            }
            ALIfThenElse {
                if_case, else_case, ..
            } => {
                if_case.all_instrs_suppl(instrs);
                if let Some(else_case) = else_case {
                    else_case.all_instrs_suppl(instrs);
                }
            }
            ALWhile { body, .. } => {
                body.all_instrs_suppl(instrs);
            }
            ALEmpty { .. }
            | ALInput { .. }
            | ALReturn { .. }
            | ALAssign { .. }
            | ALAssert { .. } => (),
        }
    }

    pub fn all_metas(&self) -> Vec<ALMeta> {
        let mut metas = Vec::new();
        self.all_metas_suppl(&mut metas);
        metas
    }

    fn all_metas_suppl(&self, metas: &mut Vec<ALMeta>) {
        metas.push(self.meta());
        use ALStmt::*;
        match self {
            ALBlock { stmts, .. } => {
                for stmt in stmts.iter() {
                    stmt.all_metas_suppl(metas);
                }
            }
            ALIfThenElse {
                if_case, else_case, ..
            } => {
                if_case.all_metas_suppl(metas);
                if let Some(else_case) = else_case {
                    else_case.all_metas_suppl(metas);
                }
            }
            ALWhile { body, .. } => {
                body.all_metas_suppl(metas);
            }
            ALEmpty { .. }
            | ALInput { .. }
            | ALReturn { .. }
            | ALAssign { .. }
            | ALAssert { .. } => (),
        }
    }
    // Helper function to format the statement with depth tracking
    fn fmt_with_depth(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        use ALStmt::*;

        let indent = "\t".repeat(depth); // Create an indentation string based on the current depth

        match self {
            ALEmpty { meta, next } => write!(f, "{}Empty: {:?} \t--> {:?}", indent, meta, next),
            ALBlock {
                meta, stmts, next, ..
            } => {
                write!(f, "{}Block: {:?} \t--> {:?}", indent, meta, next)?;
                for stmt in stmts.iter() {
                    writeln!(f)?; // Newline for each statement
                    stmt.fmt_with_depth(f, depth + 1)?; // Increase the depth for nested statements
                }
                Ok(())
            }
            ALInput { meta, name, next } => {
                write!(f, "{}Input: {:?} {} \t--> {:?}", indent, meta, name, next)
            }
            ALIfThenElse {
                meta,
                cond,
                if_case,
                else_case,
                next_true,
                next_false,
                ..
            } => {
                write!(
                    f,
                    "{}IfThenElse: {:?} \t--> {:?} or {:?}",
                    indent, meta, next_true, next_false
                )?;
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
                meta,
                cond,
                body,
                next_true,
                next_false,
                ..
            } => {
                write!(
                    f,
                    "{}While: {:?} \t--> {:?} or {:?}",
                    indent, meta, next_true, next_false
                )?;
                writeln!(f, "\n{}Cond: {:?}", indent, cond)?;
                writeln!(f, "{}Body:", indent)?;
                body.fmt_with_depth(f, depth + 1)
            }
            ALReturn { meta, value } => {
                write!(f, "{}Return: {:?} {:?}", indent, meta, value)
            }
            ALAssign {
                meta,
                var,
                value,
                next,
                ..
            } => {
                write!(
                    f,
                    "{}Assign: {:?} {}\t<-- {:?} \t--> {:?}",
                    indent, meta, var, value, next
                )
            }
            ALAssert {
                meta, cond, next, ..
            } => {
                write!(
                    f,
                    "{}Assert: {:?} {:?} \t--> {:?}",
                    indent, meta, cond, next
                )
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
            ALStmt::ALInput { meta, .. } => meta.hash(state),
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

impl ALCmpCode {
    pub fn negate(&self) -> ALCmpCode {
        use ALCmpCode::*;
        match self {
            ALLe => ALGt,
            ALGe => ALLt,
            ALLt => ALGe,
            ALGt => ALLe,
            ALEq => ALNe,
            ALNe => ALEq,
        }
    }
    pub fn flip(&self) -> ALCmpCode {
        use ALCmpCode::*;
        match self {
            ALLe => ALGe,
            ALGe => ALLe,
            ALLt => ALGt,
            ALGt => ALLt,
            ALEq => ALEq,
            ALNe => ALNe,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ALUopCode {
    ALNeg,
    ALBoolNot,
}
