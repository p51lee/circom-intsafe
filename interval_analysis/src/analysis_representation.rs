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

#[derive(Clone)]
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
        access: Vec<ALExpr>,
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
                    Mul | Add | Sub | ShiftL | ShiftR | BoolAnd | BoolOr => {
                        let op = match infix_op {
                            Mul => ALBopCode::ALMul,
                            Add => ALBopCode::ALAdd,
                            Sub => ALBopCode::ALSub,
                            ShiftL => ALBopCode::ALShl,
                            ShiftR => ALBopCode::ALShr,
                            BoolAnd => ALBopCode::ALAnd,
                            BoolOr => ALBopCode::ALOr,
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
                    BitOr | BitAnd | BitXor => panic!("Bitwise operations not supported yet"),
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
            Variable { meta, name, access } => ALExpr::ALVariable {
                meta: ALMeta::from_meta(meta),
                name: name.clone(),
                access: ALExpr::from_access(access),
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

#[derive(Clone)]
pub enum ALBopCode {
    ALMul,
    ALAdd,
    ALSub,
    ALAnd,
    ALOr,
    ALShl,
    ALShr,
}

#[derive(Clone)]
pub enum ALCmpCode {
    ALLe,
    ALGe,
    ALLt,
    ALGt,
    ALEq,
    ALNe,
}

#[derive(Clone)]
pub enum ALUopCode {
    ALNeg,
    ALBoolNot,
}
