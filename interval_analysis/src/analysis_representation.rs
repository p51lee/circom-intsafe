use core::panic;

use num_bigint::BigInt;
use program_structure::abstract_syntax_tree::ast::*;

pub struct ALMeta {
    pub start: usize,
    pub end: usize,
}

impl ALMeta {
    pub fn from_meta(meta: &Meta) -> ALMeta {
        ALMeta {
            start: meta.start,
            end: meta.end,
        }
    }
}
pub enum ALStmt {
    ALEmpty {
        meta: ALMeta,
    },
    ALBlock {
        meta: ALMeta,
        stmts: Vec<ALStmt>,
    },
    ALIfThenElse {
        meta: ALMeta,
        cond: ALExpr,
        if_case: Box<ALStmt>,
        else_case: Option<Box<ALStmt>>,
    },
    ALWhile {
        meta: ALMeta,
        cond: ALExpr,
        stmt: Box<ALStmt>,
    },
    ALReturn {
        meta: ALMeta,
        value: ALExpr,
    },
    ALAssign {
        meta: ALMeta,
        var: String,
        value: ALExpr,
        // access: Vec<ALExpr>, TODO: More complex abstraction for array access
    },
    ALAssert {
        meta: ALMeta,
        arg: ALExpr,
    },
}

impl ALStmt {
    pub fn from_stmt(stmt: &Statement) -> ALStmt {
        use Statement::*;
        match stmt {
            IfThenElse {
                meta,
                cond,
                if_case,
                else_case,
            } => ALStmt::ALIfThenElse {
                meta: ALMeta::from_meta(meta),
                cond: ALExpr::from_expr(cond),
                if_case: Box::new(ALStmt::from_stmt(&*if_case)),
                else_case: match else_case {
                    Some(else_case) => Some(Box::new(ALStmt::from_stmt(&*else_case))),
                    None => None,
                },
            },
            While { meta, cond, stmt } => ALStmt::ALWhile {
                meta: ALMeta::from_meta(meta),
                cond: ALExpr::from_expr(cond),
                stmt: Box::new(ALStmt::from_stmt(&*stmt)),
            },
            Return { meta, value } => ALStmt::ALReturn {
                meta: ALMeta::from_meta(meta),
                value: ALExpr::from_expr(value),
            },
            InitializationBlock {
                meta,
                initializations,
                ..
            } => ALStmt::ALBlock {
                meta: ALMeta::from_meta(meta),
                stmts: initializations
                    .iter()
                    .map(|stmt| ALStmt::from_stmt(stmt))
                    .collect(),
            },
            Declaration { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
            },
            Substitution { meta, var, rhe, .. } => ALStmt::ALAssign {
                meta: ALMeta::from_meta(meta),
                var: var.clone(),
                value: ALExpr::from_expr(rhe),
            },
            MultSubstitution { .. } => panic!("MultSubstitution not supported yet"),
            UnderscoreSubstitution { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
            },
            ConstraintEquality { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
            },
            LogCall { meta, .. } => ALStmt::ALEmpty {
                meta: ALMeta::from_meta(meta),
            },
            Block { meta, stmts } => ALStmt::ALBlock {
                meta: ALMeta::from_meta(meta),
                stmts: stmts.iter().map(|stmt| ALStmt::from_stmt(stmt)).collect(),
            },
            Assert { meta, arg } => ALStmt::ALAssert {
                meta: ALMeta::from_meta(meta),
                arg: ALExpr::from_expr(arg),
            },
        }
    }
}

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

pub enum ALBopCode {
    ALMul,
    ALAdd,
    ALSub,
    ALAnd,
    ALOr,
    ALShl,
    ALShr,
}

pub enum ALCmpCode {
    ALLe,
    ALGe,
    ALLt,
    ALGt,
    ALEq,
    ALNe,
}

pub enum ALUopCode {
    ALNeg,
    ALBoolNot,
}
