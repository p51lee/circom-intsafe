use core::panic;

use num_bigint::BigInt;
use program_structure::abstract_syntax_tree::ast::*;

pub struct ALMeta {
    pub start: usize,
    pub end: usize,
}
pub enum ALStmt {
    ALIfThenElse {
        meta: ALMeta,
        condition: ALExpr,
        if_case: Box<ALStmt>,
        else_case: Option<Box<ALStmt>>,
    },
    ALWhile {
        meta: ALMeta,
        condition: ALExpr,
        stmt: Box<ALStmt>,
    },
    ALReturn {
        meta: ALMeta,
        value: ALExpr,
    },
    ALAssign {
        meta: ALMeta,
        var: String,
        access: Vec<ALExpr>,
        value: ALExpr,
    },
    ALAssert {
        meta: ALMeta,
        arg: ALExpr,
    },
}

impl ALStmt {
    pub fn from_stmt(stmt: &Statement) -> ALStmt {
        todo!()
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
        op: ALBopCode,
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
            } => todo!(),
            PrefixOp {
                meta,
                prefix_op,
                rhe,
            } => todo!(),
            InlineSwitchOp {
                meta,
                cond,
                if_true,
                if_false,
            } => todo!(),
            Variable { meta, name, access } => todo!(),
            Number(meta, value) => todo!(),
            ArrayInLine { meta, values } => todo!(),
            ParallelOp { .. } => panic!("ParallelOp not supported yet"),
            Call { .. } => panic!("Call not supported yet"),
            AnonymousComp { .. } => panic!("AnonymousComp not supported yet"),
            Tuple { .. } => panic!("Tuple not supported yet"),
            UniformArray { .. } => panic!("UniformArray not supported yet"),
        }
        todo!()
    }
}

pub enum ALBopCode {
    ALMul,
    ALAdd,
    ALSub,
    ALAnd,
    ALOr,
    ALXor,
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
