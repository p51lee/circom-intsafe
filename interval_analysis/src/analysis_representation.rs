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
        lhs: Box<ALExpr>,
        rhs: Box<ALExpr>,
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
