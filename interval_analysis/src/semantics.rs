use crate::analysis_representation::*;
use crate::domain::{Memory, NumericalDomain, CPO};

trait Semantics {
    type V: NumericalDomain;
    type M: Memory<K = String, V = Self::V>;

    fn eval(expr: &ALExpr, memory: &Self::M) -> Self::V {
        use ALExpr::*;
        match expr {
            ALBop { op, lhe, rhe, .. } => {
                use ALBopCode::*;
                let left = Self::eval(lhe, memory);
                let right = Self::eval(rhe, memory);
                match op {
                    ALMul => left * right,
                    ALAdd => left + right,
                    ALSub => left - right,
                    ALAnd => Self::V::logic_and(&left, &right),
                    ALOr => Self::V::logic_or(&left, &right),
                    ALShl => left << right,
                    ALShr => left >> right,
                    ALBitAnd => left & right,
                    ALBitOr => left | right,
                    ALBitXor => left ^ right,
                }
            }
            ALCmp { op, lhe, rhe, .. } => {
                let left = Self::eval(lhe, memory);
                let right = Self::eval(rhe, memory);
                Self::V::cmp(op, &left, &right)
            }
            ALUop { op, arg, .. } => {
                use ALUopCode::*;
                match op {
                    ALNeg => -Self::eval(arg, memory),
                    ALBoolNot => Self::V::logic_not(&Self::eval(arg, memory)),
                }
            }
            ALVariable { name, .. } => memory.find(name),
            ALNumber { value, .. } => Self::V::from_bigint(value.clone()),
            ALArrayInLine { values, .. } => {
                let mut joined = Self::V::bottom();
                for value in values.iter() {
                    joined = Self::V::join(&joined, &Self::eval(value, memory));
                }
                joined
            }
        }
    }
    fn filter(cond: ALCond, truth: bool, memory: Self::M) -> Self::M;
    fn transfer(stmt: ALStmt, memory: Self::M) -> Vec<(ALMeta, Self::M)>;
    fn transfer_meta(meta: ALMeta, memory: Self::M) -> Vec<(ALMeta, Self::M)>;
}
