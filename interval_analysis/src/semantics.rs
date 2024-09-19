use std::collections::HashMap;
use std::vec;

use crate::analysis_representation::*;
use crate::domain::{Interval, IntervalMemory, Memory, NumericalDomain, CPO};

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
    fn filter(cond: ALCond, truth: bool, mut memory: Self::M) -> Self::M {
        let ALCond { lhs, rhs, pred } = cond;
        let lhv = Self::eval(&lhs.to_al_expr(), &memory);
        let rhv = Self::eval(&rhs.to_al_expr(), &memory);
        let pred = if truth { pred } else { pred.negate() };
        let pred_flip = pred.flip();
        if let ALAtomic::Var(lhs_name) = lhs {
            memory.add(lhs_name, Self::V::filter(pred, &lhv, &rhv))
        }
        if let ALAtomic::Var(rhs_name) = rhs {
            memory.add(rhs_name, Self::V::filter(pred_flip, &rhv, &lhv))
        }
        memory
    }
    fn transfer(stmt: ALStmt, memory: Self::M) -> Vec<(Option<ALMeta>, Self::M)> {
        use ALStmt::*;
        match stmt {
            ALEmpty { next, .. } => vec![(next, memory)],
            ALBlock { next, .. } => vec![(next, memory)],
            ALIfThenElse {
                cond,
                next_true,
                next_false,
                ..
            }
            | ALWhile {
                cond,
                next_true,
                next_false,
                ..
            } => {
                let memory_true = Self::filter(cond.clone(), true, memory.clone());
                let memory_false = Self::filter(cond.clone(), false, memory.clone());
                let mut res = vec![];
                if memory_true != Self::M::bottom() {
                    res.push((Some(next_true), memory_true));
                }
                if next_false.is_some() {
                    if memory_false != Self::M::bottom() {
                        res.push((next_false, memory_false));
                    }
                }
                res
            }
            ALReturn { .. } => vec![(None, memory)],
            ALAssign {
                var, value, next, ..
            } => {
                let mut new_memory = memory.clone();
                let rhs_val = Self::eval(&value, &new_memory);
                new_memory.add(var, rhs_val);
                vec![(next, new_memory)]
            }
            ALAssert { cond, next, .. } => {
                let memory_true = Self::filter(cond.clone(), true, memory.clone());
                vec![(next, memory_true)]
            }
        }
    }
    fn transfer_meta(&self, meta_opt: Option<ALMeta>, memory: Self::M) -> Vec<(ALMeta, Self::M)> {
        match meta_opt {
            Some(meta) => Self::transfer(self.instr_from_meta(meta), memory)
                .into_iter()
                .filter_map(|(m_opt, mem)| m_opt.map(|m| (m, mem)))
                .collect(),
            None => vec![],
        }
    }
    fn instr_from_meta(&self, meta: ALMeta) -> ALStmt;
}

struct IntervalSemantics {
    instr_map: HashMap<ALMeta, ALStmt>,
}

impl IntervalSemantics {
    fn new(al_program: &ALStmt) -> Self {
        let all_ins = al_program.all_instrs();
        let mut instr_map = HashMap::new();
        for ins in all_ins {
            instr_map.insert(ins.meta().clone(), ins.clone());
        }
        IntervalSemantics { instr_map }
    }
}

impl Semantics for IntervalSemantics {
    type V = Interval;
    type M = IntervalMemory;

    fn instr_from_meta(&self, meta: ALMeta) -> ALStmt {
        self.instr_map.get(&meta).unwrap().clone()
    }
}
