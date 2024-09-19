use crate::analysis_representation::{ALMeta, ALStmt};
use crate::domain::{Elt, Interval, IntervalMemory, IntervalTable, Memory, Table, Worklist, CPO};
use crate::semantics::{IntervalSemantics, Semantics};
use num_bigint::BigInt;

enum Phase {
    Widen,
    Narrow,
}

pub struct IntervalAnalyzer {
    program: ALStmt,
    parameters: Vec<String>,
    pub table: IntervalTable,
    prime: BigInt,
}

impl IntervalAnalyzer {
    pub fn new(program: ALStmt, parameters: Vec<String>, prime: BigInt) -> Self {
        let mut table = IntervalTable::new(&program);
        table.add(program.meta(), IntervalMemory::from_params(&parameters));
        IntervalAnalyzer {
            program,
            parameters,
            table,
            prime,
        }
    }

    pub fn analyze(&mut self) {
        self.widen();
        self.narrow();
        self.check_all();
    }

    pub fn widen(&mut self) {
        self.run(Phase::Widen);
    }

    pub fn narrow(&mut self) {
        self.run(Phase::Narrow);
    }

    pub fn check_all(&self) {
        let all_instrs = self.program.all_instrs();
        for inst in all_instrs {
            let memory = self.table.find(&inst.meta());
            self.check_instr(inst, memory, self.prime.clone());
        }
    }

    fn run(&mut self, phase: Phase) {
        let semantics = IntervalSemantics::new(&self.program);
        let all_metas = self.program.all_metas();

        let mut worklist = Worklist::new(&self.program);

        while let Some(work) = worklist.pop() {
            // println!("work: {:?}", work);
            let old_memory = self.table.find(&work);
            let new_memory = all_metas
                .iter()
                .map(|meta| semantics.transfer_meta(Some(meta.clone()), self.table.find(&meta)))
                .flatten()
                .filter_map(
                    |(meta, memory)| {
                        if meta == work {
                            Some(memory)
                        } else {
                            None
                        }
                    },
                )
                .fold(IntervalMemory::bottom(), |acc, x| {
                    IntervalMemory::join(&acc, &x)
                });

            // println!("old_memory: {:?}", old_memory);
            // println!("new_memory: {:?}", new_memory);

            let final_memory = match phase {
                Phase::Widen => IntervalMemory::widen(&old_memory, &new_memory),
                Phase::Narrow => IntervalMemory::narrow(&old_memory, &new_memory),
            };
            let successors = semantics
                .transfer_meta(Some(work.clone()), final_memory.clone())
                .iter()
                .map(|(m, mem)| m.clone())
                .collect::<Vec<_>>();
            let converged = match phase {
                Phase::Widen => final_memory <= old_memory,
                Phase::Narrow => final_memory >= old_memory,
            };
            if !converged {
                self.table.add(work.clone(), final_memory);
                for succ in successors {
                    worklist.push(succ);
                }
            }
        }
    }

    fn check_instr(&self, instr: ALStmt, memory: IntervalMemory, prime: BigInt) {
        match instr {
            ALStmt::ALAssign { var, value, .. } => {
                let rhs_val = IntervalSemantics::eval(&value, &memory);
                self.print_var_range(&var, rhs_val);
            }
            _ => {}
        }
    }

    fn print_var_range(&self, var: &str, range: Interval) {
        use self::Interval::*;
        let prime_bound = Interval(
            Elt::of_bigint(-self.prime.clone() / 2),
            Elt::of_bigint(self.prime.clone() / 2),
        );
        let warning_message = if range <= prime_bound {
            String::new()
        } else {
            format!("!Warning! Not Intsafe")
        };
        println!(
            "Assignment instance of {:<8}= {:<20} {}",
            var,
            format!("{:?}", range),
            warning_message
        );
    }
}
