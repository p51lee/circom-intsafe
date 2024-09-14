use crate::analysis_representation::ALCmpCode;
use num_bigint::BigInt;
use std::cmp::{max, min, Ord, PartialOrd};
use std::collections::{HashMap, HashSet};
use std::ops::{Add, BitAndAssign, BitOr, BitOrAssign, Mul, Sub};

pub trait CPO: PartialOrd + Sized {
    type T;

    fn bottom() -> Self::T;
    fn join(lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn widen(lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn narrow(lhs: &Self::T, rhs: &Self::T) -> Self::T;
}

pub trait NumericalDomain: CPO {
    fn top() -> Self::T;
    fn of_int(i: i32) -> Self::T;
    fn cmp(pred: ALCmpCode, lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn filter(pred: ALCmpCode, lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn add(lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn sub(lhs: &Self::T, rhs: &Self::T) -> Self::T;
    fn mul(lhs: &Self::T, rhs: &Self::T) -> Self::T;
}

pub trait Memory: CPO {
    type K;
    type V: NumericalDomain;
    fn find(&self, key: &Self::K) -> Self::V;
}

pub trait Table {
    type L;
    type M: Memory;
    fn find(&self, label: &Self::L) -> Self::M;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elt {
    Int(BigInt),
    PInf,
    MInf,
}

impl Elt {
    fn of_bigint(i: BigInt) -> Elt {
        Elt::Int(i)
    }

    fn of_int(i: i32) -> Elt {
        Elt::Int(BigInt::from(i))
    }
}

impl Add for Elt {
    type Output = Elt;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => Elt::Int(i1 + i2),
            (Elt::PInf, Elt::Int(_)) | (Elt::Int(_), Elt::PInf) => Elt::PInf,
            (Elt::MInf, Elt::Int(_)) | (Elt::Int(_), Elt::MInf) => Elt::MInf,
            (Elt::PInf, Elt::PInf) => Elt::PInf,
            (Elt::MInf, Elt::MInf) => Elt::MInf,
            (Elt::PInf, Elt::MInf) | (Elt::MInf, Elt::PInf) => panic!("Cannot add +inf and -inf"),
        }
    }
}

impl Sub for Elt {
    type Output = Elt;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => Elt::Int(i1 - i2),
            (Elt::PInf, Elt::Int(_)) | (Elt::Int(_), Elt::MInf) => Elt::PInf,
            (Elt::MInf, Elt::Int(_)) | (Elt::Int(_), Elt::PInf) => Elt::MInf,
            (Elt::PInf, Elt::MInf) => Elt::PInf,
            (Elt::MInf, Elt::PInf) => Elt::MInf,
            (Elt::PInf, Elt::PInf) | (Elt::MInf, Elt::MInf) => {
                panic!("Cannot subtract +inf and -inf")
            }
        }
    }
}

impl Mul for Elt {
    type Output = Elt;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => Elt::Int(i1 * i2),
            (Elt::PInf, Elt::Int(i)) | (Elt::Int(i), Elt::PInf) => {
                if i > BigInt::from(0) {
                    Elt::PInf
                } else if i < BigInt::from(0) {
                    Elt::MInf
                } else {
                    Elt::Int(BigInt::from(0))
                }
            }
            (Elt::MInf, Elt::Int(i)) | (Elt::Int(i), Elt::MInf) => {
                if i > BigInt::from(0) {
                    Elt::MInf
                } else if i < BigInt::from(0) {
                    Elt::PInf
                } else {
                    Elt::Int(BigInt::from(0))
                }
            }
            (Elt::PInf, Elt::PInf) | (Elt::MInf, Elt::MInf) => Elt::PInf,
            (Elt::PInf, Elt::MInf) | (Elt::MInf, Elt::PInf) => Elt::MInf,
        }
    }
}

impl Ord for Elt {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Elt::Int(i1), Elt::Int(i2)) => i1.cmp(i2),
            (Elt::PInf, Elt::PInf) | (Elt::MInf, Elt::MInf) => std::cmp::Ordering::Equal,
            (Elt::PInf, _) => std::cmp::Ordering::Greater,
            (_, Elt::PInf) => std::cmp::Ordering::Less,
            (Elt::MInf, _) => std::cmp::Ordering::Less,
            (_, Elt::MInf) => std::cmp::Ordering::Greater,
        }
    }
}

impl PartialOrd for Elt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Elt::Int(i1), Elt::Int(i2)) => i1.partial_cmp(i2),
            (Elt::PInf, Elt::PInf) | (Elt::MInf, Elt::MInf) => Some(std::cmp::Ordering::Equal),
            (Elt::PInf, _) => Some(std::cmp::Ordering::Greater),
            (_, Elt::PInf) => Some(std::cmp::Ordering::Less),
            (Elt::MInf, _) => Some(std::cmp::Ordering::Less),
            (_, Elt::MInf) => Some(std::cmp::Ordering::Greater),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Interval {
    Bot,
    Interval(Elt, Elt),
}

impl Interval {
    fn of_bigint(i: BigInt) -> Interval {
        Interval::Interval(Elt::of_bigint(i.clone()), Elt::of_bigint(i.clone()))
    }
    fn of_ints(l: i32, r: i32) -> Interval {
        Interval::Interval(Elt::of_int(l), Elt::of_int(r))
    }
    fn of_elts(l: Elt, r: Elt) -> Interval {
        if l <= r {
            Interval::Interval(l, r)
        } else {
            Interval::Bot
        }
    }
    fn zero_zero() -> Interval {
        Interval::of_ints(0, 0)
    }
    fn one_one() -> Interval {
        Interval::of_ints(1, 1)
    }
    fn zero_one() -> Interval {
        Interval::of_ints(0, 1)
    }
}

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Interval::Bot, Interval::Bot) => Some(std::cmp::Ordering::Equal),
            (Interval::Bot, _) => Some(std::cmp::Ordering::Less),
            (_, Interval::Bot) => Some(std::cmp::Ordering::Greater),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if l1 == r1 && l2 == r2 {
                    Some(std::cmp::Ordering::Equal)
                } else if l1 <= r1 && l2 >= r2 {
                    Some(std::cmp::Ordering::Greater)
                } else if l1 >= r1 && l2 <= r2 {
                    Some(std::cmp::Ordering::Less)
                } else {
                    None
                }
            }
        }
    }
}

impl CPO for Interval {
    type T = Interval;

    fn bottom() -> Self::T {
        Interval::Bot
    }

    fn join(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) => rhs.clone(),
            (_, Interval::Bot) => lhs.clone(),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(min(l1, r1).clone(), max(l2, r2).clone())
            }
        }
    }

    fn widen(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) => rhs.clone(),
            (_, Interval::Bot) => lhs.clone(),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => Interval::Interval(
                if l1 <= r1 { l1.clone() } else { Elt::MInf },
                if l2 >= r2 { l2.clone() } else { Elt::PInf },
            ),
        }
    }

    fn narrow(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) => Interval::Bot,
            (_, Interval::Bot) => lhs.clone(),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => Interval::Interval(
                match l1 {
                    Elt::MInf => r1.clone(),
                    _ => l1.clone(),
                },
                match l2 {
                    Elt::PInf => r2.clone(),
                    _ => l2.clone(),
                },
            ),
        }
    }
}

/** For concise joining */
impl BitOr for &Interval {
    type Output = Interval;
    fn bitor(self, rhs: Self) -> Self::Output {
        Interval::join(self, rhs)
    }
}

/** For concise widening */
impl BitOrAssign for Interval {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = Interval::widen(self, &rhs);
    }
}

/** For concise narrowing */
impl BitAndAssign for Interval {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = Interval::narrow(self, &rhs);
    }
}

impl NumericalDomain for Interval {
    fn top() -> Self::T {
        Interval::Interval(Elt::MInf, Elt::PInf)
    }

    fn cmp(pred: ALCmpCode, lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (pred, lhs, rhs) {
            (_, Interval::Bot, _) | (_, _, Interval::Bot) => Interval::Bot,
            (ALCmpCode::ALEq, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if lhs == rhs {
                    Interval::one_one()
                } else if l2 < r1 || r2 < l1 {
                    Interval::zero_zero()
                } else {
                    Interval::zero_one()
                }
            }
            (ALCmpCode::ALNe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if lhs == rhs {
                    Interval::zero_zero()
                } else if l2 < r1 || r2 < l1 {
                    Interval::one_one()
                } else {
                    Interval::zero_one()
                }
            }
            (ALCmpCode::ALGe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if l1 >= r2 {
                    Interval::one_one()
                } else if l2 < r1 {
                    Interval::zero_zero()
                } else {
                    Interval::zero_one()
                }
            }
            (ALCmpCode::ALGt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if l1 > r2 {
                    Interval::one_one()
                } else if l2 <= r1 {
                    Interval::zero_zero()
                } else {
                    Interval::zero_one()
                }
            }
            (ALCmpCode::ALLe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if l2 <= r1 {
                    Interval::one_one()
                } else if l1 > r2 {
                    Interval::zero_zero()
                } else {
                    Interval::zero_one()
                }
            }
            (ALCmpCode::ALLt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                if l2 < r1 {
                    Interval::one_one()
                } else if l1 >= r2 {
                    Interval::zero_zero()
                } else {
                    Interval::zero_one()
                }
            }
        }
    }

    fn filter(pred: ALCmpCode, lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (pred, lhs, rhs) {
            (_, Interval::Bot, _) | (_, _, Interval::Bot) => Interval::Bot,
            (ALCmpCode::ALEq, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone());
                let right = min(l2.clone(), r2.clone());
                Interval::of_elts(left, right)
            }
            (ALCmpCode::ALNe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = if r1 == r2 && r2 == l1 {
                    l1.clone() + Elt::of_int(1)
                } else {
                    l1.clone()
                };
                let right = if r1 == r2 && r2 == l2 {
                    l2.clone() - Elt::of_int(1)
                } else {
                    l2.clone()
                };
                Interval::of_elts(left, right)
            }
            (ALCmpCode::ALGe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone());
                let right = l2.clone();
                Interval::of_elts(left, right)
            }
            (ALCmpCode::ALGt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone() + Elt::of_int(1));
                let right = l2.clone();
                Interval::of_elts(left, right)
            }
            (ALCmpCode::ALLe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = l1.clone();
                let right = min(l2.clone(), r2.clone());
                Interval::of_elts(left, right)
            }
            (ALCmpCode::ALLt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = l1.clone();
                let right = min(l2.clone(), r2.clone() - Elt::of_int(1));
                Interval::of_elts(left, right)
            }
        }
    }
    fn of_int(i: i32) -> Self::T {
        Interval::of_ints(i, i)
    }
    fn add(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() + r1.clone(), l2.clone() + r2.clone())
            }
        }
    }
    fn sub(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() - r2.clone(), l2.clone() - r1.clone())
            }
        }
    }
    fn mul(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let mut v = vec![
                    l1.clone() * r1.clone(),
                    l1.clone() * r2.clone(),
                    l2.clone() * r1.clone(),
                    l2.clone() * r2.clone(),
                ];
                v.sort();
                Interval::Interval(v[0].clone(), v[3].clone())
            }
        }
    }
}

impl Add for &Interval {
    type Output = Interval;
    fn add(self, rhs: Self) -> Self::Output {
        Interval::add(self, rhs)
    }
}

impl Sub for &Interval {
    type Output = Interval;
    fn sub(self, rhs: Self) -> Self::Output {
        Interval::sub(self, rhs)
    }
}

impl Mul for &Interval {
    type Output = Interval;
    fn mul(self, rhs: Self) -> Self::Output {
        Interval::mul(self, rhs)
    }
}

// TODO: Implement more effective abstraction for arrays: segment-based analysis

#[derive(Debug, Clone, PartialEq)]
pub struct IntervalMemory {
    memory: HashMap<String, Interval>,
}
impl IntervalMemory {
    fn from_hashmap(memory: HashMap<String, Interval>) -> IntervalMemory {
        IntervalMemory { memory }
    }
    fn union_keys(&self, other: &Self) -> HashSet<String> {
        let mut union_keys = HashSet::new();
        for key in self.memory.keys() {
            union_keys.insert(key.clone());
        }
        for key in other.memory.keys() {
            union_keys.insert(key.clone());
        }
        union_keys
    }
    fn merge(
        lhs: &IntervalMemory,
        rhs: &IntervalMemory,
        f: fn(&Interval, &Interval) -> Interval,
    ) -> IntervalMemory {
        let mut new_memory = HashMap::new();
        for key in IntervalMemory::union_keys(lhs, rhs) {
            let lhs_value = lhs.memory.get(&key).unwrap_or(&Interval::Bot);
            let rhs_value = rhs.memory.get(&key).unwrap_or(&Interval::Bot);
            new_memory.insert(key, f(lhs_value, rhs_value));
        }
        IntervalMemory::from_hashmap(new_memory)
    }
}

impl PartialOrd for IntervalMemory {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut result = std::cmp::Ordering::Equal;
        for key in IntervalMemory::union_keys(self, other) {
            let lhs_value = self.memory.get(&key).unwrap_or(&Interval::Bot);
            let rhs_value = other.memory.get(&key).unwrap_or(&Interval::Bot);
            match lhs_value.partial_cmp(rhs_value) {
                Some(std::cmp::Ordering::Equal) => {}
                Some(std::cmp::Ordering::Less) => {
                    if result == std::cmp::Ordering::Greater {
                        return None;
                    }
                    result = std::cmp::Ordering::Less;
                }
                Some(std::cmp::Ordering::Greater) => {
                    if result == std::cmp::Ordering::Less {
                        return None;
                    }
                    result = std::cmp::Ordering::Greater;
                }
                None => return None,
            }
        }
        Some(result)
    }
}

impl CPO for IntervalMemory {
    type T = IntervalMemory;
    fn bottom() -> Self::T {
        IntervalMemory {
            memory: HashMap::new(),
        }
    }
    fn join(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        let mut new_memory = lhs.memory.clone();
        for (key, value) in &rhs.memory {
            if let Some(lhs_value) = lhs.memory.get(key) {
                new_memory.insert(key.clone(), lhs_value | value);
            } else {
                new_memory.insert(key.clone(), value.clone());
            }
        }
        IntervalMemory::from_hashmap(new_memory)
    }
    fn widen(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        IntervalMemory::merge(lhs, rhs, Interval::widen)
    }
    fn narrow(lhs: &Self::T, rhs: &Self::T) -> Self::T {
        IntervalMemory::merge(lhs, rhs, Interval::narrow)
    }
}

impl Memory for IntervalMemory {
    type K = String;
    type V = Interval;
    fn find(&self, key: &Self::K) -> Self::V {
        self.memory.get(key).unwrap_or(&Interval::Bot).clone()
    }
}

pub struct IntervalTable {
    table: HashMap<String, IntervalMemory>,
}
