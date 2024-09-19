use crate::analysis_representation::{ALCmpCode, ALMeta, ALStmt};
use num_bigint::{BigInt, ToBigUint};
use std::cmp::{max, min, Ord, PartialOrd};
use std::collections::{HashMap, HashSet};
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Neg, Shl, Shr, Sub};

pub trait CPO: PartialOrd + Sized {
    fn bottom() -> Self;
    fn join(lhs: &Self, rhs: &Self) -> Self;
    fn widen(lhs: &Self, rhs: &Self) -> Self;
    fn narrow(lhs: &Self, rhs: &Self) -> Self;
}

pub trait NumericalDomain:
    CPO
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self, Output = Self>
    + Neg<Output = Self>
    + BitAnd<Self, Output = Self>
    + BitOr<Self, Output = Self>
    + BitXor<Self, Output = Self>
    + Shl<Self, Output = Self>
    + Shr<Self, Output = Self>
{
    fn top() -> Self;
    fn from_int(i: i32) -> Self;
    fn from_bigint(i: BigInt) -> Self;
    fn cmp(pred: &ALCmpCode, lhs: &Self, rhs: &Self) -> Self;
    fn filter(pred: ALCmpCode, lhs: &Self, rhs: &Self) -> Self;
    fn _add(lhs: &Self, rhs: &Self) -> Self;
    fn _sub(lhs: &Self, rhs: &Self) -> Self;
    fn _mul(lhs: &Self, rhs: &Self) -> Self;
    fn _neg(this: &Self) -> Self;
    fn logic_and(lhs: &Self, rhs: &Self) -> Self;
    fn logic_or(lhs: &Self, rhs: &Self) -> Self;
    fn logic_not(this: &Self) -> Self;
}

pub trait Memory: CPO + Clone {
    type K;
    type V: NumericalDomain;
    fn from_params(params: &Vec<String>) -> Self;
    fn find(&self, key: &Self::K) -> Self::V;
    fn add(&mut self, key: Self::K, value: Self::V);
}

pub trait Table {
    type L;
    type M: Memory;
    fn find(&self, label: &Self::L) -> Self::M;
    fn add(&mut self, label: Self::L, memory: Self::M);
}

#[derive(Clone, PartialEq, Eq)]
pub enum Elt {
    Int(BigInt),
    PInf,
    MInf,
}

impl Elt {
    pub fn of_bigint(i: BigInt) -> Elt {
        Elt::Int(i)
    }

    fn of_int(i: i32) -> Elt {
        Elt::Int(BigInt::from(i))
    }
}

impl std::fmt::Debug for Elt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Elt::Int(i) => write!(f, "{}", i),
            Elt::PInf => write!(f, "+∞"),
            Elt::MInf => write!(f, "-∞"),
        }
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

impl Neg for Elt {
    type Output = Elt;
    fn neg(self) -> Self::Output {
        match self {
            Elt::Int(i) => Elt::Int(-i),
            Elt::PInf => Elt::MInf,
            Elt::MInf => Elt::PInf,
        }
    }
}

impl Shl for Elt {
    type Output = Elt;
    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => match i2.to_string().parse::<usize>() {
                Ok(i2) => Elt::Int(i1 << i2),
                Err(_) => Elt::PInf,
            },
            (Elt::PInf, _) => Elt::PInf,
            (Elt::MInf, _) => Elt::MInf,
            (_, Elt::PInf) => Elt::PInf,
            (_, Elt::MInf) => Elt::of_int(0),
        }
    }
}

impl Shr for Elt {
    type Output = Elt;
    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => match i2.to_string().parse::<usize>() {
                Ok(i2) => Elt::Int(i1 >> i2),
                Err(_) => Elt::PInf,
            },
            (Elt::PInf, _) => Elt::PInf,
            (Elt::MInf, _) => Elt::MInf,
            (_, Elt::PInf) => Elt::of_int(0),
            (_, Elt::MInf) => Elt::PInf,
        }
    }
}

impl BitOr for Elt {
    type Output = Elt;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => Elt::Int(i1 | i2),
            (Elt::PInf, _) | (_, Elt::PInf) => Elt::PInf,
            (Elt::MInf, _) | (_, Elt::MInf) => Elt::of_int(0),
        }
    }
}

impl BitXor for Elt {
    type Output = Elt;
    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Elt::Int(i1), Elt::Int(i2)) => Elt::Int(i1 ^ i2),
            (Elt::PInf, _) | (_, Elt::PInf) => Elt::PInf,
            (Elt::MInf, _) | (_, Elt::MInf) => Elt::MInf,
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

#[derive(Clone, PartialEq, Eq)]
pub enum Interval {
    Bot,
    Interval(Elt, Elt),
}

impl Interval {
    fn from_ints(l: i32, r: i32) -> Interval {
        Interval::Interval(Elt::of_int(l), Elt::of_int(r))
    }
    fn from_elts(l: Elt, r: Elt) -> Interval {
        if l <= r {
            Interval::Interval(l, r)
        } else {
            Interval::Bot
        }
    }
    fn zero_zero() -> Interval {
        Interval::from_ints(0, 0)
    }
    fn one_one() -> Interval {
        Interval::from_ints(1, 1)
    }
    fn zero_one() -> Interval {
        Interval::from_ints(0, 1)
    }
    fn _shl(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() << r1.clone(), l2.clone() << r2.clone())
            }
        }
    }
    fn _shr(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() >> r2.clone(), l2.clone() >> r1.clone())
            }
        }
    }
    fn _bit_and(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(_, l2), Interval::Interval(_, r2)) => {
                Interval::Interval(Elt::of_int(0), min(l2.clone(), r2.clone()))
            }
        }
    }
    fn _bit_or(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(max(l1.clone(), r1.clone()), l2.clone() | r2.clone())
            }
        }
    }
    fn _bit_xor(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => Interval::Interval(
                Elt::of_int(0),
                max(l1.clone() ^ r2.clone(), l2.clone() ^ r1.clone()),
            ),
        }
    }
}

impl std::fmt::Debug for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Interval::Bot => write!(f, "⊥"),
            Interval::Interval(l, r) => write!(f, "[{:?}, {:?}]", l, r),
        }
    }
}

impl Add for Interval {
    type Output = Interval;
    fn add(self, rhs: Self) -> Self::Output {
        Interval::_add(&self, &rhs)
    }
}
impl Add for &Interval {
    type Output = Interval;
    fn add(self, rhs: Self) -> Self::Output {
        Interval::_add(self, rhs)
    }
}

impl Sub for Interval {
    type Output = Interval;
    fn sub(self, rhs: Self) -> Self::Output {
        Interval::_sub(&self, &rhs)
    }
}
impl Sub for &Interval {
    type Output = Interval;
    fn sub(self, rhs: Self) -> Self::Output {
        Interval::_sub(self, rhs)
    }
}

impl Mul for Interval {
    type Output = Interval;
    fn mul(self, rhs: Self) -> Self::Output {
        Interval::_mul(&self, &rhs)
    }
}
impl Mul for &Interval {
    type Output = Interval;
    fn mul(self, rhs: Self) -> Self::Output {
        Interval::_mul(self, rhs)
    }
}

impl Neg for Interval {
    type Output = Interval;
    fn neg(self) -> Self::Output {
        Interval::_neg(&self)
    }
}

impl Shl for Interval {
    type Output = Interval;
    fn shl(self, rhs: Self) -> Self::Output {
        Interval::_shl(&self, &rhs)
    }
}

impl Shr for Interval {
    type Output = Interval;
    fn shr(self, rhs: Self) -> Self::Output {
        Interval::_shr(&self, &rhs)
    }
}

impl BitAnd for Interval {
    type Output = Interval;
    fn bitand(self, rhs: Self) -> Self::Output {
        Interval::_bit_and(&self, &rhs)
    }
}

impl BitOr for Interval {
    type Output = Interval;
    fn bitor(self, rhs: Self) -> Self::Output {
        Interval::_bit_or(&self, &rhs)
    }
}

impl BitXor for Interval {
    type Output = Interval;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Interval::_bit_xor(&self, &rhs)
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
    fn bottom() -> Self {
        Interval::Bot
    }

    fn join(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) => rhs.clone(),
            (_, Interval::Bot) => lhs.clone(),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(min(l1, r1).clone(), max(l2, r2).clone())
            }
        }
    }

    fn widen(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) => rhs.clone(),
            (_, Interval::Bot) => lhs.clone(),
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => Interval::Interval(
                if l1 <= r1 { l1.clone() } else { Elt::MInf },
                if l2 >= r2 { l2.clone() } else { Elt::PInf },
            ),
        }
    }

    fn narrow(lhs: &Self, rhs: &Self) -> Self {
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

/** For concise widening */
impl NumericalDomain for Interval {
    fn top() -> Self {
        Interval::Interval(Elt::MInf, Elt::PInf)
    }

    fn cmp(pred: &ALCmpCode, lhs: &Self, rhs: &Self) -> Self {
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

    fn filter(pred: ALCmpCode, lhs: &Self, rhs: &Self) -> Self {
        match (pred, lhs, rhs) {
            (_, Interval::Bot, _) | (_, _, Interval::Bot) => Interval::Bot,
            (ALCmpCode::ALEq, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone());
                let right = min(l2.clone(), r2.clone());
                Interval::from_elts(left, right)
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
                Interval::from_elts(left, right)
            }
            (ALCmpCode::ALGe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone());
                let right = l2.clone();
                Interval::from_elts(left, right)
            }
            (ALCmpCode::ALGt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = max(l1.clone(), r1.clone() + Elt::of_int(1));
                let right = l2.clone();
                Interval::from_elts(left, right)
            }
            (ALCmpCode::ALLe, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = l1.clone();
                let right = min(l2.clone(), r2.clone());
                Interval::from_elts(left, right)
            }
            (ALCmpCode::ALLt, Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                let left = l1.clone();
                let right = min(l2.clone(), r2.clone() - Elt::of_int(1));
                Interval::from_elts(left, right)
            }
        }
    }
    fn from_int(i: i32) -> Self {
        Interval::from_ints(i, i)
    }
    fn from_bigint(i: BigInt) -> Self {
        Interval::Interval(Elt::of_bigint(i.clone()), Elt::of_bigint(i.clone()))
    }
    fn _add(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() + r1.clone(), l2.clone() + r2.clone())
            }
        }
    }
    fn _sub(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(l1.clone() - r2.clone(), l2.clone() - r1.clone())
            }
        }
    }
    fn _mul(lhs: &Self, rhs: &Self) -> Self {
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
    fn _neg(this: &Self) -> Self {
        match this {
            Interval::Bot => Interval::Bot,
            Interval::Interval(l1, l2) => Interval::Interval(-l2.clone(), -l1.clone()),
        }
    }
    fn logic_and(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(max(l1.clone(), r1.clone()), min(l2.clone(), r2.clone()))
            }
        }
    }
    fn logic_or(lhs: &Self, rhs: &Self) -> Self {
        match (lhs, rhs) {
            (Interval::Bot, _) | (_, Interval::Bot) => Interval::Bot,
            (Interval::Interval(l1, l2), Interval::Interval(r1, r2)) => {
                Interval::Interval(min(l1.clone(), r1.clone()), max(l2.clone(), r2.clone()))
            }
        }
    }
    fn logic_not(this: &Self) -> Self {
        match this {
            Interval::Bot => Interval::Bot,
            Interval::Interval(l1, l2) => Interval::Interval(l2.clone(), l1.clone()),
        }
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
    fn bottom() -> Self {
        IntervalMemory {
            memory: HashMap::new(),
        }
    }
    fn join(lhs: &Self, rhs: &Self) -> Self {
        let mut new_memory = lhs.memory.clone();
        for (key, value) in &rhs.memory {
            if let Some(lhs_value) = lhs.memory.get(key) {
                new_memory.insert(key.clone(), Interval::join(lhs_value, value));
            } else {
                new_memory.insert(key.clone(), value.clone());
            }
        }
        IntervalMemory::from_hashmap(new_memory)
    }
    fn widen(lhs: &Self, rhs: &Self) -> Self {
        IntervalMemory::merge(lhs, rhs, Interval::widen)
    }
    fn narrow(lhs: &Self, rhs: &Self) -> Self {
        IntervalMemory::merge(lhs, rhs, Interval::narrow)
    }
}

impl Memory for IntervalMemory {
    type K = String;
    type V = Interval;
    fn find(&self, key: &Self::K) -> Self::V {
        self.memory.get(key).unwrap_or(&Interval::Bot).clone()
    }
    fn add(&mut self, key: Self::K, value: Self::V) {
        self.memory.insert(key, value);
    }
    fn from_params(params: &Vec<String>) -> Self {
        let mut memory = HashMap::new();
        for param in params {
            memory.insert(param.clone(), Interval::Interval(Elt::MInf, Elt::PInf));
        }
        IntervalMemory::from_hashmap(memory)
    }
}

#[derive(Debug)]
pub struct IntervalTable {
    table: HashMap<ALMeta, IntervalMemory>,
}

impl IntervalTable {
    pub fn new(al_program: &ALStmt) -> IntervalTable {
        let mut table = HashMap::new();
        for m in al_program.all_metas() {
            table.insert(m.clone(), IntervalMemory::bottom());
        }
        IntervalTable { table }
    }
}

impl Table for IntervalTable {
    type L = ALMeta;
    type M = IntervalMemory;
    fn find(&self, label: &Self::L) -> Self::M {
        self.table
            .get(label)
            .unwrap_or(&IntervalMemory::bottom())
            .clone()
    }
    fn add(&mut self, label: Self::L, memory: Self::M) {
        self.table.insert(label, memory);
    }
}

pub struct Worklist {
    list: HashSet<ALMeta>,
}

impl Worklist {
    pub fn new(al_program: &ALStmt) -> Worklist {
        let mut list = HashSet::new();
        for m in al_program.all_metas() {
            list.insert(m);
        }
        Worklist { list }
    }

    pub fn pop(&mut self) -> Option<ALMeta> {
        let mut next = None;
        for m in &self.list {
            next = Some(m.clone());
            break;
        }
        next.map(|m| {
            self.list.remove(&m);
            m
        })
    }

    pub fn push(&mut self, meta: ALMeta) {
        self.list.insert(meta);
    }
}
