use crate::utils::regex_capture;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashSet, VecDeque};
use std::num::ParseIntError;
use std::str::FromStr;

type State = Vec<VecDeque<i64>>;
type Rules = Vec<Rule>;

fn apply_op(value: i64, op: &Op) -> i64 {
    let operand_value = |operand: &Operand| match operand {
        Operand::Constant(n) => *n,
        Operand::Old => value,
    };
    match op {
        Op::Mul(operand) => value * operand_value(operand),
        Op::Div(operand) => value / operand_value(operand),
        Op::Add(operand) => value + operand_value(operand),
        Op::Sub(operand) => value - operand_value(operand),
        Op::Mod(operand) => value % operand_value(operand),
    }
}

fn round(
    rules: &Rules,
    mut state: State,
    mut counts: Vec<i64>,
    normalize_op: &Op,
) -> (State, Vec<i64>) {
    for (rule_idx, rule) in rules.iter().enumerate() {
        loop {
            match state[rule_idx].pop_front() {
                None => break,
                Some(worry_level) => {
                    counts[rule_idx] += 1;
                    let worry_level = apply_op(worry_level, &rule.op);
                    let worry_level = apply_op(worry_level, &normalize_op);
                    if worry_level % rule.test == 0 {
                        state[rule.when_true].push_back(worry_level);
                    } else {
                        state[rule.when_false].push_back(worry_level);
                    }
                }
            }
        }
    }
    (state, counts)
}

fn monkey_business_level(
    mut state: State,
    rules: &Rules,
    num_rounds: i32,
    normalize_op: &Op,
) -> i64 {
    let mut counts = vec![0i64; rules.len()];
    for _ in 0..num_rounds {
        (state, counts) = round(&rules, state, counts, &normalize_op);
    }
    counts.sort();
    counts.iter().rev().take(2).product()
}

pub fn part1(input: &str) -> i64 {
    let (state, rules) = parse(input);
    monkey_business_level(state, &rules, 20, &Op::Div(Operand::Constant(3)))
}

pub fn part2(input: &str) -> i64 {
    let (state, rules) = parse(input);
    let supermodulo: i64 = rules
        .iter()
        .map(|rule| rule.test)
        .collect::<HashSet<_>>()
        .iter()
        .product();
    monkey_business_level(
        state,
        &rules,
        10000,
        &Op::Mod(Operand::Constant(supermodulo)),
    )
}

#[derive(Debug)]
pub enum Day11Error {
    ParseOperandError,
    ParseNumberError(ParseIntError),
}

#[derive(Debug, Clone)]
pub enum Op {
    Mul(Operand),
    Div(Operand),
    Add(Operand),
    Sub(Operand),
    Mod(Operand),
}

impl FromStr for Op {
    type Err = Day11Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match scan_fmt!(s, "{} {}", char, Operand) {
            Ok(('*', operand)) => Ok(Op::Mul(operand)),
            Ok(('/', operand)) => Ok(Op::Div(operand)),
            Ok(('+', operand)) => Ok(Op::Add(operand)),
            Ok(('-', operand)) => Ok(Op::Sub(operand)),
            _ => Err(Day11Error::ParseOperandError),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Old,
    Constant(i64),
}

impl FromStr for Operand {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "old" => Ok(Operand::Old),
            n => Ok(Operand::Constant(n.parse()?)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rule {
    op: Op,
    test: i64,
    when_true: usize,
    when_false: usize,
}

fn parse_monkey(input: &str) -> Result<(VecDeque<i64>, Rule), Day11Error> {
    lazy_static! {
        static ref OP_EXPR: Regex = Regex::new(r"Operation: new = old (.+)$").unwrap();
        static ref TEST_EXPR: Regex = Regex::new(r"Test: divisible by (\d+)$").unwrap();
        static ref TRUE_EXPR: Regex = Regex::new(r"If true: throw to monkey (\d+)$").unwrap();
        static ref FALSE_EXPR: Regex = Regex::new(r"If false: throw to monkey (\d+)$").unwrap();
    }
    let lines = input.lines().collect::<Vec<_>>();
    let starting_items: VecDeque<i64> = scan_fmt!(lines[1], "  Starting items: {/.+/}", String)
        .unwrap()
        .split(", ")
        .flat_map(|n| n.parse())
        .collect();

    let op: Op = regex_capture(&OP_EXPR, lines[2])?;
    let test: i64 = regex_capture(&TEST_EXPR, lines[3]).map_err(Day11Error::ParseNumberError)?;
    let when_true: usize =
        regex_capture(&TRUE_EXPR, lines[4]).map_err(Day11Error::ParseNumberError)?;
    let when_false: usize =
        regex_capture(&FALSE_EXPR, lines[5]).map_err(Day11Error::ParseNumberError)?;

    Ok((
        starting_items,
        Rule {
            op,
            test,
            when_true,
            when_false,
        },
    ))
}

fn parse(input: &str) -> (State, Rules) {
    input.split("\n\n").flat_map(parse_monkey).fold(
        (Vec::new(), Vec::new()),
        |(mut state, mut rules), (starting_items, rule)| {
            state.push(starting_items);
            rules.push(rule);
            (state, rules)
        },
    )
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    #[test]
    fn part1() {
        assert_eq!(10605, super::part1(SAMPLE));
        assert_eq!(
            110220,
            super::part1(utils::read_input("2022/day11").as_str())
        );
    }

    #[test]
    fn part2() {
        assert_eq!(2713310158, super::part2(SAMPLE));
        assert_eq!(
            19457438264,
            super::part2(utils::read_input("2022/day11").as_str())
        );
    }
}
