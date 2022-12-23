use itertools::Itertools;
use std::cell::RefCell;
use std::collections::HashMap;

pub fn part1(input: &str) -> i64 {
    let (mut known, mut exprs, root_expr) = parse(input);
    resolve_unknowns(&mut known, &mut exprs);
    let (a, op, b) = root_expr;
    apply_op(known[a], op, known[b]) as i64
}

// The other method of solving this is to build an AST of the operations. Sorting it topologically
// will produce the order the dependencies need to be resolved in, inverting the tree and the
// operations will solve for the unknown variable.
pub fn part2(input: &str) -> i64 {
    let (original_known, original_exprs, root_expr) = parse(input);
    let x0 = part1(input) as f64;
    let last_fx = RefCell::new(x0);
    let last_x = RefCell::new(0.0);
    newton(
        |n| {
            let mut known = original_known.clone();
            let mut exprs = original_exprs.clone();
            known.insert("humn", n);
            resolve_unknowns(&mut known, &mut exprs);
            known[root_expr.0] - known[root_expr.2]
        },
        |x, fx| (fx - last_fx.replace(fx)) / (x - last_x.replace(x)),
        x0,
    ) as i64
}

fn resolve_unknowns<'a>(
    known: &mut HashMap<&'a str, f64>,
    exprs: &mut HashMap<&'a str, (&'a str, Op, &'a str)>,
) {
    while !&exprs.is_empty() {
        for (name, (a, op, b)) in exprs.clone() {
            if known.contains_key(a) && known.contains_key(b) {
                known.insert(name, apply_op(known[a], op, known[b]));
                exprs.remove(name);
            }
        }
    }
}

fn newton(f: impl Fn(f64) -> f64, df: impl Fn(f64, f64) -> f64, x0: f64) -> f64 {
    let mut x0 = x0;
    let tolerance = 1e-7;
    let max_iterations = 100;

    for _ in 0..max_iterations {
        let fx = f(x0);
        let dfx = df(x0, fx);
        let x1 = x0 - (fx / dfx);
        if (x1 - x0).abs() <= tolerance {
            return x1.round();
        }
        x0 = x1;
    }

    panic!("No solution!");
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

fn apply_op(a: f64, op: Op, b: f64) -> f64 {
    match op {
        Op::Add => a + b,
        Op::Sub => a - b,
        Op::Mul => a * b,
        Op::Div => a / b,
    }
}

fn parse(
    input: &str,
) -> (
    HashMap<&str, f64>,
    HashMap<&str, (&str, Op, &str)>,
    (&str, Op, &str),
) {
    let mut known = HashMap::new();
    let mut exprs = HashMap::new();
    input.lines().for_each(|line| {
        let (name, rest) = line.split_once(": ").unwrap();
        match rest.split(" ").collect_vec()[..] {
            [n] => {
                known.insert(name, n.parse::<f64>().unwrap());
            }
            [a, op_str, b] => {
                let op = match op_str {
                    "+" => Op::Add,
                    "-" => Op::Sub,
                    "*" => Op::Mul,
                    "/" => Op::Div,
                    _ => panic!("Unknown operation: {}", op_str),
                };
                exprs.insert(name, (a, op, b));
            }
            _ => panic!("Unknown expression: {}", rest),
        }
    });
    let root_expr = exprs.remove("root").unwrap();
    (known, exprs, root_expr)
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

    #[test]
    fn part1() {
        assert_eq!(152, super::part1(SAMPLE));
        assert_eq!(
            62386792426088,
            super::part1(utils::read_input("2022/day21").as_str())
        );
    }

    #[test]
    fn part2() {
        assert_eq!(301, super::part2(SAMPLE));
        assert_eq!(
            3876027196185,
            super::part2(utils::read_input("2022/day21").as_str())
        );
    }
}
