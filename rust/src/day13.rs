use serde_json::{json, Value};
use std::cmp::Ordering;

fn compare(l: &Vec<Value>, r: &Vec<Value>) -> Ordering {
    let len = l.len().max(r.len());
    for i in 0..len {
        match (l.get(i), r.get(i)) {
            (Some(Value::Number(a)), Some(Value::Number(b))) => {
                let a = a.as_i64().unwrap();
                let b = b.as_i64().unwrap();
                if a < b {
                    return Ordering::Less;
                } else if a > b {
                    return Ordering::Greater;
                }
                continue;
            }
            (Some(Value::Array(a)), Some(Value::Array(b))) => match compare(a, b) {
                Ordering::Equal => continue,
                result => return result,
            },
            (Some(Value::Array(a)), Some(b @ Value::Number(_))) => {
                match compare(a, &vec![b.clone()]) {
                    Ordering::Equal => continue,
                    result => return result,
                }
            }
            (Some(a @ Value::Number(_)), Some(Value::Array(b))) => {
                match compare(&vec![a.clone()], b) {
                    Ordering::Equal => continue,
                    result => return result,
                }
            }
            (None, Some(_)) => return Ordering::Less,
            (Some(_), None) => return Ordering::Greater,
            (a, b) => panic!("Unhandled values {:?} / {:?}", a, b),
        };
    }
    Ordering::Equal
}

pub fn part1(input: &str) -> i32 {
    (1..)
        .zip(parse(input).iter())
        .filter_map(|(idx, (l, r))| match compare(l, r) {
            Ordering::Less => Some(idx),
            Ordering::Greater => None,
            Ordering::Equal => panic!("Impossible"),
        })
        .sum()
}

pub fn part2(input: &str) -> usize {
    let div1 = vec![json![[2]]];
    let div2 = vec![json![[6]]];
    let mut signals: Vec<_> = parse(input)
        .iter()
        .flat_map(|(l, r)| [l.to_vec(), r.to_vec()])
        .chain([div1.clone(), div2.clone()])
        .collect();
    signals.sort_by(|a, b| compare(a, b));
    let x = 1 + signals.iter().position(|v| *v == div1).unwrap();
    let y = 1 + signals.iter().position(|v| *v == div2).unwrap();
    return x * y;
}

fn parse(input: &str) -> Vec<(Vec<Value>, Vec<Value>)> {
    input
        .split("\n\n")
        .map(|pair| {
            let (p1, p2) = pair.split_once('\n').unwrap();
            let a: Value = serde_json::from_str(p1).unwrap();
            let b: Value = serde_json::from_str(p2).unwrap();
            (a.as_array().unwrap().clone(), b.as_array().unwrap().clone())
        })
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    #[test]
    fn part1() {
        assert_eq!(13, super::part1(SAMPLE));
        assert_eq!(6070, super::part1(utils::read_input("2022/day13").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(140, super::part2(SAMPLE));
        assert_eq!(
            20758,
            super::part2(utils::read_input("2022/day13").as_str())
        );
    }
}
