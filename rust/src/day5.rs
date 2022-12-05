use regex::Regex;

type Stacks = Vec<Vec<char>>;

fn parse_stacks(input: &str) -> Stacks {
    let mut lines = input.lines().collect::<Vec<_>>();
    let num_stacks = lines.remove(lines.len() - 1).len() / 4 + 1;
    let mut stacks = vec![vec![]; num_stacks];

    for line in lines {
        for (i, ch) in line.chars().skip(1).step_by(4).enumerate() {
            if ch.is_alphabetic() {
                stacks[i].push(ch);
            }
        }
    }
    return stacks;
}

fn process(input: &str, move_idx: fn(usize) -> usize) -> String {
    let (stacks_str, instructions_str) = input.split_once("\n\n").unwrap();
    let mut stacks = parse_stacks(stacks_str);

    let expr = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    for line in instructions_str.lines() {
        let caps = expr.captures(line).unwrap();
        let count = caps[1].parse::<usize>().unwrap();
        let src = caps[2].parse::<usize>().unwrap() - 1;
        let dst = caps[3].parse::<usize>().unwrap() - 1;
        for idx in 0..count {
            let ch = stacks[src].remove(0);
            stacks[dst].insert(move_idx(idx), ch);
        }
    }

    stacks
        .iter()
        .flat_map(|stack| stack.first())
        .collect::<String>()
}

fn part1(input: &str) -> String {
    process(input, |_| 0)
}

fn part2(input: &str) -> String {
    process(input, |idx| idx)
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = r"
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

    #[test]
    fn part1() {
        assert_eq!("CMZ", super::part1(&SAMPLE[1..]));
        assert_eq!(
            "TLFGBZHCN",
            super::part1(utils::read_input("2022/day5").as_str())
        );
    }

    #[test]
    fn part2() {
        assert_eq!("MCD", super::part2(&SAMPLE[1..]));
        assert_eq!(
            "QRQFHFWCL",
            super::part2(utils::read_input("2022/day5").as_str())
        );
    }
}
