use std::collections::HashSet;

fn priorities(s: &str) -> HashSet<i32> {
    s.chars()
        .map(|c| match c {
            c if c.is_ascii_lowercase() => (c as i32) - ('a' as i32) + 1,
            c => (c as i32) - ('A' as i32) + 27,
        })
        .collect()
}

pub fn part1(input: &str) -> i32 {
    input
        .lines()
        .flat_map(|line| {
            let (a, b) = line.split_at(line.len() / 2);
            let sa = priorities(a);
            let sb: HashSet<i32> = priorities(b);
            sa.intersection(&sb).cloned().collect::<HashSet<_>>()
        })
        .sum()
}

pub fn part2(input: &str) -> i32 {
    input
        .lines()
        .collect::<Vec<_>>()
        .chunks(3)
        .map(|lines| {
            let z = lines
                .iter()
                .map(|line| priorities(line))
                .reduce(|a, b| a.intersection(&b).cloned().collect())
                .and_then(|set| set.into_iter().next())
                .unwrap();
            z
        })
        .sum()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

    #[test]
    fn part1() {
        assert_eq!(157, super::part1(SAMPLE));
        assert_eq!(7903, super::part1(utils::read_input("2022/day3").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(70, super::part2(SAMPLE));
        assert_eq!(2548, super::part2(utils::read_input("2022/day3").as_str()));
    }
}
