use crate::utils::regex_captures;
use regex::Regex;
use std::ops::Range;

fn parse(input: &str) -> impl Iterator<Item = (Range<i32>, Range<i32>)> + '_ {
    let expr = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)").unwrap();
    input
        .lines()
        .map(move |line| match &regex_captures(&expr, line)[0][..] {
            [a1, a2, b1, b2] => (
                (a1.parse::<i32>().unwrap()..a2.parse::<i32>().unwrap()),
                (b1.parse::<i32>().unwrap()..b2.parse::<i32>().unwrap()),
            ),
            _ => panic!("Oops"),
        })
}

fn range_contains<T: std::cmp::PartialOrd>(a: &Range<T>, b: &Range<T>) -> bool {
    a.start <= b.start && a.end >= b.end
}

pub fn part1(input: &str) -> i32 {
    parse(input)
        .filter(|(a, b)| range_contains(a, b) || range_contains(b, a))
        .count() as i32
}

fn range_overlaps<T: std::cmp::PartialOrd>(a: &Range<T>, b: &Range<T>) -> bool {
    (a.end >= b.start && a.end <= b.end) || (a.start >= b.start && a.end <= b.end)
}

pub fn part2(input: &str) -> i32 {
    parse(input)
        .filter(|(a, b)| range_overlaps(a, b) || range_overlaps(b, a))
        .count() as i32
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

    #[test]
    fn part1() {
        assert_eq!(2, super::part1(SAMPLE));
        assert_eq!(475, super::part1(utils::read_input("2022/day4").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(4, super::part2(SAMPLE));
        assert_eq!(825, super::part2(utils::read_input("2022/day4").as_str()));
    }
}
