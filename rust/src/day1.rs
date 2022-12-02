use crate::utils;

fn calorie_totals(input: &str) -> Vec<i32> {
    utils::line_groups(input)
        .map(|calories| calories.fold(0, |acc, s| acc + s.parse::<i32>().unwrap()))
        .collect()
}

fn part1(input: &str) -> i32 {
    *calorie_totals(input).iter().max().unwrap()
}

fn part2(input: &str) -> i32 {
    let mut xs: Vec<i32> = calorie_totals(input);
    xs.sort();
    return xs.iter().rev().take(3).fold(0, |a, b| a + b);
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn part1() {
        assert_eq!(24000, super::part1(SAMPLE));
        assert_eq!(69883, super::part1(utils::read_input("2022/day1").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(45000, super::part2(SAMPLE));
        assert_eq!(207576, super::part2(utils::read_input("2022/day1").as_str()));
    }
}
