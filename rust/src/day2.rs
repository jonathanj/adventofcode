#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

#[derive(Copy, Clone, Debug)]
enum Outcome {
    Lose,
    Draw,
    Win,
}

impl Shape {
    fn parse_shape_letter(letter: &str) -> Shape {
        match letter {
            "A" | "X" => Shape::Rock,
            "B" | "Y" => Shape::Paper,
            "C" | "Z" => Shape::Scissors,
            _ => panic!("Bad shape letter"),
        }
    }

    pub fn parse<B>(input: &str, parse_right_letter: fn(&str) -> B) -> Vec<(Shape, B)> {
        input
            .lines()
            .map(|line| {
                if let Some((a, b)) = line.split_once(" ") {
                    (Self::parse_shape_letter(a), parse_right_letter(b))
                } else {
                    panic!("Unhappy line")
                }
            })
            .collect()
    }
}

fn play<T>(
    input: &str,
    parse: fn(&str) -> T,
    get_outcome: fn(Shape, T) -> (Shape, Outcome),
) -> i32 {
    Shape::parse(input, parse)
        .into_iter()
        .fold(0, |acc, (a, b)| {
            let (shape, outcome) = get_outcome(a, b);
            acc + match shape {
                Shape::Rock => 1,
                Shape::Paper => 2,
                Shape::Scissors => 3,
            } + match outcome {
                Outcome::Win => 6,
                Outcome::Draw => 3,
                Outcome::Lose => 0,
            }
        })
}

pub fn part1(input: &str) -> i32 {
    play(input, Shape::parse_shape_letter, |a, b| match (a, b) {
        (Shape::Rock, Shape::Paper)
        | (Shape::Paper, Shape::Scissors)
        | (Shape::Scissors, Shape::Rock) => (b, Outcome::Win),
        _ if a == b => (b, Outcome::Draw),
        _ => (b, Outcome::Lose),
    })
}

pub fn part2(input: &str) -> i32 {
    fn parse_outcome_letter(letter: &str) -> Outcome {
        match letter {
            "X" => Outcome::Lose,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => panic!("Bad shape letter"),
        }
    }

    play(input, parse_outcome_letter, |a, b| match (a, b) {
        (a, Outcome::Draw) => (a, b),
        (a, Outcome::Lose) => match a {
            Shape::Rock => (Shape::Scissors, b),
            Shape::Paper => (Shape::Rock, b),
            Shape::Scissors => (Shape::Paper, b),
        },
        (a, Outcome::Win) => match a {
            Shape::Rock => (Shape::Paper, b),
            Shape::Paper => (Shape::Scissors, b),
            Shape::Scissors => (Shape::Rock, b),
        },
    })
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
A Y
B X
C Z";

    #[test]
    fn part1() {
        assert_eq!(15, super::part1(SAMPLE));
        assert_eq!(10718, super::part1(utils::read_input("2022/day2").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(12, super::part2(SAMPLE));
        assert_eq!(14652, super::part2(utils::read_input("2022/day2").as_str()));
    }
}
