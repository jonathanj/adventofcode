#[derive(Debug, Clone)]
pub enum Op {
    Noop,
    AddX(i32),
}

pub fn part1(input: &str) -> i32 {
    parse(input)
        .iter()
        .scan(1, |x_register, op| match op {
            Op::Noop => Some(*x_register),
            Op::AddX(count) => {
                // Account for the start/end of a cycle, mutating "after" the result is returned.
                let result = *x_register;
                *x_register += count;
                Some(result)
            }
        })
        .zip(1..)
        .skip(19)
        .step_by(40)
        .map(|(x, cycle)| cycle * x)
        .sum()
}

pub fn part2(input: &str) -> String {
    parse(input)
        .iter()
        .enumerate()
        .take(240)
        .scan(1, |x, (cycle, op)| {
            let scan = (cycle as i32) % 40;
            let pixel_on = *x >= scan - 1 && *x <= scan + 1;
            match op {
                Op::AddX(count) => *x += count,
                _ => (),
            }
            if pixel_on {
                Some('#')
            } else {
                Some('.')
            }
        })
        .collect::<Vec<_>>()
        .chunks(40)
        .map(|chunk| String::from_iter(chunk))
        .collect::<Vec<_>>()
        .join("\n")
}

fn parse(input: &str) -> Vec<Op> {
    input
        .lines()
        .flat_map(|line| match line.split_once(' ') {
            None => vec![Op::Noop],
            Some(("addx", arg)) => vec![Op::Noop, Op::AddX(arg.parse::<i32>().unwrap())],
            _ => panic!("Unknown op {}", line),
        })
        .collect::<Vec<_>>()
}

mod tests {
    use crate::utils;

    #[test]
    fn part1() {
        assert_eq!(
            13140,
            super::part1(utils::read_input("2022/day10_sample").as_str())
        );
        assert_eq!(
            11960,
            super::part1(utils::read_input("2022/day10").as_str())
        );
    }

    fn pretty_pixels(output: String) -> String {
        println!("{}\n", output.replace(".", "·").replace("#", "█"));
        output
    }

    #[test]
    fn part2() {
        assert_eq!(
            "\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....",
            pretty_pixels(super::part2(
                utils::read_input("2022/day10_sample").as_str()
            ))
        );
        assert_eq!(
            "\
####...##..##..####.###...##..#....#..#.
#.......#.#..#.#....#..#.#..#.#....#..#.
###.....#.#....###..#..#.#....#....####.
#.......#.#....#....###..#.##.#....#..#.
#....#..#.#..#.#....#....#..#.#....#..#.
####..##...##..#....#.....###.####.#..#.",
            pretty_pixels(super::part2(utils::read_input("2022/day10").as_str()))
        );
    }
}
