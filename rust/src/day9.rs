use std::collections::HashSet;
use std::iter;

#[derive(Clone)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}
type Coord = (i32, i32);

fn move_tail(head_pos: &Coord, tail_pos: &Coord) -> Coord {
    let (dx, dy) = (head_pos.0 - tail_pos.0, head_pos.1 - tail_pos.1);
    if dx.abs() > 1 || dy.abs() > 1 {
        (tail_pos.0 + dx.signum(), tail_pos.1 + dy.signum())
    } else {
        *tail_pos
    }
}

fn move_head(head_pos: &Coord, dir: &Direction) -> Coord {
    match dir {
        Direction::Up => (head_pos.0, head_pos.1 - 1),
        Direction::Down => (head_pos.0, head_pos.1 + 1),
        Direction::Left => (head_pos.0 - 1, head_pos.1),
        Direction::Right => (head_pos.0 + 1, head_pos.1),
    }
}

pub fn solve(initial_state: Vec<Coord>, directions: Vec<Direction>) -> usize {
    directions
        .iter()
        .scan(((0, 0), initial_state), |state, dir| {
            let (head_pos, tails) = state;
            let new_head_pos = move_head(head_pos, dir);
            let new_tails = tails
                .iter()
                .scan(new_head_pos, |last_coord, tail_pos| {
                    *last_coord = move_tail(last_coord, &tail_pos);
                    Some(last_coord.clone())
                })
                .collect::<Vec<_>>();
            let end_coord = new_tails.last().unwrap().clone();
            *state = (new_head_pos, new_tails);
            Some(end_coord)
        })
        .collect::<HashSet<_>>()
        .len()
}

pub fn part1(input: &str) -> usize {
    solve(vec![(0, 0); 1], parse(input))
}

pub fn part2(input: &str) -> usize {
    solve(vec![(0, 0); 9], parse(input))
}

fn parse(input: &str) -> Vec<Direction> {
    input
        .lines()
        .flat_map(|line| {
            let (dir_str, count_str) = line.split_once(' ').unwrap();
            let dir = match dir_str {
                "U" => Direction::Up,
                "D" => Direction::Down,
                "L" => Direction::Left,
                "R" => Direction::Right,
                _ => panic!("Unknown direction {}", dir_str),
            };
            iter::repeat(dir).take(count_str.parse::<usize>().unwrap())
        })
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE_1: &str = "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";
    const SAMPLE_2: &str = "\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

    #[test]
    fn part1() {
        assert_eq!(13, super::part1(SAMPLE_1));
        assert_eq!(5930, super::part1(utils::read_input("2022/day9").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(1, super::part2(SAMPLE_1));
        assert_eq!(36, super::part2(SAMPLE_2));
        assert_eq!(2443, super::part2(utils::read_input("2022/day9").as_str()));
    }
}
