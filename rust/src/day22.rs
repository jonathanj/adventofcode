use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;

type Coord = (i32, i32);
type Grid = HashMap<Coord, Tile>;

#[derive(Debug)]
enum Tile {
    Open,
    Wall,
}

#[derive(Debug, Clone)]
enum Instruction {
    Forward(u32),
    TurnLeft,
    TurnRight,
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North = 3,
    South = 1,
    East = 0,
    West = 2,
}

impl Direction {
    fn value(&self) -> i32 {
        *self as i32
    }

    fn apply_to_coord(&self, (x, y): Coord) -> Coord {
        let (dx, dy) = match self {
            Direction::North => (0, -1),
            Direction::South => (0, 1),
            Direction::East => (1, 0),
            Direction::West => (-1, 0),
        };
        (x + dx, y + dy)
    }

    fn turn_left(&self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
            Direction::West => Direction::South,
        }
    }

    fn turn_right(&self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::South => Direction::West,
            Direction::East => Direction::South,
            Direction::West => Direction::North,
        }
    }
}

fn get_next_flat_position(
    grid: &Grid,
    current_position @ (x, y): Coord,
    dir: Direction,
) -> (Coord, Direction) {
    let new_position = dir.apply_to_coord(current_position);
    match grid.get(&new_position) {
        Some(_) => (new_position, dir),
        None => {
            // Find the other side of the edge by walking in the same direction from the opposite
            // side of the grid.
            let height = grid.keys().max_by_key(|(_, y)| *y).unwrap().1;
            let width = grid.keys().max_by_key(|(x, _)| *x).unwrap().0;
            let (start_position, count) = match dir {
                Direction::North => ((x, height), height),
                Direction::South => ((x, 0), height),
                Direction::East => ((0, y), width),
                Direction::West => ((width, y), width),
            };
            let end_position = (0..=count)
                .fold_while(start_position, |coord, _| match grid.get(&coord) {
                    Some(_) => Done(coord),
                    None => Continue(dir.apply_to_coord(coord)),
                })
                .into_inner();
            (end_position, dir)
        }
    }
}

// See day22-mapping.png for how the region numbers are mapped.
fn get_next_cube_position(
    grid: &Grid,
    current_position: Coord,
    dir: Direction,
) -> (Coord, Direction) {
    let region = (current_position.0 / 50, current_position.1 / 50);
    let new_position @ (nx, ny) = dir.apply_to_coord(current_position);
    match grid.get(&new_position) {
        Some(_) => (new_position, dir),
        None => match (region, dir) {
            // 1 to 5*
            ((0, 3), Direction::West) => ((ny - 100, 0), Direction::South),
            // 5 to 1*
            ((1, 0), Direction::North) => ((0, nx + 100), Direction::East),

            // 1 to 6
            ((0, 3), Direction::South) => ((nx + 100, 0), Direction::South),
            // 6 to 1
            ((2, 0), Direction::North) => ((nx - 100, 199), Direction::North),

            // 1 to 3*
            ((0, 3), Direction::East) => ((ny - 100, 149), Direction::North),
            // 3 to 1*
            ((1, 2), Direction::South) => ((49, nx + 100), Direction::West),

            // 2 to 5!
            ((0, 2), Direction::West) => ((50, 149 - ny), Direction::East),
            // 5 to 2!
            ((1, 0), Direction::West) => ((0, 149 - ny), Direction::East),

            // 2 to 4*
            ((0, 2), Direction::North) => ((50, nx + 50), Direction::East),
            // 4 to 2*
            ((1, 1), Direction::West) => ((ny - 50, 100), Direction::South),

            // 3 to 6!
            ((1, 2), Direction::East) => ((149, 149 - ny), Direction::West),
            // 6 to 3!
            ((2, 0), Direction::East) => ((99, 149 - ny), Direction::West),

            // 4 to 6*
            ((1, 1), Direction::East) => ((ny + 50, 49), Direction::North),
            // 6 to 4*
            ((2, 0), Direction::South) => ((99, nx - 50), Direction::West),

            _ => (new_position, dir),
        },
    }
}

fn solve<F>(grid: &Grid, instructions: Vec<Instruction>, start: Coord, get_next_position: F) -> i32
where
    F: Fn(&Grid, Coord, Direction) -> (Coord, Direction),
{
    let ((x, y), dir) = instructions.iter().fold(
        (start, Direction::East),
        |(position, dir), inst| match inst {
            Instruction::Forward(n) => (0..*n)
                .fold_while((position, dir), |(position, dir), _| {
                    let (next_position, next_dir) = get_next_position(&grid, position, dir);
                    match grid.get(&next_position) {
                        Some(Tile::Open) => Continue((next_position, next_dir)),
                        Some(Tile::Wall) => Done((position, dir)),
                        _ => panic!("Impossible"),
                    }
                })
                .into_inner(),
            Instruction::TurnLeft => (position, dir.turn_left()),
            Instruction::TurnRight => (position, dir.turn_right()),
        },
    );
    ((y + 1) * 1000) + ((x + 1) * 4) + dir.value()
}

pub fn part1(input: &str) -> i32 {
    let (grid, instructions, start) = parse(input);
    solve(&grid, instructions, start, get_next_flat_position)
}

pub fn part2(input: &str) -> i32 {
    let (grid, instructions, start) = parse(input);
    solve(&grid, instructions, start, get_next_cube_position)
}

fn parse(input: &str) -> (Grid, Vec<Instruction>, Coord) {
    let (grid, instructions) = input.split_once("\n\n").unwrap();
    let start_coord: RefCell<Option<Coord>> = RefCell::new(None);
    let grid: Grid = grid
        .lines()
        .zip(0..)
        .flat_map(|(row, y)| {
            row.chars()
                .zip(0..)
                .filter_map(|(ch, x)| {
                    let coord = (x, y);
                    match ch {
                        '.' => {
                            if start_coord.borrow().is_none() {
                                start_coord.replace(Some(coord));
                            }
                            Some((coord, Tile::Open))
                        }
                        '#' => Some((coord, Tile::Wall)),
                        _ => None,
                    }
                })
                .collect_vec()
        })
        .collect();
    let re = Regex::new(r"([LR])").unwrap();
    let inst = re
        .replace_all(instructions, "\n$0\n")
        .split_whitespace()
        .map(|s| match (s, s.parse::<u32>()) {
            ("L", _) => Instruction::TurnLeft,
            ("R", _) => Instruction::TurnRight,
            (_, Ok(n)) => Instruction::Forward(n),
            _ => panic!("Unknown instruction {}", s),
        })
        .collect();
    (grid, inst, start_coord.take().unwrap())
}

mod tests {
    use crate::utils;

    #[test]
    fn part1() {
        assert_eq!(
            6032,
            super::part1(utils::read_input("2022/day22_sample").as_str())
        );
        assert_eq!(
            50412,
            super::part1(utils::read_input("2022/day22").as_str())
        );
    }

    #[test]
    fn part2() {
        assert_eq!(
            130068,
            super::part2(utils::read_input("2022/day22").as_str())
        );
    }
}
