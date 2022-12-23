use crate::utils::regex_captures;
use itertools::FoldWhile::{Continue, Done};
use itertools::{enumerate, Itertools};
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::iter;

type Coord = (i64, i64);
type Field = HashSet<Coord>;

#[derive(Debug)]
struct Block {
    data: Vec<Vec<bool>>,
    height: i32,
}

fn move_block((x, y): Coord, (dx, dy): Coord, field: &Field, block: &Block) -> MoveState {
    for ry in 0..4 {
        for rx in 0..4 {
            let rbx = x + rx + dx;
            let rby = y - ry + dy;
            if !block.data[ry as usize][rx as usize] {
                // Empty block square.
                continue;
            } else if rbx < 0 || rbx > 6 {
                // Block square moved horizontally off the field.
                return MoveState::Collided((x, y));
            } else if rby < 0 {
                // Block square hit the bottom of the field.
                return MoveState::Collided((x, y));
            } else if field.contains(&(rbx, rby)) {
                // Block square hit another fixed square.
                return MoveState::Collided((x, y));
            }
        }
    }
    MoveState::Falling((x + dx, y + dy))
}

fn move_across(coord: Coord, field: &Field, block: &Block, direction: &Direction) -> Coord {
    let dx = match direction {
        Direction::Left => -1,
        _ => 1,
    };
    match move_block(coord, (dx, 0), field, block) {
        MoveState::Collided(coord) => coord,
        MoveState::Falling(coord) => coord,
    }
}

enum MoveState {
    Falling(Coord),
    Collided(Coord),
}

fn move_down(coord: Coord, field: &Field, block: &Block) -> MoveState {
    move_block(coord, (0, -1), field, block)
}

fn fill_block((x, y): Coord, field: &mut Field, block: &Block) {
    for ry in 0..4 {
        let cy = y - ry as i64;
        for rx in 0..4 {
            if block.data[ry][rx] {
                let cx = x + rx as i64;
                field.insert((cx, cy));
            }
        }
    }
}

fn field_extent(field: &Field) -> i64 {
    field.iter().max_by_key(|(_, y)| y).unwrap().1
}

#[allow(dead_code)]
fn render_field(field: &Field) {
    let y_extent = field_extent(field);
    for y in (0..=y_extent).rev() {
        for x in 0..7 {
            print!("{}", if field.contains(&(x, y)) { "#" } else { "." });
        }
        println!();
    }
}

pub fn part1(input: &str) -> i64 {
    let (all_moves, all_blocks) = parse(input);
    let mut moves = all_moves.iter().cycle();
    let mut blocks = all_blocks.iter().cycle();
    let mut field = Field::new();
    let mut max_y = 0;
    let mut current_block = blocks.next().unwrap();
    let mut current_coord = (2, 3);
    let mut count = 0;

    while count < 2022 {
        loop {
            let mv = moves.next().unwrap();
            match move_down(
                move_across(current_coord, &field, &current_block, mv),
                &field,
                &current_block,
            ) {
                MoveState::Falling(coord) => current_coord = coord,
                MoveState::Collided(coord) => {
                    fill_block(coord, &mut field, &current_block);
                    max_y = field_extent(&field);
                    count += 1;
                    current_block = blocks.next().unwrap();
                    current_coord = (2, max_y + 3 + current_block.height as i64);
                    break;
                }
            }
        }
    }

    return max_y + 1;
}

pub fn part2(input: &str) -> i64 {
    let mut field = Field::new();
    let (all_moves, all_blocks) = parse(input);
    let mut moves = all_moves.iter().enumerate().cycle();
    let mut blocks = all_blocks.iter().enumerate().cycle();
    let mut max_y = 0;
    let (mut current_block_id, mut current_block) = blocks.next().unwrap();
    let mut current_coord = (2, 3);
    let mut count = 0;
    let mut seen = HashMap::<(usize, Vec<i64>, usize), (i64, i64)>::new();

    let mut extra_max_y = 0;

    while count < 1000000000000i64 {
        loop {
            let (move_id, mv) = moves.next().unwrap();
            match move_down(
                move_across(current_coord, &field, &current_block, mv),
                &field,
                &current_block,
            ) {
                MoveState::Falling(coord) => current_coord = coord,
                MoveState::Collided(coord) => {
                    fill_block(coord, &mut field, &current_block);
                    max_y = field_extent(&field);
                    (current_block_id, current_block) = blocks.next().unwrap();
                    current_coord = (2, max_y + 3 + current_block.height as i64);

                    // Fingerprint the current structure of the field by finding the relative
                    // distance for the top of each column.
                    let max_ys = field.iter().fold([-1; 7], |mut max_ys, (x, y)| {
                        max_ys[*x as usize] = max_ys[*x as usize].max(*y);
                        max_ys
                    });
                    let norm_max_y = *max_ys.iter().max().unwrap();
                    let top_y_deltas = max_ys.iter().map(|my| my - norm_max_y).collect();
                    let key = (move_id, top_y_deltas, current_block_id);
                    if seen.contains_key(&key) {
                        let (old_count, old_max_y) = *seen.get(&key).unwrap();
                        let repeat = (1000000000000i64 - count) / (count - old_count);
                        count += repeat * (count - old_count);
                        extra_max_y += repeat * (max_y as i64 - old_max_y);
                    }
                    seen.insert(key, (count, max_y as i64));

                    count += 1;
                    break;
                }
            }
        }
    }

    return (max_y as i64) + 1 + extra_max_y;
}

const BLOCKS: &str = "\
####
....
....
....

.#..
###.
.#..

..#.
..#.
###.

#...
#...
#...
#...

##..
##..
....
....";

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

fn parse(input: &str) -> (Vec<Direction>, Vec<Block>) {
    let blocks = BLOCKS
        .split("\n\n")
        .map(|lines| {
            let mut data = vec![
                vec![false, false, false, false],
                vec![false, false, false, false],
                vec![false, false, false, false],
                vec![false, false, false, false],
            ];
            let mut height = 0;
            for (y, line) in lines.lines().enumerate() {
                if line.contains("#") {
                    height += 1;
                }
                for (x, ch) in line.chars().enumerate() {
                    data[y][x] = ch == '#';
                }
            }
            Block { data, height }
        })
        .collect();

    let moves = input
        .trim()
        .chars()
        .map(|ch| match ch {
            '<' => Direction::Left,
            '>' => Direction::Right,
            _ => panic!("Unknown input {}", ch),
        })
        .collect();

    (moves, blocks)
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

    #[test]
    fn part1() {
        assert_eq!(3068, super::part1(SAMPLE));
        assert_eq!(3159, super::part1(utils::read_input("2022/day17").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(1514285714288, super::part2(SAMPLE));
        assert_eq!(
            1566272189352,
            super::part2(utils::read_input("2022/day17").as_str())
        );
    }
}
