use num_integer::Integer;
use std::collections::{HashMap, HashSet, VecDeque};

type Coord = (i32, i32);

struct Blizzards {
    data: HashMap<Direction, HashSet<Coord>>,
    width: i32,
    height: i32,
}

impl Blizzards {
    fn blocked_at(&self, (x, y): &Coord, time: i32, dir: Direction) -> bool {
        let (bdx, bdy) = dir.offset();
        // This is like Python's modulo operator with respect to negative numbers.
        let bx = (x - bdx * time).rem_euclid(self.width);
        let by = (y - bdy * time).rem_euclid(self.height);
        self.data[&dir].contains(&(bx, by))
    }

    fn is_out_of_bounds(&self, (x, y): &Coord) -> bool {
        let (x, y) = (*x, *y);
        x < 0 || x >= self.width || y < 0 || y >= self.height
    }
}

#[derive(Eq, PartialEq, Hash, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn offset(&self) -> Coord {
        match self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

fn solve(blizzards: &Blizzards, start_time: i32, start: &Coord, end: &Coord) -> i32 {
    let start = *start;
    let end = *end;
    let mut queue: VecDeque<_> = [(start_time, start)].into();
    let mut seen = HashSet::new();
    // The trick here is that the blizzards are predictable, technically the search space is
    // width * height but they will repeat at lcm(width, height).
    let lcm = blizzards.width.lcm(&blizzards.height);
    while !queue.is_empty() {
        let (mut time, (cx, cy)) = queue.pop_front().unwrap();
        time += 1;

        'search: for ncoord in [
            (cx + 1, cy),
            (cx - 1, cy),
            (cx, cy + 1),
            (cx, cy - 1),
            (cx, cy),
        ] {
            if ncoord == end {
                return time - start_time;
            } else if ncoord != start && blizzards.is_out_of_bounds(&ncoord) {
                continue 'search;
            }

            if ncoord != start {
                for dir in [
                    Direction::Left,
                    Direction::Right,
                    Direction::Up,
                    Direction::Down,
                ] {
                    if blizzards.blocked_at(&ncoord, time, dir) {
                        continue 'search;
                    }
                }
            }

            let key = (ncoord, time % lcm);
            if !seen.contains(&key) {
                seen.insert(key);
                queue.push_back((time, ncoord));
            }
        }
    }
    panic!("Couldn't find a goal");
}

pub fn part1(input: &str) -> i32 {
    let (blizzards, start, end) = parse(input);
    solve(&blizzards, 0, &start, &end)
}

pub fn part2(input: &str) -> i32 {
    let (blizzards, start, end) = parse(input);
    // start -> end -> start -> end
    let a = solve(&blizzards, 0, &start, &end);
    let b = solve(&blizzards, a, &end, &start);
    let c = solve(&blizzards, a + b, &start, &end);
    a + b + c
}

fn parse(input: &str) -> (Blizzards, Coord, Coord) {
    let mut data = HashMap::new();
    let lines: Vec<_> = input.lines().collect();
    let width = lines[0].len() as i32 - 2;
    let height = lines.len() as i32 - 2;
    for (y, line) in (0..).zip(lines.into_iter().skip(1)) {
        for (x, ch) in (0..).zip(line.chars().skip(1)) {
            let dir = match ch {
                '<' => Some(Direction::Left),
                '>' => Some(Direction::Right),
                '^' => Some(Direction::Up),
                'v' => Some(Direction::Down),
                _ => None,
            };
            if let Some(dir) = dir {
                data.entry(dir).or_insert(HashSet::new()).insert((x, y));
            }
        }
    }
    let start = (0, -1);
    let end = (width - 1, height);
    (
        Blizzards {
            data,
            width,
            height,
        },
        start,
        end,
    )
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";

    #[test]
    fn part1() {
        assert_eq!(18, super::part1(SAMPLE));
        assert_eq!(295, super::part1(utils::read_input("2022/day24").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(54, super::part2(SAMPLE));
        assert_eq!(851, super::part2(utils::read_input("2022/day24").as_str()));
    }
}
