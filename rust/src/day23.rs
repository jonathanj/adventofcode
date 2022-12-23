use std::collections::{HashMap, HashSet, VecDeque};

type Coord = (i32, i32);
type Offsets = VecDeque<Vec<Coord>>;
type Grid = HashSet<Coord>;

const STARTING_OFFSETS: [[Coord; 3]; 4] = [
    // N
    [(0, -1), (1, -1), (-1, -1)],
    // S
    [(0, 1), (1, 1), (-1, 1)],
    // W
    [(-1, 0), (-1, -1), (-1, 1)],
    // E
    [(1, 0), (1, -1), (1, 1)],
];

pub fn part1(input: &str) -> u32 {
    let grid = parse(input);
    let offsets: Offsets = STARTING_OFFSETS
        .map(|offs| offs.into_iter().collect())
        .into_iter()
        .collect();
    let (grid, _) = (0..10).fold((grid, offsets), |(grid, offsets), _| {
        let (grid, offsets) = elf_round(&grid, &offsets);
        (grid, offsets)
    });
    return count_spaces(&grid);
}

pub fn part2(input: &str) -> u32 {
    let mut grid = parse(input);
    let mut offsets: Offsets = STARTING_OFFSETS
        .map(|offs| offs.into_iter().collect())
        .into_iter()
        .collect();
    let mut round = 0;
    loop {
        round += 1;
        let prev_grid = grid.clone();
        (grid, offsets) = elf_round(&grid, &offsets);
        if prev_grid == grid {
            break round;
        }
    }
}

fn count_spaces(grid: &Grid) -> u32 {
    let ((min_x, min_y), (max_x, max_y)) = grid_extents(grid);
    (max_y - min_y + 1) as u32 * (max_x - min_x + 1) as u32 - grid.len() as u32
}

fn contains_an_elf(grid: &Grid, (x, y): &Coord, offsets: &Vec<Coord>) -> bool {
    offsets
        .iter()
        .any(|(dx, dy)| grid.contains(&(x + dx, y + dy)))
}

fn elf_round(grid: &Grid, offsets: &Offsets) -> (Grid, Offsets) {
    let mut result = Grid::new();
    let mut proposed: HashMap<Coord, Vec<Coord>> = HashMap::new();
    let mut offsets = offsets.clone();
    for coord in grid {
        // No elves nearby?
        if offsets
            .iter()
            .all(|offsets| !contains_an_elf(&grid, coord, offsets))
        {
            result.insert(*coord);
            continue;
        }

        let mut can_move = false;
        for offsets in &offsets {
            if !contains_an_elf(&grid, coord, offsets) {
                let (dx, dy) = offsets[0];
                let proposed_coord = (coord.0 + dx, coord.1 + dy);
                proposed
                    .entry(proposed_coord)
                    .or_insert(Vec::new())
                    .push(*coord);
                can_move = true;
                break;
            }
        }

        if !can_move {
            result.insert(*coord);
        }
    }

    for (proposed_coord, candidates) in &proposed {
        if candidates.len() == 1 {
            result.insert(*proposed_coord);
        } else {
            for original_coord in candidates {
                result.insert(*original_coord);
            }
        }
    }
    offsets.rotate_left(1);
    (result, offsets)
}

fn grid_extents(grid: &Grid) -> (Coord, Coord) {
    let min_x = grid.iter().min_by_key(|(x, _)| *x).unwrap().0;
    let max_x = grid.iter().max_by_key(|(x, _)| *x).unwrap().0;
    let min_y = grid.iter().min_by_key(|(_, y)| *y).unwrap().1;
    let max_y = grid.iter().max_by_key(|(_, y)| *y).unwrap().1;
    ((min_x, min_y), (max_x, max_y))
}

#[allow(dead_code)]
fn render_grid(grid: &Grid) {
    let ((min_x, min_y), (max_x, max_y)) = grid_extents(grid);
    println!("   {}0", " ".repeat(min_x.abs() as usize));
    for y in min_y..=max_y {
        let row_prefix = if y == 0 { " 0 " } else { "   " };
        print!("{}", row_prefix);
        for x in min_x..=max_x {
            print!("{}", if grid.contains(&(x, y)) { "#" } else { "." });
        }
        println!();
    }
    println!();
}

fn parse(input: &str) -> Grid {
    (0..)
        .zip(input.lines())
        .flat_map(|(y, line)| {
            (0..)
                .zip(line.chars())
                .filter_map(|(x, ch)| if ch == '#' { Some((x, y)) } else { None })
                .collect::<Vec<_>>()
        })
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..";

    #[test]
    fn part1() {
        assert_eq!(110, super::part1(SAMPLE));
        assert_eq!(3931, super::part1(utils::read_input("2022/day23").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(20, super::part2(SAMPLE));
        assert_eq!(944, super::part2(utils::read_input("2022/day23").as_str()));
    }
}
