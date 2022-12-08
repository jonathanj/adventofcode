use std::iter;

pub fn part1(input: &str) -> usize {
    let grid = PuzzleGrid::from_str(input);
    let perimeter = 2 * (grid.width + grid.height) - 4;
    grid.inner_coords()
        .filter(|(x, y)| {
            let target_tree = grid.at(*x, *y);
            grid.direction_rays(*x, *y)
                .iter()
                .flat_map(|ray| ray.iter().max())
                .any(|ray_max| target_tree > *ray_max)
        })
        .count()
        + perimeter
}

fn scenic_score(grid: &PuzzleGrid, x: usize, y: usize) -> i32 {
    let current_tree = grid.at(x, y);
    grid.direction_rays(x, y).iter().fold(1, |acc, ns| {
        let mut score = 0;
        for other in ns {
            score += 1;
            if *other >= current_tree {
                break;
            }
        }
        acc * score
    })
}

pub fn part2(input: &str) -> i32 {
    let grid = PuzzleGrid::from_str(input);
    grid.inner_coords()
        .map(|(x, y)| scenic_score(&grid, x, y))
        .max()
        .unwrap()
}

struct PuzzleGrid {
    width: usize,
    height: usize,
    data: Vec<Vec<i32>>,
}

impl PuzzleGrid {
    fn from_str(input: &str) -> PuzzleGrid {
        let data: Vec<Vec<i32>> = input
            .lines()
            .map(|line| line.bytes().map(|b| b as i32 - 48).collect())
            .collect();
        PuzzleGrid {
            width: data[0].len(),
            height: data.len(),
            data,
        }
    }

    fn at(&self, x: usize, y: usize) -> i32 {
        self.data[y][x]
    }

    fn direction_rays(&self, x: usize, y: usize) -> [Vec<i32>; 4] {
        [
            (0..y).rev().map(|y| self.at(x, y)).collect(),
            (y + 1..self.height).map(|y| self.at(x, y)).collect(),
            (x + 1..self.width).map(|x| self.at(x, y)).collect(),
            (0..x).rev().map(|x| self.at(x, y)).collect(),
        ]
    }

    fn inner_coords(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        (1..self.height - 1).flat_map(move |y| (1..self.width - 1).map(move |x| (x, y)))
    }
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
30373
25512
65332
33549
35390";

    #[test]
    fn part1() {
        assert_eq!(21, super::part1(SAMPLE));
        assert_eq!(1816, super::part1(utils::read_input("2022/day8").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(8, super::part2(SAMPLE));
        assert_eq!(
            383520,
            super::part2(utils::read_input("2022/day8").as_str())
        );
    }
}
