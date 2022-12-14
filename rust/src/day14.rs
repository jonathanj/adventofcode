use crate::utils::Animation;
use image::{Rgba, RgbaImage};
use std::collections::HashMap;

type Coord = (i32, i32);
type GridMap = HashMap<Coord, char>;

const SAND_ORIGIN: Coord = (500, 0);

enum HitResult {
    Hit,
    NoHit,
    Terminate,
}

fn next_sand_pt<F>(grid: &GridMap, start: &Coord, hit_test: F) -> Option<Coord>
where
    F: Fn(&GridMap, Coord) -> HitResult,
{
    let mut x = start.0;
    for y in start.1.. {
        match hit_test(grid, (x, y + 1)) {
            HitResult::Terminate => break,
            HitResult::NoHit => (),
            HitResult::Hit => {
                match (
                    /* L */ hit_test(grid, (x - 1, y + 1)),
                    /* M */ hit_test(grid, (x, y)),
                    /* R */ hit_test(grid, (x + 1, y + 1)),
                ) {
                    (HitResult::NoHit, _, _) => x -= 1,
                    (_, _, HitResult::NoHit) => x += 1,
                    (_, HitResult::NoHit, _) => return Some((x, y)),
                    _ => break,
                }
            }
        }
    }
    None
}

fn simulate_sand<F, C>(grid: &mut GridMap, hit_test: F, mut iteration_callback: C) -> i32
where
    F: Fn(&GridMap, Coord) -> HitResult,
    C: FnMut(&GridMap, i32) -> (),
{
    let mut count = 0;
    loop {
        match next_sand_pt(grid, &SAND_ORIGIN, &hit_test) {
            Some((x, y)) => {
                grid.insert((x, y), 'o');
                iteration_callback(&grid, count);
                count += 1
            }
            None => break,
        }
    }
    iteration_callback(&grid, count);
    count
}

pub fn part1(input: &str) -> i32 {
    let mut grid = parse(input);
    let bottom = grid_bottom(&grid);
    let hit_test = |grid: &GridMap, pt: Coord| match pt {
        (_, y) if y > bottom => HitResult::Terminate,
        pt if grid.contains_key(&pt) => HitResult::Hit,
        _ => HitResult::NoHit,
    };
    let mut anim = Animation::new("day14-1.gif");
    simulate_sand(&mut grid, &hit_test, |grid, count| {
        if count % 100 == 0 {
            anim.add_frame(grid_map_to_image(grid));
        }
    })
}

pub fn part2(input: &str) -> i32 {
    let mut grid = parse(input);
    let bottom = grid_bottom(&grid) + 2;
    let hit_test = |grid: &GridMap, pt: Coord| match pt {
        (_, y) if y == bottom => HitResult::Hit,
        pt if grid.contains_key(&pt) => HitResult::Hit,
        _ => HitResult::NoHit,
    };
    let mut anim = Animation::new("day14-2.gif");
    simulate_sand(&mut grid, &hit_test, |grid, count| {
        if count % 100 == 0 {
            anim.add_frame(grid_map_to_image(grid));
        }
    })
}

fn grid_bounds(grid: &GridMap) -> (Coord, Coord) {
    grid.keys().fold(
        ((i32::MAX, i32::MAX), (i32::MIN, i32::MIN)),
        |((nx, ny), (mx, my)), (x, y)| ((nx.min(*x), ny.min(*y)), (mx.max(*x), my.max(*y))),
    )
}

fn grid_bottom(grid: &GridMap) -> i32 {
    grid_bounds(&grid).1 .1
}

fn render_grid(grid: &GridMap) {
    let ((nx, ny), (mx, my)) = grid_bounds(grid);
    for y in 0..(my - ny) + 1 {
        let mut line = String::new();
        for x in 0..(mx - nx) + 1 {
            let (gx, gy) = ((nx + x), (ny + y));
            match grid.get(&(gx, gy)) {
                Some(ch) => line.push(*ch),
                None => line.push('.'),
            }
        }
        println!("{}", line);
    }
    println!();
}

fn parse(input: &str) -> GridMap {
    let mut grid = GridMap::new();
    for line in input.lines() {
        let pts = line
            .split(" -> ")
            .map(|p_str| {
                let (x_str, y_str) = p_str.split_once(',').unwrap();
                (x_str.parse::<i32>().unwrap(), y_str.parse::<i32>().unwrap())
            })
            .collect::<Vec<_>>();
        for window in pts.windows(2) {
            match window {
                [(ax, ay), (bx, by)] => {
                    for iy in *ay.min(by)..*ay.max(by) + 1 {
                        for ix in *ax.min(bx)..*ax.max(bx) + 1 {
                            grid.insert((ix, iy), '#');
                        }
                    }
                }
                _ => panic!("Impossible!"),
            }
        }
    }
    grid
}

fn grid_map_to_image(grid: &GridMap) -> RgbaImage {
    let ((nx, ny), (mx, my)) = grid_bounds(grid);
    let padding = 0;
    let exact_width = padding * 2 + (mx - nx + 1) as u32;
    let exact_height = padding * 2 + (my - ny + 1) as u32;
    let width = 400;
    let height = 300;
    let width_offset = (width - exact_width).max(0);
    let height_offset = (height - exact_height).max(0);

    RgbaImage::from_fn(width, height, |x, y| {
        let (gx, gy) = (
            (nx + (x as i32) - (padding + width_offset / 2) as i32),
            (ny + (y as i32) - (padding + height_offset / 2) as i32),
        );
        match grid.get(&(gx, gy)) {
            Some('o') => Rgba([255u8, 0u8, 255u8, 255u8]),
            Some(_) => Rgba([0u8, 0u8, 0u8, 255u8]),
            None => Rgba([255u8, 255u8, 255u8, 255u8]),
        }
    })
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    #[test]
    fn part1() {
        assert_eq!(24, super::part1(SAMPLE));
        assert_eq!(625, super::part1(utils::read_input("2022/day14").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(93, super::part2(SAMPLE));
        assert_eq!(
            25193,
            super::part2(utils::read_input("2022/day14").as_str())
        );
    }
}
