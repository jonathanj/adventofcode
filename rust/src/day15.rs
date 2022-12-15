use crate::utils::regex_captures;
use regex::Regex;
use std::collections::HashSet;
use std::iter;

type Coord = (i32, i32);
type Sensors = Vec<(Coord, Coord, i32)>;

fn unseen_by_any_sensor(sensors: &Sensors, coord: &Coord) -> bool {
    sensors
        .iter()
        .all(|(sensor, _, r)| manhattan_dist(coord, sensor) > *r)
}

pub fn part1(input: &str, target_y: i32) -> usize {
    let sensors = parse(input);
    let (min_x, max_x) = sensors
        .iter()
        .fold((i32::MAX, i32::MIN), |(min_x, max_x), ((x, y), _, r)| {
            (min_x.min(x - r), max_x.max(x + r))
        });
    let beacons = sensors
        .iter()
        .map(|(_, beacon, _)| *beacon)
        .collect::<HashSet<_>>();
    (min_x..=max_x)
        .zip(iter::repeat(target_y))
        .filter(|coord| !unseen_by_any_sensor(&sensors, coord) && !beacons.contains(&coord))
        .count()
}

pub fn part2(input: &str, max_extent: i32) -> i64 {
    let sensors = parse(input);
    for n in 1.. {
        for (sensor, _, r) in &sensors {
            // Expand the diamond edges by `n` to scan for points that are not seen by any sensor.
            let lx = (sensor.0 - *r - n).max(0);
            let ly = sensor.1;
            let tx = sensor.0;
            let ty = (sensor.1 - *r - n).max(0);
            let rx = (sensor.0 + *r + n).min(max_extent);
            let ry = sensor.1;
            let bx = sensor.0;
            let by = (sensor.1 + *r + n).min(max_extent);

            match (lx..=tx)
                .zip(ly..=ty)
                .chain((tx..=rx).zip(ly..=ry))
                .chain((bx..=rx).zip(by..=ry).chain((lx..=bx).zip(ly..=by)))
                .find(|coord| unseen_by_any_sensor(&sensors, coord))
            {
                Some((x, y)) => return x as i64 * 4000000 + y as i64,
                _ => (),
            }
        }
    }

    panic!("Impossible");
}

fn manhattan_dist(a: &Coord, b: &Coord) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

fn parse(input: &str) -> Sensors {
    let expr = Regex::new(
        r"Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)",
    )
    .unwrap();
    input
        .lines()
        .map(|line| match &regex_captures(&expr, line)[0][..] {
            [sx_str, sy_str, bx_str, by_str] => {
                let signal = (
                    sx_str.parse::<i32>().unwrap(),
                    sy_str.parse::<i32>().unwrap(),
                );
                let beacon = (
                    bx_str.parse::<i32>().unwrap(),
                    by_str.parse::<i32>().unwrap(),
                );
                (signal, beacon, manhattan_dist(&signal, &beacon))
            }
            _ => panic!("Impossible!"),
        })
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    #[test]
    fn part1() {
        assert_eq!(26, super::part1(SAMPLE, 10));
        assert_eq!(
            5367037,
            super::part1(utils::read_input("2022/day15").as_str(), 2000000)
        );
    }

    #[test]
    fn part2() {
        assert_eq!(56000011, super::part2(SAMPLE, 20));
        assert_eq!(
            11914583249288i64,
            super::part2(utils::read_input("2022/day15").as_str(), 4000000)
        );
    }
}
