use itertools::Itertools;
use std::collections::{HashSet, VecDeque};

type Coord = (i64, i64, i64);

pub fn part1(input: &str) -> usize {
    let coords: HashSet<_> = parse(input).into_iter().collect();
    let mut count = 0;
    for (x, y, z) in &coords {
        for (dx, dy, dz) in [
            (0, 0, 1),
            (0, 0, -1),
            (0, 1, 0),
            (0, -1, 0),
            (1, 0, 0),
            (-1, 0, 0),
        ] {
            let ncoord = (x + dx, y + dy, z + dz);
            if !coords.contains(&ncoord) {
                count += 1;
            }
        }
    }
    return count;
}

pub fn part2(input: &str) -> usize {
    let coords: HashSet<_> = parse(input).into_iter().collect();
    let min_x = coords.iter().min_by_key(|(x, _, _)| *x).unwrap().0 - 1;
    let min_y = coords.iter().min_by_key(|(_, y, _)| *y).unwrap().1 - 1;
    let min_z = coords.iter().min_by_key(|(_, _, z)| *z).unwrap().2 - 1;
    let max_x = coords.iter().max_by_key(|(x, _, _)| *x).unwrap().0 + 1;
    let max_y = coords.iter().max_by_key(|(_, y, _)| *y).unwrap().1 + 1;
    let max_z = coords.iter().max_by_key(|(_, _, z)| *z).unwrap().2 + 1;
    let mut queue = VecDeque::from([(min_x, min_y, min_z)]);
    let mut seen = HashSet::new();
    let mut count = 0;

    while !queue.is_empty() {
        let ncoord @ (x, y, z) = queue.pop_front().unwrap();
        if x < min_x
            || y < min_y
            || z < min_z
            || x > max_x
            || y > max_y
            || z > max_z
            || seen.contains(&ncoord)
        {
            continue;
        }

        seen.insert(ncoord);

        for (dx, dy, dz) in [
            (0, 0, 1),
            (0, 0, -1),
            (0, 1, 0),
            (0, -1, 0),
            (1, 0, 0),
            (-1, 0, 0),
        ] {
            let ncoord = (x + dx, y + dy, z + dz);
            if coords.contains(&ncoord) {
                count += 1;
            } else {
                queue.push_back(ncoord);
            }
        }
    }

    return count;
}

fn parse(input: &str) -> Vec<Coord> {
    input
        .lines()
        .map(|line| match line.split(',').collect_vec()[..] {
            [x, y, z] => (
                x.parse::<i64>().unwrap(),
                y.parse::<i64>().unwrap(),
                z.parse::<i64>().unwrap(),
            ),
            _ => panic!("Malformed input"),
        })
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

    #[test]
    fn part1() {
        assert_eq!(64, super::part1(SAMPLE));
        assert_eq!(4500, super::part1(utils::read_input("2022/day18").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(58, super::part2(SAMPLE));
        assert_eq!(2558, super::part2(utils::read_input("2022/day18").as_str()));
    }
}
