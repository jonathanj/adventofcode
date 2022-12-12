use petgraph::algo::dijkstra;
use petgraph::graphmap::GraphMap;
use petgraph::Directed;

type Day12Graph = GraphMap<Coord, i32, Directed>;
type Coord = (i32, i32);

pub fn part1(input: &str) -> i32 {
    let (start, end, g, _) = parse(input);
    shortest_path(start, end, &g).unwrap()
}

pub fn part2(input: &str) -> i32 {
    let (_, end, g, starting_coords) = parse(input);
    starting_coords
        .iter()
        .filter_map(|start| shortest_path(*start, end, &g))
        .min()
        .unwrap()
}

fn shortest_path(start: Coord, end: Coord, g: &Day12Graph) -> Option<i32> {
    dijkstra(&g, start, Some(end), |_| 1)
        .get(&end)
        .map(|cost| *cost)
}

fn parse(input: &str) -> (Coord, Coord, Day12Graph, Vec<Coord>) {
    let mut g = Day12Graph::new();
    let mut starting_points: Vec<Coord> = vec![];
    let mut start: Coord = (-1, -1);
    let mut end: Coord = (-1, -1);
    let lines: Vec<Vec<u8>> = input.lines().map(|line| line.bytes().collect()).collect();
    let height = lines.len() as i32;
    let width = lines[0].len() as i32;
    let node_value = |(x, y): &Coord| {
        let b = &lines[*y as usize][*x as usize];
        match *b {
            83 /* S */ => 0,
            69 /* E */ => 25,
            b => b as i32 - 97,
        }
    };

    for (y, row) in (0..).zip(&lines) {
        for (x, b) in (0..).zip(row) {
            match *b {
                83 /* S */ => start = (x, y),
                69 /* E */ => end = (x, y),
                _ => (),
            };
            let coord = (x, y);
            let value = node_value(&coord);
            if value == 0 {
                starting_points.push(coord);
            }
            g.add_node(coord);

            for (cx, cy) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
                let target_coord = (cx, cy);
                if cx >= 0 && cx < width && cy >= 0 && cy < height {
                    let target_value = node_value(&target_coord);
                    if target_value <= value + 1 {
                        g.add_edge(coord, target_coord, target_value);
                    }
                }
            }
        }
    }
    (start, end, g, starting_points)
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    #[test]
    fn part1() {
        assert_eq!(31, super::part1(SAMPLE));
        assert_eq!(534, super::part1(utils::read_input("2022/day12").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(29, super::part2(SAMPLE));
        assert_eq!(525, super::part2(utils::read_input("2022/day12").as_str()));
    }
}
