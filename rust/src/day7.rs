use std::collections::VecDeque;

#[derive(Clone, Debug)]
struct File {
    name: String,
    size: i64,
}

#[derive(Clone, Debug)]
struct Directory {
    name: String,
    size: i64,
    directories: Vec<Box<Directory>>,
}

impl Directory {
    pub fn new(name: &str) -> Directory {
        Directory {
            name: name.to_string(),
            size: 0,
            directories: Vec::new(),
        }
    }

    pub fn add_file(&mut self, f: File) {
        self.size += &f.size;
    }

    pub fn add_dir(&mut self, d: Box<Directory>) {
        self.size += d.size;
        self.directories.push(d.clone());
    }
}

fn nested_dirs(root: &Box<Directory>) -> Vec<&Box<Directory>> {
    let mut visited = Vec::new();
    let mut queue = VecDeque::from([root]);
    while let Some(dir) = queue.pop_front() {
        visited.push(dir);
        for inner_dir in &dir.directories {
            queue.push_front(&inner_dir);
        }
    }
    return visited;
}

fn parse_inner<'a>(
    name: &'a str,
    lines: &Vec<&'a str>,
    lines_idx: usize,
) -> (Box<Directory>, usize) {
    let mut current_idx = lines_idx;
    let mut result = Box::new(Directory::new(name));
    while let Some(line) = lines.get(current_idx) {
        current_idx += 1;
        match *line {
            "$ cd .." => break,
            "$ ls" => continue,
            line if line.starts_with("$ cd ") => {
                let (_, name_str) = line.split_once("$ cd ").unwrap();
                let (dir, new_idx) = parse_inner(name_str, lines, current_idx);
                current_idx = new_idx;
                result.add_dir(dir);
            }
            line => {
                let (size_str, name_str) = line.split_once(" ").unwrap();
                match size_str.parse::<i64>() {
                    Ok(size) => result.add_file(File {
                        name: name_str.to_string(),
                        size,
                    }),
                    _ => continue,
                }
            }
        }
    }
    (result, current_idx)
}

fn parse(input: &str) -> Box<Directory> {
    let lines = input.lines().skip(1).collect::<Vec<_>>();
    let (root, _) = parse_inner("", &lines, 0);
    root
}

pub fn part1(input: &str) -> i64 {
    let root = parse(input);
    nested_dirs(&root)
        .into_iter()
        .filter(|dir| dir.size <= 100000)
        .map(|dir| dir.size)
        .sum()
}

pub fn part2_params(input: &str, total_size: i64, required_size: i64) -> i64 {
    let root = parse(input);
    let unused_space = total_size - root.size;
    let mut sorted_dirs = nested_dirs(&root);
    sorted_dirs.sort_by(|a, b| a.size.cmp(&b.size));
    sorted_dirs
        .into_iter()
        .find(|dir| unused_space + dir.size >= required_size)
        .map(|dir| dir.size)
        .unwrap()
}

pub fn part2(input: &str) -> i64 {
    part2_params(input, 70000000, 30000000)
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn part1() {
        assert_eq!(95437, super::part1(SAMPLE));
        assert_eq!(
            1297159,
            super::part1(utils::read_input("2022/day7").as_str())
        );
    }

    #[test]
    fn part2() {
        assert_eq!(24933642, super::part2(SAMPLE));
        assert_eq!(
            3866390,
            super::part2(utils::read_input("2022/day7").as_str())
        );
    }
}
