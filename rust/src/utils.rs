use std::fs::{self, File};
use std::io::{self, BufRead};

#[allow(dead_code)]
pub fn read_lines(filename: &str) -> io::Result<io::Lines<io::BufReader<File>>> {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub fn read_input(filename: &str) -> String {
    fs::read_to_string("../inputs/".to_string() + filename).unwrap()
}

pub fn line_groups(content: &str) -> impl Iterator<Item=impl Iterator<Item=&str>> {
    content
        .trim_end()
        .split("\n\n")
        .map(|s| s.split("\n"))
}
