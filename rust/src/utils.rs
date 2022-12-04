use regex::Regex;
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

pub fn line_groups(content: &str) -> impl Iterator<Item = impl Iterator<Item = &str>> {
    content.trim_end().split("\n\n").map(|s| s.split("\n"))
}

pub fn regex_captures(expr: &Regex, input: &str) -> Vec<Vec<String>> {
    expr.captures_iter(input)
        .map(|c| {
            c.iter()
                .skip(1)
                .filter_map(|m| m.map(|x| x.as_str().to_string()))
                .collect::<Vec<_>>()
        })
        .collect()
}
