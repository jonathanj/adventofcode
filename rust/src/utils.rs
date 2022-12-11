use regex::Regex;
use std::fs::{self, File};
use std::io::{self, BufRead};
use std::str::FromStr;

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

pub fn regex_capture<F: FromStr>(expr: &Regex, input: &str) -> Result<F, F::Err> {
    match expr
        .captures_iter(input)
        .flat_map(|c| c.get(1).map(|m| m.as_str().parse()))
        .next()
    {
        Some(res) => res,
        _ => panic!("No regex captures"),
    }
}
