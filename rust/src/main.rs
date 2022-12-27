extern crate core;
#[macro_use]
extern crate scan_fmt;

use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::num::ParseIntError;

mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day17;
mod day2;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod utils;

fn main() {
    match parse_args() {
        Ok((1, 1)) => println!("{:?}", day1::part1(read_stdin().as_str())),
        Ok((1, 2)) => println!("{:?}", day1::part2(read_stdin().as_str())),
        Ok((2, 1)) => println!("{:?}", day2::part1(read_stdin().as_str())),
        Ok((2, 2)) => println!("{:?}", day2::part2(read_stdin().as_str())),
        Ok((3, 1)) => println!("{:?}", day3::part1(read_stdin().as_str())),
        Ok((3, 2)) => println!("{:?}", day3::part2(read_stdin().as_str())),
        Ok((4, 1)) => println!("{:?}", day4::part1(read_stdin().as_str())),
        Ok((4, 2)) => println!("{:?}", day4::part2(read_stdin().as_str())),
        Ok((5, 1)) => println!("{:?}", day5::part1(read_stdin().as_str())),
        Ok((5, 2)) => println!("{:?}", day5::part2(read_stdin().as_str())),
        Ok((6, 1)) => println!("{:?}", day6::part1(read_stdin().as_str())),
        Ok((6, 2)) => println!("{:?}", day6::part2(read_stdin().as_str())),
        Ok((7, 1)) => println!("{:?}", day7::part1(read_stdin().as_str())),
        Ok((7, 2)) => println!("{:?}", day7::part2(read_stdin().as_str())),
        Ok((8, 1)) => println!("{:?}", day8::part1(read_stdin().as_str())),
        Ok((8, 2)) => println!("{:?}", day8::part2(read_stdin().as_str())),
        Ok((9, 1)) => println!("{:?}", day9::part1(read_stdin().as_str())),
        Ok((9, 2)) => println!("{:?}", day9::part2(read_stdin().as_str())),
        Ok((10, 1)) => println!("{:?}", day10::part1(read_stdin().as_str())),
        Ok((10, 2)) => println!("{:?}", day10::part2(read_stdin().as_str())),
        Ok((11, 1)) => println!("{:?}", day11::part1(read_stdin().as_str())),
        Ok((11, 2)) => println!("{:?}", day11::part2(read_stdin().as_str())),
        Ok((12, 1)) => println!("{:?}", day12::part1(read_stdin().as_str())),
        Ok((12, 2)) => println!("{:?}", day12::part2(read_stdin().as_str())),
        Ok((13, 1)) => println!("{:?}", day13::part1(read_stdin().as_str())),
        Ok((13, 2)) => println!("{:?}", day13::part2(read_stdin().as_str())),
        Ok((14, 1)) => println!("{:?}", day14::part1(read_stdin().as_str())),
        Ok((14, 2)) => println!("{:?}", day14::part2(read_stdin().as_str())),
        Ok((15, 1)) => println!("{:?}", day15::part1(read_stdin().as_str(), 2000000)),
        Ok((15, 2)) => println!("{:?}", day15::part2(read_stdin().as_str(), 4000000)),
        Ok((17, 1)) => println!("{:?}", day17::part1(read_stdin().as_str())),
        Ok((17, 2)) => println!("{:?}", day17::part2(read_stdin().as_str())),
        Ok((20, 1)) => println!("{:?}", day20::part1(read_stdin().as_str())),
        Ok((20, 2)) => println!("{:?}", day20::part2(read_stdin().as_str())),
        Ok((21, 1)) => println!("{:?}", day21::part1(read_stdin().as_str())),
        Ok((21, 2)) => println!("{:?}", day21::part2(read_stdin().as_str())),
        Ok((22, 1)) => println!("{:?}", day22::part1(read_stdin().as_str())),
        Ok((22, 2)) => println!("{:?}", day22::part2(read_stdin().as_str())),
        Ok((23, 1)) => println!("{:?}", day23::part1(read_stdin().as_str())),
        Ok((23, 2)) => println!("{:?}", day23::part2(read_stdin().as_str())),
        Ok((24, 1)) => println!("{:?}", day24::part1(read_stdin().as_str())),
        Ok((24, 2)) => println!("{:?}", day24::part2(read_stdin().as_str())),
        Ok((25, 1)) => println!("{:?}", day25::part1(read_stdin().as_str())),
        Ok((day, part)) => panic!("Unknown day {:?} and part {:?}", day, part),
        err => panic!("Other error {:?}", err),
    }
}

fn read_stdin() -> String {
    let mut stdin = io::stdin();
    let mut buf = String::new();
    match stdin.read_to_string(&mut buf) {
        Ok(n) if n > 0 => buf,
        _ => panic!("stdin is empty, pipe a puzzle input to it"),
    }
}

fn parse_args() -> Result<(i32, i32), ParseIntError> {
    let argv: Vec<String> = env::args().collect();
    match &argv[..] {
        [_, day_n_string, part_n_string] => {
            Ok((day_n_string.parse::<i32>()?, part_n_string.parse::<i32>()?))
        }
        _ => {
            panic!("usage: $0 [DAY]");
        }
    }
}
