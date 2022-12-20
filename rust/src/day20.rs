fn mix(numbers: Vec<i64>, iterations: i32) -> i64 {
    let numbers: Vec<_> = numbers.clone().into_iter().enumerate().collect();
    let mut state = numbers.clone();
    let len = state.len() as i64 - 1;
    for _ in 0..iterations {
        for (seek_key, _) in numbers.iter() {
            match state.iter().position(|(key, _)| *key == *seek_key) {
                Some(idx) => {
                    let current = state.remove(idx);
                    let new_idx = (idx as i64 + current.1) % len;
                    match new_idx {
                        // Inserting between the start and the end
                        0 => state.insert(state.len(), current),
                        new_idx if new_idx.is_positive() => {
                            state.insert(new_idx as usize, current);
                        }
                        new_idx if new_idx.is_negative() => {
                            state.insert((new_idx + state.len() as i64) as usize, current);
                        }
                        _ => panic!("What is this case?"),
                    }
                }
                _ => panic!("Missing number"),
            }
        }
    }

    let zero_idx = state.iter().position(|(_, n)| *n == 0).unwrap();
    [1000, 2000, 3000]
        .iter()
        .fold(0, |acc, idx| acc + state[(*idx + zero_idx) % state.len()].1)
}

pub fn part1(input: &str) -> i64 {
    mix(parse(input), 1)
}

const DECRYPTION_KEY: i64 = 811589153;

pub fn part2(input: &str) -> i64 {
    mix(
        parse(input)
            .into_iter()
            .map(|n| n * DECRYPTION_KEY)
            .collect(),
        10,
    )
}

fn parse(input: &str) -> Vec<i64> {
    input
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect()
}

mod tests {
    use crate::utils;

    const SAMPLE: &str = "\
1
2
-3
3
-2
0
4";

    #[test]
    fn part1() {
        assert_eq!(3, super::part1(SAMPLE));
        assert_eq!(7278, super::part1(utils::read_input("2022/day20").as_str()));
    }

    #[test]
    fn part2() {
        assert_eq!(1623178306, super::part2(SAMPLE));
        assert_eq!(
            14375678667089,
            super::part2(utils::read_input("2022/day20").as_str())
        );
    }
}
