use num_integer::Integer;

pub fn part1(input: &str) -> String {
    i64_to_snafu(parse(input).into_iter().sum())
}

fn snafu_to_i64(snafu: &str) -> i64 {
    snafu.chars().rev().zip((0..)).fold(0, |sum, (ch, n)| {
        sum + (match ch {
            '-' => -1,
            '=' => -2,
            ch => ch.to_digit(10).unwrap() as i64,
        } * 5_i64.pow(n))
    })
}

fn i64_to_snafu(n: i64) -> String {
    let mut n = n;
    let mut result = Vec::new();
    while n > 0 {
        let (new_n, ch) = match n.div_rem(&5) {
            (new_n, 4) => (new_n + 1, '-'),
            (new_n, 3) => (new_n + 1, '='),
            (new_n, rem) => (new_n, char::from_digit(rem as u32, 10).unwrap()),
        };
        result.push(ch);
        n = new_n;
    }
    result.iter().rev().collect::<String>()
}

fn parse(input: &str) -> Vec<i64> {
    input.lines().map(snafu_to_i64).collect()
}

mod tests {
    use crate::day25::i64_to_snafu;
    use crate::utils;

    const SAMPLE: &str = "\
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122";

    #[test]
    fn part1() {
        assert_eq!("122", i64_to_snafu(37));
        assert_eq!("1=", i64_to_snafu(3));
        assert_eq!("12", i64_to_snafu(7));
        assert_eq!("1-12", i64_to_snafu(107));
        assert_eq!("12111", i64_to_snafu(906));
        assert_eq!("1=-0-2", i64_to_snafu(1747));
        assert_eq!("2=-1=0", super::part1(SAMPLE));
        assert_eq!(
            "2-10==12-122-=1-1-22",
            super::part1(utils::read_input("2022/day25").as_str())
        );
    }
}
