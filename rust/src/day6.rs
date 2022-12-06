fn detect_start_of_packet(window_size: usize, input: &str) -> usize {
    input
        .as_bytes()
        .windows(window_size)
        .position(|bytes| {
            let mut bitset = 0u32;
            for &b in bytes {
                bitset |= 1 << (b - 96);
            }
            bitset.count_ones() as usize == window_size
        })
        .unwrap()
        + window_size
}

pub fn part1(input: &str) -> usize {
    detect_start_of_packet(4, input)
}

pub fn part2(input: &str) -> usize {
    detect_start_of_packet(14, input)
}

mod tests {
    use crate::utils;

    #[test]
    fn part1() {
        let sample_1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
        let sample_2 = "bvwbjplbgvbhsrlpgdmjqwftvncz";
        let sample_3 = "nppdvjthqldpwncqszvftbrmjlhg";
        assert_eq!(7, super::part1(sample_1));
        assert_eq!(5, super::part1(sample_2));
        assert_eq!(6, super::part1(sample_3));
        assert_eq!(1658, super::part1(utils::read_input("2022/day6").as_str()));
    }

    #[test]
    fn part2() {
        let sample_1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
        let sample_2 = "bvwbjplbgvbhsrlpgdmjqwftvncz";
        let sample_3 = "nppdvjthqldpwncqszvftbrmjlhg";
        assert_eq!(19, super::part2(sample_1));
        assert_eq!(23, super::part2(sample_2));
        assert_eq!(23, super::part2(sample_3));
        assert_eq!(2260, super::part2(utils::read_input("2022/day6").as_str()));
    }
}
