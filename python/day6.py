import sys
from functools import partial
from util import frequencies

example1 = ["eedadn", "drvtee", "eandsr", "raavrd", "atevrs", "tsrnev",
            "sdttsa", "rasrtv", "nssdts", "ntnada", "svetve", "tesnvt",
            "vntsnd", "vrdear", "dvrsen", "enarar"]


def ffreq(freqs, reverse):
    return sorted(freqs.items(), key=lambda (k, v): v, reverse=reverse)[0][0]


def solution1(input):
    return ''.join(
        map(partial(ffreq, reverse=True), map(frequencies, zip(*input))))


def solution2(input):
    return ''.join(
        map(partial(ffreq, reverse=False), map(frequencies, zip(*input))))


if __name__ == '__main__':
    input = list(sys.stdin)
    print solution1(input)
    print solution2(input)
