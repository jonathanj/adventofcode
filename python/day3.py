import sys
import itertools


def grouper(iterable, n, fillvalue=None):
    args = [iter(iterable)] * n
    return itertools.izip_longest(fillvalue=fillvalue, *args)


def transpose(xs):
    return zip(*xs)


def read_sides(line):
    return map(int, line.split())


def valid_triangle((a, b, c)):
    return a + b > c and b + c > a and a + c > b


if __name__ == '__main__':
    print len(filter(valid_triangle,
                     grouper(
                         itertools.chain.from_iterable(
                             transpose(map(read_sides, sys.stdin))), 3)))
