import sys


def read_sides(line):
    return map(int, line.split())


def valid_triangle((a, b, c)):
    return a + b > c and b + c > a and a + c > b


if __name__ == '__main__':
    print len(filter(valid_triangle, map(read_sides, sys.stdin)))
