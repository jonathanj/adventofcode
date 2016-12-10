import re
import sys
from functools import partial
from itertools import chain

hypernets = re.compile(r'\[([^]]*?)\]').findall
supernets = re.compile(r'\[[^]]*?\]').split


def pieces_of(n, s):
    return [s[i:i + n] for i in xrange(len(s) - n + 1)]


def mapcat(f, xs):
    return list(chain.from_iterable(map(f, xs)))


def is_palindrome(s):
    return s[0] == s[-1] and s[0] != s[1]


def is_abba(s):
    sn = mapcat(partial(pieces_of, 4), supernets(s))
    hn = mapcat(partial(pieces_of, 4), hypernets(s))
    return filter(is_palindrome, sn) and not filter(is_palindrome, hn)


def solution1(data):
    return len(filter(is_abba, data))


def is_aba_bab(s):
    sn = mapcat(partial(pieces_of, 3), supernets(s))
    hn = mapcat(partial(pieces_of, 3), hypernets(s))
    for aba in filter(is_palindrome, sn):
        bab = aba[1] + aba[0] + aba[1]
        if bab in hn:
            return True


def solution2(data):
    return len(filter(is_aba_bab, data))


examples1 = ["abba[mnop]qrst",
             "abcd[bddb]xyyx",
             "aaaa[qwer]tyui",
             "ioxxoj[asdfgh]zxcvbn"]

examples2 = ["aba[bab]xyz",
             "xyx[xyx]xyx",
             "aaa[kek]eke",
             "zazbz[bzb]cdb"]

if __name__ == '__main__':
    puzzle = list(sys.stdin)
    print solution1(puzzle)
    print solution2(puzzle)
