import hashlib
from itertools import izip, repeat, count, islice, ifilter, imap

example1 = 'abc'
puzzle = 'ffykfhsq'


def hash((key, n)):
    return hashlib.md5('{}{}'.format(key, n)).hexdigest()


def is_zero_hash(hash):
    return hash.startswith('00000')


zero_hashes = ifilter(is_zero_hash, imap(hash, izip(repeat(puzzle), count())))


def solution1():
    return ''.join(islice(imap(lambda h: h[5], zero_hashes), 8))


def solution2():
    def sols(it):
        result = [None] * 8
        for h in it:
            n = int(h[5], 16)
            if n < 8 and result[n] is None:
                result[n] = h[6]
            if None not in result:
                return result
    return ''.join(sols(zero_hashes))


if __name__ == '__main__':
    #print solution1()
    print solution2()
