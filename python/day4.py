import string
import sys

alphabet = 'abcdefghijklmnopqrstuvwxyz'

example1 = 'aaaaa-bbb-z-y-x-123[abxyz]'


def frequencies(xs):
    freq = {}
    for x in xs:
        freq[x] = freq.setdefault(x, 0) + 1
    return freq


def combine_freqs(freqs):
    result = {}
    for freq in freqs:
        for key, count in freq.items():
            result[key] = result.setdefault(key, 0) + count
    return result


def split_parts(room):
    input, checksum = room.rstrip().split('[')
    checksum = checksum.rstrip(']')
    parts = input.split('-')
    sector = parts.pop()
    return parts, int(sector), list(checksum)


def is_real_room((letters, sector, checksum)):
    freqs = sorted(
        combine_freqs(map(frequencies, letters)).items(),
        key=lambda (k, v): (-v, k))
    keys = list(zip(*freqs)[0][:5])
    return keys == checksum


def rotate(s, n):
    n = n % 26
    tr = string.maketrans(alphabet + ' ', alphabet[n:] + alphabet[:n] + ' ')
    return s.translate(tr)


if __name__ == '__main__':
    real_rooms = filter(is_real_room, map(split_parts, sys.stdin))
    print sum(sector for _, sector, _ in real_rooms)
    for letters, sector, _ in real_rooms:
        print sector, rotate(' '.join(letters), sector)
