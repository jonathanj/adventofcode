def frequencies(xs):
    freq = {}
    for x in xs:
        freq[x] = freq.setdefault(x, 0) + 1
    return freq
