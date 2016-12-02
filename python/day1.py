def new_heading(prev_heading, dir):
    return {
        (None, 'R'): 'E',
        ('N', 'R'): 'E',
        ('S', 'R'): 'W',
        ('E', 'R'): 'S',
        ('W', 'R'): 'N',
        (None, 'L'): 'W',
        ('N', 'L'): 'W',
        ('S', 'L'): 'E',
        ('E', 'L'): 'N',
        ('W', 'L'): 'S'}[(prev_heading, dir)]


def walk(heading, count, (x, y)):
    if heading == 'N': return (0, -count)
    elif heading == 'S': return (0, +count)
    elif heading == 'W': return (-count, 0)
    elif heading == 'E': return (+count, 0)
    raise NotImplementedError()


def transform_directions(s):
    return [(x[0], int(x[1:])) for x in s.split(', ')]


def update_seen(seen, (ox, oy), (dx, dy)):
    # Fill in the intermediate positions between a start and an end.
    for i in xrange(0, dx, -1 if dx < 0 else 1):
        pos = (ox + i, oy)
        if pos in seen:
            return pos
        seen.append(pos)
    for i in xrange(0, dy, -1 if dy < 0 else 1):
        pos = (ox, oy + i)
        if pos in seen:
            return pos
        seen.append(pos)
    return None


def travel(directions):
    pos = (0, 0)
    heading = None
    seen = []
    for step, (dir, count) in enumerate(transform_directions(directions), 1):
        #old_pos = pos
        heading = new_heading(heading, dir)
        paces = walk(heading, count, pos)
        seen_pos = update_seen(seen, pos, paces)
        if seen_pos is not None:
            #print 'twice!', seen_pos
            return seen_pos
        pos = (pos[0] + paces[0], pos[1] + paces[1])
        #print step, '{}{}'.format(dir, count), old_pos, '->', pos, heading
    return pos


def calculate_blocks(end, start=(0, 0)):
    return abs(end[0] - start[0]) + abs(end[1] - start[1])


example1 = 'R2, L3'
example2 = 'R2, R2, R2'
example3 = 'R5, L5, R5, R3'
example4 = 'R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1'
puzzle = 'R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1'


if __name__ == '__main__':
    print calculate_blocks(travel(puzzle))
