# pylint: disable-all
def adj(x, y):
    return [
        (x - 1, y - 1),
        (x + 1, y - 1),
        (x - 1, y + 1),
        (x + 1, y + 1),
        (x, y - 1),
        (x, y + 1),
        (x - 1, y),
        (x + 1, y)]


def ringMax(n):
    return (2*n+1)**2


def ringSideSize(n):
    return 2*n+1


def turn(str):
    if str == "u":
        return "l"
    elif str == "l":
        return "d"
    elif str == "d":
        return "r"


def spiral():
    yield "r"
    ring = 1
    tc = 0
    while True:
        sc = 0
        cd = "u"
        while (sc < ringSideSize(ring) - 2):
            yield cd
            tc += 1
            sc += 1
        nsc = 1
        while (nsc < 4):
            sc = 0
            cd = turn(cd)
            while (sc < ringSideSize(ring) - 1):
                yield cd
                sc += 1
                tc += 1
            nsc += 1
        cd = "r"
        yield cd
        ring += 1


def spiralpos():
    x = 0
    y = 0
    it = spiral()
    yield (x, y)
    while True:
        n = next(it)
        if n == "r":
            x += 1
        elif n == "u":
            y += 1
        elif n == "l":
            x -= 1
        elif n == "d":
            y -= 1
        yield (x, y)


def sm(maxe):
    m = {}
    it = spiralpos()
    ec = 0
    while ec < maxe:
        p = next(it)
        if p == (0, 0):
            m[p] = 1
        else:
            p_adj = adj(p[0], p[1])
            v = 0
            for a in p_adj:
                v += m.get(a, 0)
            m[p] = v
            ec += 1
    return m
