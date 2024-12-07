import networkx as nx
from matplotlib import pyplot as plt


def calc_visibility(G, n):
    for i in range(n):
        tallest = [-1] * 4
        for j in range(n):
            tallest[0] = assign_visibility(G, i, j, tallest[0])
            tallest[1] = assign_visibility(G, i, n - 1 - j, tallest[1])
            tallest[2] = assign_visibility(G, j, i, tallest[2])
            tallest[3] = assign_visibility(G, n - 1 - j, i, tallest[3])


def calc_max_scenic_score(G, n):
    max_score = 0
    for x, y in G.nodes:
        views = [0] * 4
        height = G.nodes[(x, y)]['height']
        for i in range(x - 1, -1, -1):
            views[0] += 1
            if G.nodes[(i, y)]['height'] >= height:
                break
        for i in range(x + 1, n):
            views[1] += 1
            if G.nodes[(i, y)]['height'] >= height:
                break
        for j in range(y - 1, -1, -1):
            views[2] += 1
            if G.nodes[(x, j)]['height'] >= height:
                break
        for j in range(y + 1, n):
            views[3] += 1
            if G.nodes[(x, j)]['height'] >= height:
                break
        max_score = max(max_score, views[0] * views[1] * views[2] * views[3])
    return max_score


def assign_visibility(G, i, j, tallest):
    height = G.nodes[(i, j)]['height']
    if height > tallest:
        G.nodes[(i, j)]['visible'] = True
        tallest = height
    return tallest


if __name__ == '__main__':
    with open("08.txt") as f:
        m = [[int(c) for c in line.strip()] for line in f.readlines()]
    n = len(m)
    G = nx.grid_2d_graph(n, n)
    for (i, j) in G.nodes():
        G.nodes[(i, j)]['height'] = m[i][j]

    calc_visibility(G, n)
    print(sum(attrs.get('visible', False) for _, attrs in G.nodes(data=True)))
    print(calc_max_scenic_score(G, n))
