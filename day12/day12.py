import networkx as nx
import numpy as np


def parse_line(line):
    return [ord(c) for c in list(line)]


def generate_edges(matrix):
    edges = []
    for i in range(matrix.shape[0]):
        for j in range(matrix.shape[1]):
            if (j + 1 < matrix.shape[1]) and (matrix[i, j] >= matrix[i, j + 1] - 1):
                edges.append(((i, j), (i, j + 1)))
            if (j - 1 >= 0) and (matrix[i, j] >= matrix[i, j - 1] - 1):
                edges.append(((i, j), (i, j - 1)))
            if (i + 1 < matrix.shape[0]) and (matrix[i, j] >= matrix[i + 1, j] - 1):
                edges.append(((i, j), (i + 1, j)))
            if (i - 1 >= 0) and (matrix[i, j] >= matrix[i - 1, j] - 1):
                edges.append(((i, j), (i - 1, j)))

    return edges


with open("input.txt", "r") as f:
    lines = f.readlines()

matrix = np.array([parse_line(line) for line in lines])

start_coord = np.where(matrix == ord("S"))
start_coord = (start_coord[0][0], start_coord[1][0])
end_coord = np.where(matrix == ord("E"))
end_coord = (end_coord[0][0], end_coord[1][0])

matrix[start_coord] = ord("a")
matrix[end_coord] = ord("z")

edges = generate_edges(matrix)
G = nx.DiGraph()
G.add_edges_from(edges)

part1 = nx.shortest_path_length(G, start_coord, end_coord)
print(f"Question 1: {part1}")

possible_starts = list(zip(*np.where(matrix == ord("a"))))
shortest = np.inf
for start in possible_starts:
    try:
        path_len = nx.shortest_path_length(G, start, end_coord)
        if path_len < shortest:
            shortest = path_len
    except nx.NetworkXNoPath:
        pass

print(f"Question 2: {shortest}")
