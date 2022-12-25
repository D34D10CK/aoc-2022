import networkx as nx

with open("input.txt", "r") as f:
    lines = f.readlines()

coords = [tuple([int(i) for i in line.strip().split(",")]) for line in lines]

num_adjacent = 0
for x1, y1, z1 in coords:
    for x2, y2, z2 in coords:
        if x2 == (x1 + 1) and y2 == y1 and z2 == z1:
            num_adjacent += 1
        if x2 == x1 and y2 == (y1 + 1) and z2 == z1:
            num_adjacent += 1
        if x2 == x1 and y2 == y1 and z2 == (z1 + 1):
            num_adjacent += 1

print("Question 1:", len(coords) * 6 - num_adjacent * 2)

min_x = min(list(zip(*coords))[0])
max_x = max(list(zip(*coords))[0])
min_y = min(list(zip(*coords))[1])
max_y = max(list(zip(*coords))[1])
min_z = min(list(zip(*coords))[2])
max_z = max(list(zip(*coords))[2])

air_cubes = [
    (x, y, z)
    for x in range(min_x - 1, max_x + 2)
    for y in range(min_y - 1, max_y + 2)
    for z in range(min_z - 1, max_z + 2)
    if ((x, y, z) not in coords)
]

G = nx.Graph()
for x1, y1, z1 in air_cubes:
    for x2, y2, z2 in air_cubes:
        if x2 == (x1 + 1) and y2 == y1 and z2 == z1:
            G.add_edge((x1, y1, z1), (x2, y2, z2))
        if x2 == x1 and y2 == (y1 + 1) and z2 == z1:
            G.add_edge((x1, y1, z1), (x2, y2, z2))
        if x2 == x1 and y2 == y1 and z2 == (z1 + 1):
            G.add_edge((x1, y1, z1), (x2, y2, z2))

num_adjacent = 0
for x1, y1, z1 in next(nx.connected_components(G)):
    for x2, y2, z2 in coords:
        if x2 == (x1 + 1) and y2 == y1 and z2 == z1:
            num_adjacent += 1
        if x2 == x1 and y2 == (y1 + 1) and z2 == z1:
            num_adjacent += 1
        if x2 == x1 and y2 == y1 and z2 == (z1 + 1):
            num_adjacent += 1

print("Question 2:", num_adjacent * 2)
