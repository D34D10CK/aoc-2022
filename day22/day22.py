import re

with open("input2.txt", "r") as f:
    text = f.read().strip()

directions = []
while text:
    if text.startswith("L") or text.startswith("R"):
        directions.append(text[0])
        text = text[1:]
    else:
        m = re.match(r"(\d+)", text)
        directions.append(int(m.groups()[0]))
        text = text[len(m.groups()[0]) :]

with open("input1.txt", "r") as f:
    lines = [l.rstrip() for l in f.readlines()]

walls = set()
grid = set()
for i, line in enumerate(lines):
    for j, c in enumerate(line):
        if c != " ":
            grid.add((j, i))
        if c == "#":
            walls.add((j, i))

y = 0
x = 10000
for x1, y1 in grid:
    if y1 == 0 and x1 < x:
        x = x1

orientation = 0
for direction in directions:
    if direction == "L":
        orientation = (orientation - 1) % 4
    elif direction == "R":
        orientation = (orientation + 1) % 4
    else:
        for _ in range(direction):
            if orientation == 0:  # right
                if (x + 1, y) in grid:
                    if (x + 1, y) not in walls:
                        x = x + 1
                    else:
                        break
                else:
                    min_x = 10000
                    for x1, y1 in grid:
                        if y1 == y and x1 < min_x:
                            min_x = x1

                    if (min_x, y) not in walls:
                        x = min_x
                    else:
                        break
            if orientation == 1:  # down
                if (x, y + 1) in grid:
                    if (x, y + 1) not in walls:
                        y = y + 1
                    else:
                        break
                else:
                    min_y = 10000
                    for x1, y1 in grid:
                        if x1 == x and y1 < min_y:
                            min_y = y1

                    if (x, min_y) not in walls:
                        y = min_y
                    else:
                        break
            if orientation == 2:  # left
                if (x - 1, y) in grid:
                    if (x - 1, y) not in walls:
                        x = x - 1
                    else:
                        break
                else:
                    max_x = 0
                    for x1, y1 in grid:
                        if y1 == y and x1 > max_x:
                            max_x = x1

                    if (max_x, y) not in walls:
                        x = max_x
                    else:
                        break
            if orientation == 3:  # up
                if (x, y - 1) in grid:
                    if (x, y - 1) not in walls:
                        y = y - 1
                    else:
                        break
                else:
                    max_y = 0
                    for x1, y1 in grid:
                        if x1 == x and y1 > max_y:
                            max_y = y1

                    if (x, max_y) not in walls:
                        y = max_y
                    else:
                        break

print("Question 1:", 1000 * (y + 1) + 4 * (x + 1) + orientation)
