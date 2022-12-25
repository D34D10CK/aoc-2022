from collections import deque


def decrypt(sequence, rounds):
    mixed_sequence = deque(sequence)
    indexes = deque(range(len(sequence)))
    for _ in range(rounds):
        for i, x in enumerate(sequence):
            idx = indexes.index(i)
            mixed_sequence.rotate(-idx)
            indexes.rotate(-idx)

            mixed_sequence.rotate(-mixed_sequence.popleft())
            indexes.remove(i)
            indexes.rotate(-x)

            mixed_sequence.appendleft(x)
            indexes.appendleft(i)

    idx_0 = mixed_sequence.index(0)
    coordinates = sum(
        [mixed_sequence[(idx_0 + i) % len(sequence)] for i in [1000, 2000, 3000]]
    )
    return coordinates


def encode_sequence(sequence, key):
    return [i * key for i in sequence]


with open("input.txt", "r") as f:
    original_sequence = [int(l) for l in f.readlines()]

encoded_sequence = encode_sequence(original_sequence, 1)
coordinates = decrypt(encoded_sequence, 1)
print("Question 1:", coordinates)

encoded_sequence = encode_sequence(original_sequence, 811589153)
coordinates = decrypt(encoded_sequence, 10)
print("Question 2:", coordinates)
