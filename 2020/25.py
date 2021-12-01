# Part A:
def transform(subject, loop_size):
    value = 1
    for _ in range(loop_size):
        value = (value * subject) % 20201227
    return value

def reverse_transform(subject, keys, max_size=100000000):
    assert len(keys) == 2
    loop_sizes = [None] * len(keys)
    value = 1
    for i in range(max_size):
        value = (value * subject) % 20201227
        if value == keys[0]:
            loop_sizes[0] = i+1
            if all(loop_sizes): break
        elif value == keys[1]:
            loop_sizes[1] = i+1
            if all(loop_sizes): break
    return loop_sizes

public_keys = [2069194, 16426071]
subject = 7
loop_sizes = reverse_transform(subject, public_keys)
assert all(loop_sizes)

encryption_keys = [transform(public_keys[0], loop_sizes[1]),
                   transform(public_keys[1], loop_sizes[0])]
encryption_key = encryption_keys[0]
assert encryption_keys[0] == encryption_keys[1]

print("Part A:", encryption_key)
