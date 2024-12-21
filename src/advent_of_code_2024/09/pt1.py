test_input_raw = "2333133121414131402"

def parse_input(input_raw):
    nums = [int(c) for c in input_raw]
    file_index, ret = 0, []
    for (idx, n) in enumerate(nums):
        if (idx % 2 == 0):
            for i in range(n):
                ret.append(file_index)
            file_index += 1
        else:
            for i in range(n):
                ret.append(None)
    return ret

def find_next_free_block(disk, i):
    idx = i
    while (idx < len(disk) and disk[idx] != None):
        idx += 1
    if idx == len(disk):
        return None
    else:
        return idx

def find_prev_file_block(disk, j):
    idx = j
    while (idx >= 0 and disk[idx] == None):
        idx -= 1
    if idx < 0:
        return None
    else:
        return idx

def swap_blocks(disk, i, j):
    temp = disk[i]
    disk[i] = disk[j]
    disk[j] = temp

def compact_disk(input):
    i = find_next_free_block(input, 0)
    j = find_prev_file_block(input, len(input)-1)
    while (i < j):
        swap_blocks(input, i, j)
        i = find_next_free_block(input, i)
        j = find_prev_file_block(input, j)
        #print(i, j, input)
        # TODO: not sure if this line is correct
        if (i is None or j is None):
            return input
    return input

def solve_pt1(input):
    total = 0
    for (idx, id) in enumerate(input):
        if id is None:
            return total
        total += idx * id
    return total

if __name__ == '__main__':
    test_input = parse_input(test_input_raw)
    print(test_input)
    compact_disk(test_input)
    #print(test_input)
    pt1_soln = solve_pt1(test_input)
    print(pt1_soln)
