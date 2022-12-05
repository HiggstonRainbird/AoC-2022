# Import the input file
with open('Day3Input.txt', 'r') as f:
    lines = f.read().splitlines()

# Solves part 1 of the Advent of Code problem.
def solve_day_3_1(input_list: list) -> int:
    total_priority = 0

    for rucksack in input_list:
        first_compartment = rucksack[:len(rucksack) // 2]
        second_compartment = rucksack[len(rucksack) // 2:]

        # Get the item types in both compartments
        common_item_types = set(first_compartment) & set(second_compartment)

        for item_type in common_item_types:
            # Calculate the priority of the item type
            if item_type.islower():
                priority = ord(item_type) - 96
            else:
                priority = ord(item_type) - 64 + 26

            total_priority += priority

    return total_priority

# Solves part 2 of the Advent of Code problem.
def solve_day_3_2(input_list: list) -> int:
    total_priority = 0

    # Iterate through the list in steps of 3
    for i in range(0, len(input_list), 3):
        # Get the three rucksacks
        first_rucksack = input_list[i]
        second_rucksack = input_list[i+1]
        third_rucksack = input_list[i+2]

        # Get the common item types between all three
        common_item_types = set(first_rucksack) & set(second_rucksack) & set(third_rucksack)

        # Calculate the priority of the item type
        for item_type in common_item_types:
            if item_type.islower():
                priority = ord(item_type) - 96
            else:
                priority = ord(item_type) - 64 + 26

            total_priority += priority

    return total_priority

print("Part 1 answer:", solve_day_3_1(lines))
print("Part 2 answer:", solve_day_3_2(lines))
