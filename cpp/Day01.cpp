#include <iostream>
#include <numeric>
#include <set>
#include <vector>

using input_vec = std::vector<long>;

long part1(input_vec const& input) {
    return std::reduce(input.begin(), input.end());
}

long part2(input_vec const& input) {
    auto acc = 0L;
    std::set<long> seen;

    for(;;) {
        for (auto x : input) {
            auto added = seen.insert(acc).second;
            if (!added) {
                return acc;
            }
            acc += x;
        }
    }
}

input_vec get_input() {
    input_vec input;
    long x;
    while (std::cin >> x) {
        input.push_back(x);
    }
    return input;
}

int main() {
    auto input = get_input();
    std::cout << part1(input) << std::endl;
    std::cout << part2(input) << std::endl;
}
