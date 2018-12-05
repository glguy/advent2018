#include <algorithm>
#include <iostream>
#include <limits>
#include <string>

// Compute the lower-case version of a letter
inline char to_lower(char x) {
    return x - 'A' + 'a';
}

inline bool matches(char x, char y) {
    return to_lower(x) == y || to_lower(y) == x;
}

std::string simplify(std::string const& input) {
    std::string output;
    for (auto x : input) {
        if (!output.empty() && matches(output[output.size()-1], x)) {
            output.pop_back();
        } else {
            output.push_back(x);
        }
    }
    return output;
}

size_t part1(std::string const& input) {
    return simplify(input).size();
}

size_t part2(std::string const& input) {
    
    auto const initial = simplify(input);
    size_t best = std::numeric_limits<size_t>::max();

    for (char c = 'A'; c <= 'Z'; c++) {

        std::string candidate;
        for (auto x : input) {
            if (x != c && x != to_lower(c)) {
                candidate.push_back(x);
            }
        }

        best = std::min(best, simplify(candidate).size());
    }
    return best;
}

int main() {
    std::string input;
    std::getline(std::cin, input);
    std::cout << "Part 1: " << part1(input) << std::endl;
    std::cout << "Part 2: " << part2(input) << std::endl;
}