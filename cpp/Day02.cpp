#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <vector>

std::vector<std::string> get_input() {
    std::vector<std::string> input;
    std::string x;
    while (std::cin >> x) {
        input.emplace_back(x);
    }
    return input;
}

std::map<char, long> cardinalities(std::string const& s) {
    std::map<char, long> occurrences;
    for (auto x : s) {
        occurrences[x]++;
    }
    return occurrences;
}

long part1(std::vector<std::string> const& input) {
    auto n2 = 0L;
    auto n3 = 0L;

    for (auto const& x : input) {
        auto cards = cardinalities(x);
        auto exact = [&cards](long n) {
            auto predicate = [n](auto const& entry){ return entry.second == n; };
            auto found = std::find_if(cards.begin(), cards.end(), predicate);
            return found != cards.end();
        };
        if (exact(2)) { n2++; }
        if (exact(3)) { n3++; }
    }
    return n2 * n3;
}

std::string part2(std::vector<std::string> const& input) {

    // Iterate over all unique pairs of string elements
    for (auto p = input.begin(); p < input.end(); ++p) {
        for (auto q = p + 1; q < input.end(); ++q) {
            // Find first mismatched character
            auto res = std::mismatch(p->begin(), p->end(), q->begin());

            // Check that the strings weren't identical and that their
            // tails after the mismatch are equal
            if (res.first != p->end() &&
                std::equal(res.first + 1, p->end(), res.second + 1)) {

               return std::string(p->begin(), res.first) +
                      std::string(res.first + 1, p->end()); 
            }
        }
    }
    return "";
}

int main() {
    auto input = get_input();
    std::cout << "Part 1: " << part1(input) << std::endl;
    std::cout << "Part 2: " << part2(input) << std::endl;
}