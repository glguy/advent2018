#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <functional>

using metatype = unsigned long;

struct Tree {
    std::vector<Tree> children;
    std::vector<metatype> metadata;
};

template <class E>
inline void populate(std::istream &in, std::vector<E> &v, size_t n) {
    v.resize(n);
    std::for_each(v.begin(), v.end(), [&in](auto &x) { in >> x; });
}

std::istream &operator>>(std::istream &in, Tree& tree) {
    size_t n_children, n_metadata;
    in >> n_children >> n_metadata;
    populate(in, tree.children, n_children);
    populate(in, tree.metadata, n_metadata);
    return in;
}

metatype part1(Tree const& tree) {
    auto metadata_sum = std::reduce(tree.metadata.begin(), tree.metadata.end());
    auto children_sum = std::transform_reduce(tree.children.begin(), tree.children.end(),
                        0, std::plus(), part1);
    return metadata_sum + children_sum;
}

metatype part2(Tree const& tree) {
    if (tree.children.empty()) {
        return std::reduce(tree.metadata.begin(), tree.metadata.end());
    } else {
        return std::transform_reduce(tree.metadata.begin(), tree.metadata.end(),
                0, std::plus(), // sum
                [&tree](auto i) -> metatype {
                    if (1 <= i && i <= tree.children.size()) {
                        return part2(tree.children[i-1]);
                    } else {
                        return 0;
                    }
                });
    }
}

int main() {
    Tree tree;
    std::cin >> tree;
    std::cout << "Part 1: " << part1(tree) << std::endl;
    std::cout << "Part 2: " << part2(tree) << std::endl;
}