#include <array>
#include <iostream>
#include <regex>
#include <string>
#include <vector>

struct Patch {
    long id, offX, offY, sizeX, sizeY;
};

Patch parse_patch(std::string const& str) {
    static const std::regex re("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)");
    std::smatch parts;
    std::regex_match(str, parts, re);

    Patch p;
    p.id = std::stol(parts[1].str());
    p.offX = std::stol(parts[2].str());
    p.offY = std::stol(parts[3].str());
    p.sizeX = std::stol(parts[4].str());
    p.sizeY = std::stol(parts[5].str());
    return p;
}

std::vector<Patch> get_input() {
    std::vector<Patch> patches;
    std::string line;
    while (std::getline(std::cin, line)) {
        auto patch = parse_patch(line);
        patches.push_back(patch);
    }
    return patches;
}

using Fabric = std::array<int[1000],1000>;

Fabric cut_patches(std::vector<Patch> const& patches) {
    Fabric fabric {};
    for (auto patch : patches) {
        for (long x = patch.offX; x < patch.offX + patch.sizeX; ++x) {
            for (long y = patch.offY; y < patch.offY + patch.sizeY; ++y) {
                fabric[x][y]++;
            }
        }
    }
    return fabric;
}

long part1(Fabric const& fabric) {
    long count = 0;
    for (auto const& row : fabric) {
        for (auto cell : row) {
            if (cell > 1) count++;
        }
    }
    return count;
}

long part2(std::vector<Patch> const& patches, Fabric const& fabric) {
    for (auto patch : patches) {
        for (long x = patch.offX; x < patch.offX + patch.sizeX; ++x) {
            for (long y = patch.offY; y < patch.offY + patch.sizeY; ++y) {
                if (fabric[x][y] > 1) {
                    goto skip;
                }
            }
        }
        return patch.id;
        skip:;
    }
    return -1;
}

int main () {
    auto patches = get_input();
    auto fabric = cut_patches(patches);
    std::cout << "Part 1: " << part1(fabric) << std::endl;
    std::cout << "Part 2: " << part2(patches, fabric) << std::endl;
}
