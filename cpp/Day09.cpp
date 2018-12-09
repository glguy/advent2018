#include <deque>
#include <iostream>
#include <vector>

template <class E>
inline void clock(std::deque<E> *d, unsigned n) {
    for (unsigned i = 0; i < n; i++) {
        d->push_back((*d)[0]);
        d->pop_front();
    }
}

template <class E>
inline void counterclock(std::deque<E> *d, unsigned n) {
    for (unsigned i = 0; i < n; i++) {
        d->push_front((*d)[d->size() - 1]);
        d->pop_back();
    }
}

long game(long players, long marbles) {

    std::vector<long> scores(players);
    std::deque<long> circle { 0 };
    
    for (long m = 1; m <= marbles; m++) {
        if (m % 23) {
            clock(&circle, 2);
            circle.push_front(m);
        } else {
            counterclock(&circle, 7);
            scores[m % players] += m + circle[0];
            circle.pop_front();
        }
    }

    return *std::max_element(scores.begin(), scores.end());
}

int main() {
    std::cout << "Part 1: " << game(405, 70953) << std::endl;
    std::cout << "Part 2: " << game(405, 100*70953) << std::endl;
}
