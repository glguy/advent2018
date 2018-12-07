//! Solution to https://adventofcode.com/2018/day/1

use std::collections::HashSet;
use std::io::{self, BufRead};

/// Print solution to part 1 and part 2 given the input on stdin
fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

/// Parse stdin as lines of integers
fn get_input() -> Vec<i64> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}

/// Compute sum of list of deltas
fn part1(deltas: &[i64]) -> i64 {
    deltas.iter().sum()
}

/// Find first duplicate in partial sums of cycled deltas
fn part2(deltas: &[i64]) -> i64 {
    fn psum(acc: &mut i64, x: &i64) -> Option<i64> {
        let res = *acc;
        *acc += x;
        Some(res)
    }

    let mut seen = HashSet::new();
    deltas
        .iter()
        .cycle() // repeat the list over and over
        .scan(0, psum) // compute partial sums
        .find(|&x| !seen.insert(x)) // find first duplicate
        .unwrap() // we'll either loop forever or succeed
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&vec![1, -2, 3, 1]), 3);
        assert_eq!(part1(&vec![1, 1, 1]), 3);
        assert_eq!(part1(&vec![1, 1, -2]), 0);
        assert_eq!(part1(&vec![-1, -2, -3]), -6);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&vec![1, -2, 3, 1]), 2);
    }
}
