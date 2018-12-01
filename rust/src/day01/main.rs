//! Solution to https://adventofcode.com/2018/day/1

use std::io::{self, BufRead};
use std::collections::HashSet;

/// Print solution to part 1 and part 2 given the input on stdin
fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

/// Parse stdin as lines of integers
fn get_input() -> Vec<i64>{
    let stdin = io::stdin();
    let handle = stdin.lock();

    let mut v: Vec<i64> = vec![];
    for x in handle.lines() {
        let n = x.unwrap().parse().unwrap();
        v.push(n);
    }
    v
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
        .cycle()                    // repeat the list over and over
        .scan(0, psum)              // compute partial sums
        .find(|&x| !seen.insert(x)) // find first duplicate
        .unwrap()                   // we'll either loop forever or succeed
}
