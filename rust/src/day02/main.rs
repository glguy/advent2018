use std::io::{self, BufRead};
use std::collections::HashMap;
use std::hash::Hash;

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

/// Parse stdin as lines of integers
fn get_input() -> Vec<String> {
    let stdin = io::stdin();
    let handle = stdin.lock();

    let mut v = vec![];
    for x in handle.lines() {
        v.push(x.unwrap());
    }
    v
}

fn cardinalities<F>(items: F) -> HashMap<F::Item, usize>
    where F: Iterator, F::Item: Eq + Hash {
    let mut counts = HashMap::new();
    for x in items {
        *counts.entry(x).or_insert(0) += 1
    }
    counts
}

fn part1(input: &[String]) -> usize {
    let mut n2 = 0;
    let mut n3 = 0;
    for x in input {
        let counts = cardinalities(x.chars());
        let exact = |n| counts.values().find(|&&x| x == n).is_some();
        if exact(2) {
            n2 += 1
        }
        if exact(3) {
            n3 += 1
        }
    }
    n2 * n3
}

fn off_by_one(x: &str, y: &str) -> Option<String> {
    // Compute String of common letters
    let candidate: String = x
        .chars()
        .zip(y.chars())
        .filter_map(|(x,y)| if x == y { Some(x) } else { None })
        .collect();

    // Return the candidate if it is one shorter than the original
    if candidate.len() + 1 == x.len() {
        Some(candidate)
    } else {
        None
    }
}

fn part2(input: &[String]) -> String {

    for (i,x) in input.iter().enumerate() {
        for y in &input[i+1 ..] {
            if let Some(shared) = off_by_one(x, y) {
                return shared
            }
        }
    }
    panic!("No solution to part 2")
}