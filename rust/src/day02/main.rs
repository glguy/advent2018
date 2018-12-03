use std::collections::HashMap;
use std::hash::Hash;
use std::io::{self, BufRead};

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

/// Parse stdin as lines of integers
fn get_input() -> Vec<String> {
    let stdin = io::stdin();
    let handle = stdin.lock();
    let v = handle.lines().collect::<Result<_, _>>().unwrap();
    v
}

fn cardinalities<F>(items: F) -> HashMap<F::Item, usize>
where
    F: Iterator,
    F::Item: Eq + Hash,
{
    let mut counts = HashMap::new();
    for x in items {
        *counts.entry(x).or_insert(0) += 1
    }
    counts
}

fn part1<S: AsRef<str>>(input: &[S]) -> usize {
    let mut n2 = 0;
    let mut n3 = 0;
    for x in input {
        let counts = cardinalities(x.as_ref().chars());
        let exact = |n| counts.values().any(|&x| x == n);
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
        .filter_map(|(x, y)| if x == y { Some(x) } else { None })
        .collect();

    // Return the candidate if it is one shorter than the original
    if candidate.len() + 1 == x.len() {
        Some(candidate)
    } else {
        None
    }
}

fn part2<S: AsRef<str>>(input: &[S]) -> String {
    for (i, x) in input.iter().enumerate() {
        for y in &input[i + 1..] {
            if let Some(shared) = off_by_one(x.as_ref(), y.as_ref()) {
                return shared;
            }
        }
    }
    panic!("No solution to part 2")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let v = vec![
            "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab",
        ];
        assert_eq!(part1(&v), 12);
    }

    #[test]
    fn test_part2() {
        let v = vec![
            "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz",
        ];
        assert_eq!(part2(&v), "fgij");
    }
}
