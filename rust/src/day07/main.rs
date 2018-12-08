use std::io::{self, BufRead};
use scan_fmt::scan_fmt;

fn main() {
    let input = get_input();
    let tasks = Tasks::new(input);
    println!("Part 1: {}", part1(tasks.clone()));
    println!("Part 2: {}", part2(tasks));
}

fn part1(mut tasks: Tasks) -> String {
    let mut out: Vec<char> = vec![];

    while let Some(next) = tasks.next() {
        tasks.complete(next);
        out.push(next);
    }

    out.into_iter().collect()
}

fn part2(mut tasks: Tasks) -> u64 {
    let mut workers: Vec<(char, u64)> = vec![];
    let mut time = 0;

    loop {
        // Add workers until there's no work, or all workers are busy
        if workers.len() < 5 {
            if let Some(next) = tasks.next() {
                workers.push((next, cost(next)));
                continue
            }
        }

        // No more work to schedule, do some work
        if let Some(step) = workers.iter().map(|x| x.1).min() {
            time += step;
            for (task,t) in &mut workers {
                *t -= step;
                if *t == 0 { tasks.complete(*task) }
            }
            workers.retain(|&(_,n)| n > 0);
            continue
        }

        break time
    }
}

fn cost(c: char) -> u64 {
    c as u64 - 'A' as u64 + 61
}

fn get_input() -> Vec<(char, char)> {
    io::stdin().lock().lines().map(|x| parse_record(x.unwrap())).collect()
}

fn parse_record(record: String) -> (char, char) {
    let (x,y) = scan_fmt!(&record, "Step {[A-Z]} must be finished before step {[A-Z]} can begin.", char, char);
    (x.unwrap(), y.unwrap())
}

#[derive(Clone, Debug)]
struct Tasks {
    tasks: Vec<char>,
    deps: Vec<(char,char)>,
}

impl Tasks {
    fn new(deps: Vec<(char,char)>) -> Self {
        let mut tasks: Vec<char> = deps.iter().flat_map(|&(x,y)| vec![x,y]).collect();
        tasks.sort();
        tasks.dedup();
        Tasks { tasks, deps }
    }

    fn next(&mut self) -> Option<char> {
        match self.tasks.iter().position(|&x| self.task_ready(x)) {
            None => None,
            Some(p) => {
                let result = self.tasks[p];
                self.tasks.remove(p);
                Some(result)
            }
        }
    }

    fn complete(&mut self, task: char) {
        self.deps.retain(|&(x,_)| x != task)
    }

    fn task_ready(&self, task: char) -> bool {
        !self.deps.iter().any(|&(_,x)| x == task)
    }
}