fn main () {
    println!("Part 1: {:?}", part1()); 
    println!("Part 2: {:?}", part2()); 
}

fn part1() -> Vec<u8> {
    Recipies::new().skip(320851).take(10).collect()
}

fn part2() -> usize {
    // No repetition means I don't need to be clever in my matching algorithm
    let needle = [3,2,0,8,5,1];
    let mut cursor = 0;
    
    Recipies::new().position(|x| {
        if x == needle[cursor] {
            cursor += 1
        } else if x == needle[0] {
            cursor = 1
        } else {
            cursor = 0
        }
        cursor == needle.len()
    }).unwrap() - needle.len() + 1
}

struct Recipies {
    elf1: usize,
    elf2: usize,
    next_output: usize,
    entries: Vec<u8>,
}

impl Recipies {
    fn new() -> Self {
        Recipies {
            elf1: 0,
            elf2: 1,
            next_output: 0,
            entries: vec![3,7],
        }
    }
}

impl Iterator for Recipies {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {

        if self.next_output == self.entries.len() {
            let v1 = self.entries[self.elf1];
            let v2 = self.entries[self.elf2];
            let mut new_recipe = v1 + v2;

            // recipies can't get larger than 18 (two digits)
            if new_recipe > 9 {
                self.entries.push(new_recipe / 10);
                new_recipe -= 10;
            }
            self.entries.push(new_recipe);

            self.elf1 = (self.elf1 + v1 as usize + 1) % self.entries.len();
            self.elf2 = (self.elf2 + v2 as usize + 1) % self.entries.len();
        }

        let result = self.entries[self.next_output];
        self.next_output += 1;
        return Some(result)
    }
}