use std::iter;

const SERIAL: i64 = 8868;
const GRID_SIZE: usize = 300;
type Grid = [[i64; GRID_SIZE+1]; GRID_SIZE+1];

fn main() {
    let area = area_sum(SERIAL);
    println!("Part 1: {}", part1(&area));
    println!("Part 2: {}", part2(&area));
}

fn square(grid: &Grid, x: usize, y: usize, w: usize) -> i64 {
    grid[x+w-1][y+w-1]
    - grid[x-1][y+w-1]
    - grid[x+w-1][y-1]
    + grid[x-1][y-1]
}

/// Iterator over valid x or y coordinates for a particular square size
fn square_range(size: usize) -> impl Clone + Iterator<Item = usize> {
    1 ..= GRID_SIZE - (size - 1)
}

fn find_best<I: Iterator<Item = usize>>(grid: &Grid, sizes: I) -> (usize, usize, usize, i64) {
    sizes.flat_map(move |size|
        square_range(size).flat_map(move |x|
            square_range(size).map(move |y|
                (x, y, size, square(grid, x, y, size)))))
        .max_by_key(|x| x.3).unwrap()
}

fn part1(grid: &Grid) -> String {
    let (x,y,_,_) = find_best(grid, iter::once(3));
    format!("{},{}", x, y)
}

fn part2(grid: &Grid) -> String {
    let (x,y,s,_) = find_best(grid, 1 ..= GRID_SIZE);
    format!("{},{},{}", x,y,s)
}

fn power_level(serial: i64, x: usize, y: usize) -> i64 {
    let rack_id = x as i64 + 10;
    let mut level = rack_id * y as i64;
    level += serial;
    level *= rack_id;
    level = level / 100 % 10; // hundreds digit
    level - 5
}

fn area_sum(serial: i64) -> Grid {
    let mut area = [[0i64; 301]; 301];

    for x in 1 ..= GRID_SIZE {
        for y in 1 ..= GRID_SIZE {
            area[x][y] = power_level(serial, x, y)
                       + area[x-1][y]
                       + area[x][y-1]
                       - area[x-1][y-1];
        }
    }

    area
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn power_level_test() {
        assert_eq!(power_level( 8,   3,   5),  4);
        assert_eq!(power_level(57, 122,  79), -5);
        assert_eq!(power_level(39, 217, 196),  0);
        assert_eq!(power_level(71, 101, 153),  4);
    }

    #[test]
    fn test_18() {
        let grid = area_sum(18);
        assert_eq!(part1(&grid), "33,45");
        assert_eq!(part2(&grid), "90,269,16");
    }

    #[test]
    fn test_42() {
        let grid = area_sum(42);
        assert_eq!(part1(&grid), "21,61");
        assert_eq!(part2(&grid), "232,251,12");
    }
}