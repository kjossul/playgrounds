use std::collections::HashMap;
use itertools::Itertools;

pub struct Grid<T> {
    cells: HashMap<(isize, isize), T>,
}

impl<T: Copy> Grid<T> {
    pub fn new() -> Self {
        Self { cells: HashMap::new() }
    }

    pub fn get(&self, x: isize, y: isize) -> Option<&T> {
        self.cells.get(&(x, y))
    }

    pub fn insert(&mut self, x: isize, y: isize, v: T) -> Option<T> {
        self.cells.insert((x, y), v)
    }

    pub fn neighbors(&self, x: isize, y: isize) -> Vec<&T> {
        let deltas = [-1, 0, 1];
        deltas.iter().cartesian_product(deltas.iter())
            .filter(|(&i, &j)| (i, j) != (0, 0))
            .flat_map(|(&i, &j)| self.get(x + i, y + j))
            .collect()
    }
}