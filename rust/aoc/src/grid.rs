use std::collections::HashMap;
use itertools::Itertools;
use petgraph::graphmap::UnGraphMap;

#[derive(Debug)]
pub struct Grid<T> {
    cells: HashMap<(isize, isize), T>,
}

impl<T> Grid<T> {
    pub fn new() -> Self {
        Self { cells: HashMap::new() }
    }

    pub fn from_maze_matrix<Matrix, Row>(matrix: Matrix, is_wall: fn(&T) -> bool) -> Self
        where Matrix: IntoIterator<Item=Row>,
              Row: IntoIterator<Item=T>, {
        let mut s = Self::new();
        for (y, row) in matrix.into_iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate().filter(|(_, t)| !is_wall(t)) {
                s.insert(x as isize, y as isize, cell);
            }
        }
        s
    }

    pub fn get_maze_graph(&self) -> UnGraphMap<(isize, isize), ()> {
        let mut graph = UnGraphMap::new();
        for &(x1, y1) in self.cells.keys() {
            graph.add_node((x1, y1));
            for (x2, y2) in self.adjacent_coords(x1, y1) {
                graph.add_edge((x1, y1), (x2, y2), ());
            }
        }
        graph
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

    pub fn adjacent_coords(&self, x: isize, y: isize) -> Vec<(isize, isize)> {
        // like neighbors but doesn't count diagonals
        let offsets = [(1, 0), (0, -1), (-1, 0), (0, 1)];
        offsets.iter()
            .map(|(i, j)| (x + i, y + j))
            .filter(|t| self.cells.contains_key(t))
            .collect()
    }
}