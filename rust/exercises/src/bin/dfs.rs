use petgraph::graph::node_index as n;
use petgraph::prelude::*;
use petgraph::visit::depth_first_search;
use petgraph::visit::{DfsEvent, Time};

fn main() {
    let gr: Graph<(), ()> = Graph::from_edges(&[(0, 1), (1, 2), (1, 1), (2, 1)]);
    let start = n(0);
    let mut back_edges = 0;
    let mut discover_time = 0;
    let mut v = Vec::from_slice(&[(0, 1), (1, 2), (1, 1), (2, 1)]);
    let mut current = Vec::new();
// Stop the search when there are no edges available
    let result = depth_first_search(&gr, Some(start), |event| {
        if let DfsEvent::TreeEdge(u, v) = event {

        }
        current.pop();
    });

}
