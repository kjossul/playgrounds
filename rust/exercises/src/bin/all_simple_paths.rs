use petgraph::algo::all_simple_paths;
use petgraph::Graph;
use petgraph::graph::NodeIndex;

fn main() {
    let graph = make_graph();
    all_simple_paths::<Vec<NodeIndex>, _>(&graph, 0.into(), 3.into(), 0, None).count();
}

fn make_graph() -> Graph<(), i32> {
    Graph::<(), i32>::from_edges(&[
        (0, 1), (0, 2), (0, 3),
        (1, 2), (1, 3),
        (2, 3),
    ])
}
