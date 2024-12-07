import networkx as nx


def list_to_path(ls):
    return '/'.join(ls)


if __name__ == '__main__':
    system = nx.DiGraph()
    current_dir = []
    system.add_node('/', dir=True, size=0)
    with open("07.txt") as f:
        for line in f.readlines():
            tokens = line.split()
            if tokens[0] == '$':
                if tokens[1] == 'cd':
                    if tokens[2] == '..':
                        e = current_dir.pop()
                        if e != '/':
                            system.nodes[list_to_path(current_dir)]['size'] += system.nodes[list_to_path(current_dir + [e])]['size']
                        else:
                            current_dir = ['/']
                    else:
                        current_dir.append(tokens[2])
            elif tokens[0] == 'dir':
                system.add_node(list_to_path(current_dir + [tokens[1]]), dir=True, size=0)
                system.add_edge(list_to_path(current_dir), list_to_path(current_dir + [tokens[1]]))
            else:
                size = int(tokens[0])
                system.add_node(list_to_path(current_dir + [tokens[1]]), dir=False, size=size)
                system.add_edge(list_to_path(current_dir), list_to_path(current_dir + [tokens[1]]))
                system.nodes[list_to_path(current_dir)]["size"] += size

    limit = 100_000
    print(sum(attrs['size'] for _, attrs in system.nodes(data=True) if attrs['dir'] and attrs['size'] <= limit))

    total_space = 70000000
    required_space = 30000000
    free_space = total_space - system.nodes['/']['size']
    target = required_space - free_space
    print(min(attrs['size'] for _, attrs in system.nodes(data=True) if attrs['dir'] and attrs['size'] >= target))
