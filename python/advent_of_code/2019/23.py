import time
from threading import Thread

intcode = __import__("09")


class Network:
    def __init__(self, n, program):
        self.nodes = tuple(Node(program, i, self) for i in range(n))
        self.nat_msg = None

    def boot(self):
        for node in self.nodes:
            t = Thread(target=node.boot)
            t.start()
        nat = Thread(target=self.nat_routine)
        nat.start()

    def nat_routine(self):
        last_y = None
        while True:
            if not any(n.queue for n in self.nodes):
                print(f"Idle network: sending {self.nat_msg} to 0")
                x, y = self.nat_msg
                self.nodes[0].queue.extend(self.nat_msg)
                if last_y is not None and y == last_y:
                    print(y)
                    raise NotImplementedError
                last_y = y
            time.sleep(5)



class Node:
    def __init__(self, program, identifier, network):
        self.id = identifier
        self.program = program.copy()
        self.queue = [identifier]
        self.program.input_callback = self.input_callback
        self.program.output_callback = self.output_callback
        self.outputs = []
        self.network = network

    def input_callback(self):
        if not self.queue:
            return -1
        else:
            return self.queue.pop(0)

    def output_callback(self, v):
        self.outputs.append(v)
        if len(self.outputs) == 3:
            node, *msg = self.outputs
            print(f"{self.id} -> {node}: {msg}")
            if node == 255:
                self.network.nat_msg = msg
            else:
                self.network.nodes[node].queue.extend(msg)
            self.outputs.clear()

    def boot(self):
        self.program.execute()


if __name__ == '__main__':
    program = intcode.IntCode.from_file("23.txt")
    network = Network(50, program)
    network.boot()
