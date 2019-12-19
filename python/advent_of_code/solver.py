import sys
from collections import defaultdict


class Game:
    def __init__(self, s0, f, t, h=None, min_h=False, step_limit=None, hashfun=None,
                 save_path=False, solution_limit=None):
        """
        :param s0: initial state
        :param f: function that explores reachable states
        :param t: function that checks termination of a state
        :param h: heuristic function to select next state to explore (default: breadth-first search). The higher the
        integer value returned, the more priority is given to the state
        :param step_limit: if set, ignores states that require more steps than this value
        :param step: value associated with each state. It could be a simple counter, or directions to reach said state
        into a maze and so on.
        :param hashfun: hash function for each state (to compute already visited states)
        :param stop_condition: whether to terminate execution after first solution is found
        """
        self.h = h if h else Game.bfs
        self.min_h = min_h
        self.states = defaultdict(list, {self.h(s0, 0): [((s0,), 0)]})
        self.visited = set()
        self.f = f
        self.t = t
        self.step_limit = step_limit
        self.hashfun = hashfun if hashfun else self.hashfun
        self.solution_limit = solution_limit
        self.save_path = save_path
        self.active_nodes = 1
        self.total_visited = 0

    @staticmethod
    def bfs(state, step):
        return -step

    @staticmethod
    def dfs(state, step):
        return step

    def hashfun(self, state):
        return state

    def solve(self):
        solutions = []
        while True:
            try:
                k = min(self.states) if self.min_h else max(self.states)
                frontier = self.states[k]
                ss, step = frontier.pop(0)
                self.active_nodes -= 1
            except IndexError:
                del self.states[k]
                continue
            except ValueError:
                print(f"\nNo more states to visit.")
                print(f"Search stopped. Total unique states: {len(self.visited)}. {len(solutions)} solutions found")
                return solutions
            s = ss[-1]
            h = self.hashfun(s)
            sys.stdout.write('\r' + f"active: {self.active_nodes:6}, visited:{self.total_visited:6}, current h(s): {k}")
            if h in self.visited or (self.step_limit and step > self.step_limit):
                continue
            else:
                self.total_visited += 1
                self.visited.add(h)
            if self.t(s):
                solutions.append((ss, step))
                print(f"\nSolution found! {s}")
                if self.solution_limit is not None and self.solution_limit >= len(solutions):
                    print(f"Search stopped. Total unique states: {len(self.visited)}. {len(solutions)} solutions found")
                    return solutions
            else:
                for new_s in self.f(s):
                    new_path = ss + (new_s,) if self.save_path else (new_s,)
                    self.states[self.h(new_s, step + 1)].append((new_path, step + 1))
                    self.active_nodes += 1
