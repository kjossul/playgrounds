from collections import defaultdict


class Game:
    def __init__(self, s0, f, t, h=None, min_h=False, step_limit=None, hashfun=None, stop=True, save_path=False):
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
        :param stop: whether to terminate execution after first solution is found
        """
        self.h = h if h else Game.bfs
        self.min_h = min_h
        self.states = defaultdict(list, {self.h(s0, 0): [((s0,), 0)]})
        self.visited = set()
        self.f = f
        self.t = t
        self.step_limit = step_limit
        self.hashfun = hashfun if hashfun else self.hashfun
        self.stop = stop
        self.save_path = save_path

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
            except IndexError:
                del self.states[k]
                continue
            except ValueError:
                print(f"Seatch stopped. Total unique states: {len(self.visited)}.")
                return solutions

            s = ss[-1]
            h = self.hashfun(s)
            if h in self.visited or (self.step_limit and step > self.step_limit):
                continue
            else:
                self.visited.add(h)
            if self.t(s):
                if self.stop:
                    return ss, step
                else:
                    solutions.append((ss, step))
                    continue
            for new_s in self.f(s):
                new_path = ss + (new_s,) if self.save_path else (new_s,)
                self.states[self.h(new_s, step+1)].append((new_path, step+1))
