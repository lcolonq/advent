import networkx as nx
with open("input.txt", "r") as f:
    inp = f.read()
splits = [x.split("-") for x in inp.split("\n") if x]
conns = [(sp[0], sp[1]) for sp in splits]
g = nx.Graph()
g.add_edges_from(conns)
p1 = 0
for c in nx.enumerate_all_cliques(g):
    if len(c) == 3 and len([True for x in c if x[0] == 't']) > 0:
        p1 += 1
print(p1)
m = []
for c in nx.find_cliques(g):
    if len(c) > len(m):
        m = c
m.sort()
print(",".join(m))
