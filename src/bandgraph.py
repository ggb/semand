import graphviz as gv

gr = gv.Graph(format='svg')

with open("band.pairs", "r", encoding="utf8") as f:
    for line in f:
        l = line.split(" ")
        gr.edge(l[0], l[1])

filename = gr.render(filename='img/g1')