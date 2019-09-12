"""
This module implements functions to build Ising Hamiltonians
from the Treewidth problem formulations
"""
import networkx as nx
import matplotlib.pyplot as plt

import graph_model as gm
import sat


clauses, info = sat.read_cnf_file('test1.wcnf')
graph = sat.clauses_to_graph(
    clauses, read_as='sat_mis',
    formula_format=info['formula_format'], n_hard=info['n_hard'])


peo1, tw1 = gm.get_peo(graph, mathod='tamaki')

gm.draw_graph(graph)
# plt.show()

graph = sat.clauses_to_graph(
    clauses, read_as='sat_clique',
    formula_format=info['formula_format'], n_hard=info['n_hard'])


peo2, tw2 = gm.get_peo(
    graph, method='tamaki')

# n_cliques = nx.graph_clique_number(graph)
cliques = nx.find_cliques(graph)
some_clique = next(cliques)
clique_size = len(some_clique)
