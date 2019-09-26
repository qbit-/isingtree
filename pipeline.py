"""
This module implements functions to build Ising Hamiltonians
from the Treewidth problem formulations
"""
import networkx as nx
import matplotlib.pyplot as plt

import graph_model as gm
import sat
import interfaces as api


if __name__ == '__main__':
    graph_initial = nx.grid_graph([4, 4])
    graph, inv_dict = gm.relabel_graph_nodes(
        graph_initial, dict(zip(
            graph_initial.nodes,
            range(0, graph_initial.number_of_nodes()))))

    peo1, tw1 = gm.get_upper_bound_peo(graph, method='min_fill')
    temp_filename_prefix = 'test1'
    gm.exporters.generate_dgf_file(
        graph, temp_filename_prefix+'.dgf')
    api.run_dgf2wcnf(temp_filename_prefix+'.dgf')
    clauses, info = sat.read_cnf_file(
        temp_filename_prefix+'.wcnf', dgf2wcnf_faulty_sizes=True)
    graph_sat_mis = sat.clauses_to_graph(
        clauses, read_as='sat_mis',
        formula_format=info['formula_format'], n_hard=info['n_hard']
    )
    peo2, tw2 = gm.get_upper_bound_peo(graph_sat_mis, method='min_degree')
    # gm.draw_graph(graph_sat_mis)
    # plt.show()
