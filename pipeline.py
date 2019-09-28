"""
This module implements functions to build Ising Hamiltonians
from the Treewidth problem formulations
"""
import numpy as np
import networkx as nx
import copy
import matplotlib.pyplot as plt

import graph_model as gm
import sat
import interfaces as api


def get_mis_hamiltonian(graph_old, A=1, B=None, use_node_weights=True):
    """
    Builds a Maximal Independent Set hamiltonian according to
    'Ising formulations of many NP problems' by Andrew Lucas
    The eigenvector with n * B energy corresponds to the solution
    with n elements.

    Parameters
    ----------
    graph: networkx.Graph
           graph of the problem
    A: float, default 1.0
           Coupling constant to enforce that the selected
           vertices are not adjacent. If node weights are used this
           is a scaling factor
    B: float, default None
           Coupling constant enforcing the minimal size of the
           independent set. We should enforce B < A to have a
           correct solution, and hence B = 0.5 * A by default.
           If node weights are used this is a scaling factor
    use_node_weights: bool, default True
           Use weights of nodes. Weights on edges are maximum
           of the node weights
    Returns
    -------
    hamiltonian: np.array
           Hamiltonian of the problem.
    var_to_node_dict: dict
           mapping from the graph vertices to the variables
           of the Hamiltonian
    """
    graph = copy.deepcopy(graph_old)
    min_edge_weight = 0
    if use_node_weights:
        for edge in graph.edges():
            u, v = edge
            weight_u = graph.nodes[u]['weight']
            weight_v = graph.nodes[v]['weight']
            graph.edges[(u, v)]['weight'] = max(weight_u, weight_v)
            min_edge_weight = min(max(weight_u, weight_v), min_edge_weight)

    # take the adjacency matrix. The order of nodes is produced
    # by graph.nodes(). The adjacency matrix may contain weights
    # (of edges).
    hamiltonian = A * nx.convert_matrix.to_numpy_matrix(graph)
    n_nodes = graph.number_of_nodes()

    var_to_node_dict = {num: node for num, node
                        in enumerate(graph.nodes())}

    if B is None:
        # we have to ensure that B is less than the minimal constraint
        # for excluding adjacent node selection
        B = 0.5 * A

    if use_node_weights:
        hamiltonian += - B * np.eye(n_nodes) * min_edge_weight
    else:
        hamiltonian += - B * np.eye(n_nodes)

    return hamiltonian, var_to_node_dict


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
