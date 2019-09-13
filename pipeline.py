"""
This module implements functions to build Ising Hamiltonians
from the Treewidth problem formulations
"""
import networkx as nx
import matplotlib.pyplot as plt
import subprocess

import graph_model as gm
import sat


def get_dgf2wcnf_path():
    """
    Set up DGF2WCNF path
    """
    import os
    # _dgf2wcnf_path = os.path.join(
    #     gm.system_defs.THIRDPARTY_PATH, 'dgf2wcnf', 'run_dgf2wcnf.sh')
    _dgf2wcnf_path = os.path.join(
        gm.system_defs.THIRDPARTY_PATH,
        'SAT-BasedTW', 'builds', 'DGF2WCNF_TW_MacOS')
    if os.path.isfile(_dgf2wcnf_path):
        DGF2WCNF_COMMAND = _dgf2wcnf_path
    else:
        DGF2WCNF_COMMAND = None
        raise FileNotFoundError('DGF2WCNF not found')
    return DGF2WCNF_COMMAND


def run_dgf2wcnf(
        dgf_filename, command=get_dgf2wcnf_path(), cwd=None):
    """
    Run DGF2WCNF_TW program

    Parameters
    ----------
    dgf_filename : str
         Path to the input DGF file
    command : str, optional
         DGF2WCNF command name
    cwd : str, default None
         Current work directory

    Returns
    -------
    None
    """
    if command is None:
        raise ValueError('No DGF2WCNF command is given.'
                         ' Did you install DGF2WCNF?')
    sh = command + f" {dgf_filename} "

    process = subprocess.Popen(
        sh.split(), stdout=subprocess.PIPE, cwd=cwd)
    output, error = process.communicate()
    if error:
        print(error)

    return


if __name__ == '__main__':
    graph_initial = nx.grid_graph([4, 4])
    graph, inv_dict = gm.relabel_graph_nodes(
        graph_initial, dict(zip(
            graph_initial.nodes,
            range(0, graph_initial.number_of_nodes()))))

    peo1, tw1 = gm.get_upper_bound_peo(graph, method='min_degree')
    temp_filename_prefix = 'test1'
    gm.exporters.generate_dgf_file(
        graph, temp_filename_prefix+'.dgf')
    run_dgf2wcnf(temp_filename_prefix+'.dgf')
    clauses, info = sat.read_cnf_file(
        temp_filename_prefix+'.wcnf', dgf2wcnf_faulty_sizes=True)
    graph_sat_mis = sat.clauses_to_graph(
        clauses, read_as='sat_mis',
        formula_format=info['formula_format'], n_hard=info['n_hard']
    )
    peo2, tw2 = gm.get_upper_bound_peo(graph_sat_mis, method='min_degree')
    # gm.draw_graph(graph_sat_mis)
    # plt.show()
