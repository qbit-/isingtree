"""
This module implements functions to build Ising Hamiltonians
from the Treewidth problem formulations
"""
import re
import networkx as nx


def read_dgf_file(filename):
    """
    Reads graph from a DGF file
    """
    graph = nx.Graph()
    with open(filename, 'r+') as datafile:
        # search for the header line
        comment_patt = re.compile('^(\s*c\s+)(?P<comment>.*)')
        header_patt = re.compile(
            '^(\s*p\s+)(?P<file_type>cnf|tw)?(?P<n_nodes>\d+)\s+(?P<n_edges>\d+)')
        for n, line in enumerate(datafile):
            m = re.search(comment_patt, line)
            if m is not None:
                continue
            m = re.search(header_patt, line)
            if m is not None:
                n_nodes = int(m.group('n_nodes'))
                n_edges = int(m.group('n_edges'))
                break
            else:
                raise ValueError(f'File format error at line {n}:\n'
                                 f' expected pattern: {header_patt}')
        # search for the edges
        edge_patt = re.compile('^(\s*e\s+)(?P<u>\d+)\s+(?P<v>\d+)')
        for nn, line in enumerate(datafile, n):
            m = re.search(comment_patt, line)
            if m is not None:
                continue
            m = re.search(edge_patt, line)
            if m is None:
                raise ValueError(f'File format error at line {nn}:\n'
                                 f' expected pattern: {edge_patt}')
            graph.add_edge(m.group('u'), m.group('v'))
    if (graph.number_of_edges() != n_edges
       or graph.number_of_nodes() != n_nodes):
        raise ValueError('Header states:\n'
                         f' n_nodes = {n_nodes}, n_edges = {n_edges}\n'
                         'Got graph:\n'
                         f' n_nodes = {graph.number_of_nodes()},'
                         f' n_edges = {graph.number_of_edges()}\n')

    return graph


def read_cnf_file(filename, read_as='graph'):
    """
    Reads a CNF or a WCNF file and returns a graph.
    What this graph is depends on the value  of the 'read_as'
    argument. Possible values are:
    'graph': variables are interpreted as nodes and clauses as cliques
    'sat_mis': clauses are interpreted as cliques and edges are added
               between all variables X in some clauses and their
               negation *X in other clauses.
               The maximal independent set of this graph encodes the
               solution of the SAT problem
    'sat_clique': clauses are interpreted as cliques and edges are added
               between all variables X in different clauses.
               The maximal clique of this graph encodes the
               solution of the SAT problem
    Parameters
    ----------
    filename: str
              File to read
    read_as: str, default 'graph'
              how to interpret the CNF file, described above
    """
    allowed_values = {'graph', 'sat_mis', 'sat_clique'}
    if read_as not in allowed_values:
        raise ValueError(f'Wrong argument: {read_as}.'
                         f' Allowed_values: {allowed_values}')
    graph = nx.Graph()
    with open(filename, 'r+') as datafile:
        # search for the header line
        comment_patt = re.compile(
            '^(\s*c\s+)(?P<comment>.*)')
        header_patt = re.compile(
            '^(\s*p\s+)(?P<file_type>cnf|wcnf)\s+(?P<n_vars>\d+)\s+(?P<n_clauses>\d+)?(\s+(?P<n_soft>\d+))\s*$')
        for n, line in enumerate(datafile):
            m = re.search(comment_patt, line)
            if m is not None:
                continue
            m = re.search(header_patt, line)
            if m is not None:
                n_vars = int(m.group('n_vars'))
                n_clauses = int(m.group('n_clauses'))
                break
            else:
                raise ValueError(f'File format error at line {n}:\n'
                                 f' expected pattern: {header_patt}')
        print(n_vars, n_clauses)


g = read_dgf_file('SAT-BasedTW/ExampleGraphs/mildew.dgf')
