"""
This file implements functions related to SAT
The simple SAT is stored as a nested list of integers
starting from 1, like

[[-1, 2, 3], [-2, 1, 4], ...]
"""
import numpy as np
import networkx as nx
import itertools as it
import lzma
from io import StringIO
import re


def rand_clause_generator(n_variables, n_clauses):
    """
    Generates a CNF from uniform distribution. The
    CNF is generates as a list of lists

    Parameters
    ----------
    n_variables: number of variables
    n_clauses: number of clauses

    Returns
    -------
    list of lists
    """
    # Basic checks
    assert(n_variables > 2)

    clauses = []
    for nn in range(n_clauses):
        while True:
            clause = np.random.randint(1, n_variables+1, size=3)
            if len(set(clause)) == len(clause):
                break
        signs = (-1)**np.random.binomial(1, 0.5, 3)
        clauses.append((clause * signs).tolist())
    return clauses


def generate_cnf_file(clauses, filename='', compressed=False):
    """
    Transforms a list of clauses to a CNF file.
    If filename is provided the file is written,
    otherwise the file will be returned as string

    Parameters
    ----------
    clauses: list of lists of int
    filename: str, default ''

    Returns
    -------
    str or None
    """
    import sys

    ENCODING = sys.getdefaultencoding()

    # first count variables
    n_variables = len(set(abs(item) for sublist in clauses
                          for item in sublist))
    n_clauses = len(clauses)
    data = "c outputting clauses as cnf\n"
    data = f'p cnf {n_variables} {n_clauses}\n'

    for clause in clauses:
        data += ''.join('{:d} '.format(n) for n in clause) + '0\n'

    if compressed:
        data = lzma.compress(data.encode(ENCODING))

    if len(filename) > 0:
        with open(filename, 'w+') as fp:
            fp.write(data)
        return None
    else:
        return data


def clauses_to_graph(clauses, read_as='sat_mis', formula_format='cnf',
                     n_hard=None):
    """
    Takes a CNF or a WCNF formula and returns a graph.
    What this graph is depends on the value  of the 'read_as'
    argument. Possible values are:

    'sat_mis': clauses are interpreted as cliques and edges are added
           between all variables X in some clauses and their
           negation *X in other clauses.
           The maximal independent set of this graph encodes the
           solution of the MIS problem
    'sat_clique': clauses are interpreted as cliques and edges are added
           between all variables X in different clauses.
           The maximal clique of this graph encodes the
           solution of the SAT problem
    Parameters
    ----------
    clauses: list of lists
             Formula in the format [[1, 2, 3], [-2, 4, 1], ...]
             (unweighted)
             or  [[1.0, [1, 2, 3]], [1.3, [-2, 4, 1]], ...]
             (weighted)
    read_as: bool
              How to interpret (W)CNF. See above
    formula_format: str, default 'cnf'
              Format of the CNF formula. Can be either 'cnf' or 'wcnf'
    n_hard: int, default None
              Number of hard clauses if a MaxSAT problem is considered
    """
    allowed_read_as_values = {'sat_mis', 'sat_clique'}
    if read_as not in allowed_read_as_values:
        raise ValueError(f'Unknown option read_as: {read_as}, '
                         f'allowed_values: {allowed_read_as_values}')
    allowed_format_values = {'cnf', 'wcnf'}
    if formula_format not in allowed_format_values:
        raise ValueError(
            f'Unknown option formula_format: {formula_format}, '
            f'allowed_values: {allowed_format_values}')
    CNF = 1
    WCNF = 2

    graph = nx.Graph()

    data_type = CNF if formula_format == 'cnf' else WCNF
    n_clauses = len(clauses)

    all_vars = []
    if data_type == CNF:
        for clause in clauses:
            for var in clause:
                all_vars.append(var)
    else:
        for weighted_clause in clauses:
            for var in weighted_clause[1]:
                all_vars.append(var)

    n_vars = len(set(map(abs, all_vars)))
    if n_hard is None:
        n_hard = n_clauses

    # arrays holding the nodes of the current variable
    variables_nodes = {}

    # counter for the graph node number
    current_node = 0
    # sum of weights of soft clauses
    soft_weight_sum = 0

    # go over the clauses
    for n, clause in enumerate(clauses):
        if data_type == WCNF:
            weight = clause[0]
            clause_vars = clause[1]
        else:
            weight = 1.0
            clause_vars = clause

        # make new nodes for the graph
        new_nodes = list(range(current_node,
                               current_node+len(clause_vars)))
        current_node = current_node + len(clause_vars)
        for var, node in zip(clause_vars, new_nodes):
            try:
                variables_nodes[var].append(node)
            except KeyError:
                variables_nodes[var] = [node]
            graph.add_node(node, label=var)

        # We add a clique to each clause
        # and store weight in the edges.
        # Clique specific: we need weights to be stored in nodes
        if read_as == 'sat_clique':
            graph.add_nodes_from(new_nodes, weight=weight)
        else:
            graph.add_nodes_from(new_nodes)
        graph.add_edges_from(it.combinations(new_nodes, 2),
                             weight=weight)
        if n == 0:
            # we are at the first clause. Save maximal weight
            maximal_weight = weight
        elif n > n_hard - 1:
            soft_weight_sum += weight

    # Final check for content
    # Check weights correctness
    if soft_weight_sum > maximal_weight:
        raise ValueError(f'Sum of weights of soft clauses:'
                         f' {soft_weight_sum} > '
                         f'maximal weight: {maximal_weight}')
    # Check number of variables
    neg_vars = sorted(var for var in variables_nodes.keys() if var < 0)
    n_vars_read = len(set(map(abs, variables_nodes.keys())))
    if n_vars_read != n_vars:
        raise ValueError(f'Number of variables read: {n_vars_read}'
                         f' != number of variables in header: {n_vars}')

    # Finalize the graph building. Add additional edges. We build graph
    # as MIS first
    for var in neg_vars:
        nodes = variables_nodes[var]
        try:
            other_nodes = variables_nodes[-var]
        except KeyError:
            continue  # skip a variable without negation in CNF
        n_other_nodes = len(other_nodes)
        for node in nodes:
            graph.add_edges_from(list(
                zip([node]*n_other_nodes, other_nodes)),
                                 weight=maximal_weight)

    # Turn the graph into clique problem graph (if requested)
    # The solver has to find a clique with maximal node weight
    if read_as == 'sat_clique':
        attr = {}
        for node, data in graph.nodes(data=True):
            attr[node] = {'label': data['label'],
                          'weight': data['weight']}
        graph = nx.complement(graph)
        nx.set_node_attributes(graph, attr)

    return graph


def read_cnf_file(file_or_data,
                  as_data=False, compressed=False,
                  dgf2wcnf_faulty_sizes=False):
    """
    Reads clauses from a CNF or WCNF file.

    Parameters
    ----------
    file_or_data: str
             Name of the file with graph data or its contensts
    as_data: bool, default False
             If filedata should be interpreted as contents of the
             data file
    compressed : bool, default False
           if input file or data is compressed
    dgf2wcnf_faulty_sizes: bool, default False
           This option is for fixing a bug in the DGF2WCNF output
           Basically, the number of hard clauses is 1 more than
           is stated in the header, and the number of soft clauses
           is 1 less.
    """

    if as_data is False:
        if compressed:
            datafile = lzma.open(file_or_data, 'r')
        else:
            datafile = open(file_or_data, 'r+')
    else:
        if compressed:
            datafile = StringIO(lzma.decompress(file_or_data))
        else:
            datafile = StringIO(file_or_data)

    CNF = 1
    WCNF = 2

    info = {}
    comment_patt = re.compile('^(\s*c\s+)(?P<comment>.*)')
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
            file_type = CNF if m.group('file_type') == 'cnf' else WCNF
            info.update({'n_vars': n_vars, 'n_clauses': n_clauses,
                         'formula_format': m.group('file_type')})
            if m.group('n_soft') is None:
                n_hard = n_clauses
                info['n_hard'] = None  # to comply with clauses_to_graph
            else:
                n_soft = int(m.group('n_soft'))
                if dgf2wcnf_faulty_sizes:
                    n_soft = n_soft - 1  # to fix the bug in the sizes
                n_hard = n_clauses - n_soft
                info['n_hard'] = n_hard
                if n_soft > 0:
                    info['is_maxsat'] = True
                else:
                    info['is_maxsat'] = False
            break
        else:
            raise ValueError(f'File format error at line {n}:\n'
                             f' expected pattern: {header_patt}')

    if file_type == CNF:
        clause_patt = re.compile(
            '^(\s*)(?P<clause>(-?\d+ )*)(0)')
    else:
        clause_patt = re.compile(
            '^(\s*)(?P<weight>([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?))(?P<clause>( -?\d+)*)(\s+0)')

    # initialize clause array
    clauses = []

    all_variables = []
    for nn, line in zip(range(n, n+n_hard), datafile):
        m = re.search(clause_patt, line)
        if m is None:
            raise ValueError(f'File format error at line {nn}:\n'
                             f' expected pattern: {clause_patt}')
        clause_vars = list(map(int, m.group('clause').split()))
        all_variables += clause_vars

        if file_type == WCNF:
            clauses.append(
                (float(m.group('weight')), clause_vars)
            )
        else:
            clauses.append(clause_vars)

    # Additional soft clauses if MaxSAT
    if file_type == WCNF and n_clauses - n_hard > 0:
        # sum of weights of soft clauses
        soft_weight_sum = 0
        # weight of the last hard clause
        maximal_weight = float(m.group('weight'))

        for nnn, line in zip(range(
                nn, nn+n_clauses-n_hard), datafile):
            m = re.search(clause_patt, line)
            if m is None:
                raise ValueError(f'File format error at line {nnn}:\n'
                                 f' expected pattern: {clause_patt}')
            clause_vars = list(map(int, m.group('clause').split()))
            all_variables += clause_vars

            weight = float(m.group('weight'))
            clauses.append(
                (weight, clause_vars)
            )
            soft_weight_sum += weight

        # Check weights correctness
        if soft_weight_sum > maximal_weight:
            raise ValueError(f'Sum of weights of soft clauses:'
                             f' {soft_weight_sum} > '
                             f'maximal weight: {maximal_weight}')
    # Final check for content
    n_vars_read = len(set(map(abs, all_variables)))
    if n_vars_read != n_vars:
        raise ValueError(f'Number of variables read: {n_vars_read}'
                         f' != number of variables in header: {n_vars}')

    return clauses, info
