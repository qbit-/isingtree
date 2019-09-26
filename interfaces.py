"""
This module provides interfaces to run different thirdparty code
"""
import subprocess
from io import StringIO
import re
import sys
ENCODING = sys.getdefaultencoding()


def get_dgf2wcnf_path():
    """
    Set up DGF2WCNF path
    """
    import os
    from .graph_model.system_defs import THIRDPARTY_PATH

    # _dgf2wcnf_path = os.path.join(
    #     THIRDPARTY_PATH, 'dgf2wcnf', 'DGF2WCNF_TW_linux')
    _dgf2wcnf_path = os.path.join(
        THIRDPARTY_PATH, 'SAT-BasedTW',
        'builds', 'DGF2WCNF_TW_MacOS')
    if os.path.isfile(_dgf2wcnf_path):
        DGF2WCNF_COMMAND = _dgf2wcnf_path
    else:
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


def get_openwbo_path():
    """
    Set up OpenWBO path
    """
    import os
    from .graph_model.system_defs import THIRDPARTY_PATH

    _openwbo_path = os.path.join(
        THIRDPARTY_PATH, 'open-wbo', 'open-wbo')

    if os.path.isfile(_openwbo_path):
        OPENWBO_COMMAND = _openwbo_path
    else:
        OPENWBO_COMMAND = None
        raise FileNotFoundError('open-wbo not found')
    return OPENWBO_COMMAND


def run_openwbo(
        wcnf_filename, command=get_openwbo_path(), cwd=None):
    """
    Runs the OpenWBO MaxSAT solver and collects its output

    Parameters
    ----------
    wcnf_filename : str
         Path to the input WCNF file
    command : str, optional
         OpenWBO command name
    cwd : str, default None
         Current work directory

    Returns
    -------
    output: str
    """
    if command is None:
        raise ValueError('No open-wbo command is given.'
                         ' Did you install open-wbo?')
    sh = command + f" {wcnf_filename} "

    process = subprocess.Popen(
        sh.split(), stdout=subprocess.PIPE, cwd=cwd)
    output, error = process.communicate()
    if error:
        print(error)

    return output.decode(ENCODING)


def parse_openwbo_out(data):
    """
    Parses OpenWBO MaxSAT solver output

    Parameters
    ----------
    data : str
         Data collected from Open-WBO

    Returns
    -------
    solution : dict
         assignment of the variables
    info: dict
         information about the problem/solution
    """
    strings = StringIO(data)

    type_patt = re.compile(
        '^(\s*c\s+).*(Problem Type:)\s+(?P<weighted>\w*)')
    nvar_patt = re.compile(
        '^(\s*c\s+).*(Number of variables:)\s+(?P<n_vars>\d*)')
    nhard_patt = re.compile(
        '^(\s*c\s+).*(Number of hard clauses:)\s+(?P<n_hard>\d*)')
    nsoft_patt = re.compile(
        '^(\s*c\s+).*(Number of soft clauses:)\s+(?P<n_soft>\d*)')
    cost_patt = re.compile('^(\s*o\s+)(?P<cost>\d*)')
    status_patt = re.compile(
        '^(\s*s\s+)(?P<is_sat>SATISFIABLE|OPTIMUM FOUND|UNSATISFIABLE)')
    solution_patt = re.compile('^(\s*v)(?P<solution>( [-+]?\d+)*)')

    int_patterns = iter([nvar_patt, nhard_patt, nsoft_patt,
                         cost_patt])

    info = {}

    # check if weighted
    patt = type_patt
    for line in strings:
        m = re.search(patt, line)
        if m is None:
            continue
        else:
            info['weighted'] = (
                True if m.group('weighted') == 'Weighted' else False)
            patt = None
            break
    if patt is not None:
        raise ValueError(f'pattern not found: {patt}')

    # read all integer patterns
    patt = next(int_patterns)
    for line in strings:
        m = re.search(patt, line)
        if m is None:
            continue
        else:
            info.update({key: int(val) for key, val
                         in m.groupdict().items()})
            try:
                patt = next(int_patterns)
            except StopIteration:
                patt = None
                break
    if patt is not None:
        raise ValueError(f'pattern not found: {patt}')

    patt = status_patt
    for line in strings:
        m = re.search(patt, line)
        if m is None:
            continue
        else:
            info['is_sat'] = (
                True if m.group('is_sat').lower() == 'satisfiable'
                or m.group('is_sat').lower() == 'optimum found' else
                False)
            patt = None
            break
    if patt is not None:
        raise ValueError(f'pattern not found: {patt}')

    # read assignment
    solution = {}
    patt = solution_patt
    if info['is_sat']:
        for line in strings:
            m = re.search(patt, line)
            if m is None:
                continue
            else:
                solution.update({abs(int(var)): int(int(var) > 0)
                                 for var
                                 in m.group('solution').split()})
                patt = None
                break
        if patt is not None:
            raise ValueError(f'pattern not found: {patt}')

    return solution, info


def solve_sat_openwbo(wcnf_filename):
    """
    Runs the OpenWBO solver and gets the solution

    Parameters
    ----------
    wcnf_filename: str
            Path to the input file
    Returns
    -------
    solution: dict,
            Assignment of the SAT variables
    cost: int,
            Sum of weights of the unsatisfied clauses
    """

    data = run_openwbo(wcnf_filename)
    solution, info = parse_openwbo_out(data)

    if not info['is_sat']:
        cost = info['n_vars']
    else:
        cost = info['cost']

    return solution, cost
