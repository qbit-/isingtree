"""
This module provides interfaces to run different thirdparty code
"""
import subprocess


def get_dgf2wcnf_path():
    """
    Set up DGF2WCNF path
    """
    import os
    from .graph_model.system_defs import THIRDPARTY_PATH

    _dgf2wcnf_path = os.path.join(
        THIRDPARTY_PATH, 'dgf2wcnf', 'run_dgf2wcnf.sh')
    # _dgf2wcnf_path = os.path.join(
    #     THIRDPARTY_PATH,
    #     'SAT-BasedTW', 'builds', 'DGF2WCNF_TW_MacOS')
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
