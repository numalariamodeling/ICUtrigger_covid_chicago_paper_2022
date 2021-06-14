import os

def load_paths(user_path=None, Location='Local'):

    dir_path = os.path.dirname(os.path.realpath(__file__))
    wdir = os.path.abspath(os.path.join(dir_path, os.pardir))
    data_path = os.path.join(wdir, 'data')
    sim_dir = os.path.join(wdir, 'simulations')
    exe_dir = os.path.join(sim_dir, 'binaries')

    return data_path, wdir, sim_dir, exe_dir

