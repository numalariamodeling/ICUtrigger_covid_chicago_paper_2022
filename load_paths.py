import os

def load_box_paths(user_path=None, Location='Local'):

    user_path = user_path
    home_path = os.path.join(user_path, 'Box', 'NU-malaria-team', 'projects')
    data_path = os.path.join(user_path, 'Box', 'NU-malaria-team', 'data')
    git_dir = os.path.join(user_path, 'gitrepos', 'ICUtrigger_covid_chicago')
    project_path = os.path.join(home_path, 'covid_chicago')
    wdir = os.path.join(project_path, 'cms_sim')
    exe_dir = os.path.join(git_dir, 'binaries')

    return data_path, project_path, wdir, exe_dir, git_dir

