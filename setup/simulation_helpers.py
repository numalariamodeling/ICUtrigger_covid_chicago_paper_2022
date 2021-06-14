import logging
import os
import subprocess
import shutil
import stat
import sys
import numpy as np
import pandas as pd
import matplotlib as mpl

mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns

mpl.rcParams['pdf.fonttype'] = 42
from setup.processing_helpers import CI_50, CI_25, CI_75, CI_2pt5, CI_97pt5, load_sim_data, get_group_names

from setup.load_paths import load_paths

datapath, wdir, sim_dir, EXE_DIR = load_paths()

log = logging.getLogger(__name__)


def DateToTimestep(date, startdate):
    datediff = date - startdate
    timestep = datediff.days
    return timestep


def TimestepToDate(timesteps, startdate):
    dates = startdate + pd.Timedelta(timesteps, 'days')
    return dates


def get_cms_cmd(exe_dir=EXE_DIR, workdir=None, docker_image=None):
    """Generate the command to invoke the CMS executable

    Cross-platform -- generate the appropriate command on Windows, OS X, or Linux.
    On OS X or Linux, run either via a Docker container which contains CMS,
    or with wine.

    Parameters
    ----------
    exe_dir : str, optional
        The directory of your `compartments.exe` executable.
        Not needed if running via Docker.
    workdir : str, optional
        Only needed for non-Windows systems. The working directory, which
        must contain the config file, the emodl file, and the output location.
    docker_image : str, optional
        Only needed for non-Windows systems.
        If provided, generate a run command which invokes this Docker image.
        If not provided, generate a run command which invokes `wine` to run
        `compartments.exe`.

    Returns
    -------
    cmd : str
        A string which can be executed to run CMS
    """

    if sys.platform in ['win32', 'cygwin']:
        log.debug("Generating Windows run command")
        return os.path.join(exe_dir, 'compartments.exe')
    else:
        if docker_image:
            log.debug(f"Generating Docker run command for workdir {workdir}")
            if not workdir:
                raise TypeError("Must provide `workdir` input for running via Docker")
            cmd = f"docker run -v={workdir}:{workdir} {docker_image} -d {workdir}"
        else:
            cmd = f"wine {os.path.join(exe_dir, 'compartments.exe')} -d {workdir}"
        return cmd


def runExp(trajectories_dir, Location='Local', submission_script=None):
    if Location == 'Local':
        log.info("Starting experiment.")
        p = os.path.join(trajectories_dir, 'runSimulations.bat')
        subprocess.call([p])
    if Location == 'NUCLUSTER':
        if submission_script is None:
            submission_script = 'submit_runSimulations.sh'
        p = os.path.join(trajectories_dir, submission_script)
        subprocess.call(['sh', p])


def reprocess(trajectories_dir, temp_exp_dir, input_fname='trajectories.csv', output_fname=None):
    fname = os.path.join(trajectories_dir, input_fname)
    row_df = pd.read_csv(fname, skiprows=1)
    df = row_df.set_index('sampletimes').transpose()
    run_time = len([x for x in df.columns.values if '{0}' in x])
    num_runs = int((len(row_df)) / run_time)

    df = df.reset_index(drop=False)
    df = df.rename(columns={'index': 'time'})
    df['time'] = df['time'].astype(float)

    adf = pd.DataFrame()
    for run_num in range(num_runs):
        channels = [x for x in df.columns.values if '{%d}' % run_num in x]
        sdf = df[['time'] + channels]
        sdf = sdf.rename(columns={
            x: x.split('{')[0] for x in channels
        })
        sdf['run_num'] = run_num
        adf = pd.concat([adf, sdf])

    adf = adf.reset_index()
    del adf['index']
    if output_fname:
        adf.to_csv(os.path.join(temp_exp_dir, output_fname), index=False)
    return adf


def combineTrajectories(Nscenarios, trajectories_dir, temp_exp_dir, deleteFiles=False, addSamples=True,
                        sim_dir=sim_dir):
    sampledf = pd.read_csv(os.path.join(temp_exp_dir, "sampled_parameters.csv"))
    if addSamples == False:
        sampledf = sampledf[["scen_num", "sample_num", "startdate"]]
    df_list = []
    for scen_i in range(Nscenarios + 1):
        input_name = "trajectories_scen" + str(scen_i) + ".csv"
        try:
            df_i = reprocess(trajectories_dir=trajectories_dir, temp_exp_dir=temp_exp_dir, input_fname=input_name)
            df_i['scen_num'] = scen_i
            df_i = df_i.merge(sampledf, on=['scen_num'])
            df_list.append(df_i)
        except:
            continue

        if deleteFiles == True: os.remove(os.path.join(sim_dir, input_name))

    dfc = pd.concat(df_list)
    dfc.to_csv(os.path.join(temp_exp_dir, "trajectoriesDat.csv"), index=False, date_format='%Y-%m-%d')

    nscenarios = sampledf['scen_num'].max()
    nscenarios_processed = len(dfc['scen_num'].unique())
    trackScen = "Number of scenarios processed n= " + str(nscenarios_processed) + " out of total N= " + str(
        nscenarios) + " (" + str(nscenarios_processed / nscenarios) + " %)"
    writeTxt(temp_exp_dir, "Simulation_report.txt", trackScen)

    return dfc


def writeTxt(txtdir, filename, textstring):
    file = open(os.path.join(txtdir, filename), 'w')
    file.write(textstring)
    file.close()


def get_process_dict():
    process_dict = {'0_runCombineAndTrimTrajectories': 'combine_and_trim.py',
                    '0_cleanupSimulations': 'cleanup.py',
                    '1_runTraceSelection': 'trace_selection.py',
                    '2_runDataComparison': 'data_comparison_spatial.py',
                    '3_runRtEstimation': 'estimate_Rt_forCivisOutputs.py',
                    '4_runCleanUpAndZip': 'cleanup_and_zip_simFiles.py'
                    }
    return process_dict


def generateSubmissionFile(scen_num, exp_name, experiment_config, trajectories_dir, temp_dir, temp_exp_dir,
                           sim_output_path,
                           model, exe_dir=EXE_DIR, docker_image="cms", sim_dir=sim_dir):
    process_dict = get_process_dict()

    fname = f'runSimulations.bat'
    log.debug(f"Generating submission file {fname}")
    if sys.platform not in ["win32", "cygwin"]:
        file = open(os.path.join(trajectories_dir, fname), 'w')
        # If this is OSX or Linux, mark the file as executable and
        # write a bash script
        os.chmod(fname, stat.S_IXUSR | stat.S_IWUSR | stat.S_IRUSR)
        cfg_fname = os.path.join(temp_dir, 'model_$i.cfg')
        emodl_fname = os.path.join(temp_dir, 'simulation_$i.emodl')
        file.write(f"""#!/bin/bash
echo start
for i in {{1..{scen_num}}} 
  do
    {get_cms_cmd(exe_dir, temp_exp_dir, docker_image)} -c "{cfg_fname}" -m "{emodl_fname}"
  done
echo end""")
    else:
        file = open(os.path.join(trajectories_dir, fname), 'w')
        file.write('ECHO start' + '\n' + 'FOR /L %%i IN (1,1,{}) DO ( "{}" -c "{}" -m "{}") >> "{}/log/log.txt"'.format(
            str(scen_num),
            get_cms_cmd(exe_dir, temp_exp_dir),
            os.path.join(temp_dir, "model_%%i" + ".cfg"),
            os.path.join(temp_dir, "simulation_%%i" + ".emodl"),
            os.path.join(temp_exp_dir)
        ) + "\n ECHO end")

        emodl_name = str([i for i in os.listdir(temp_exp_dir) if "emodl" in i][0]).replace('.emodl', '')
        emodl_from = os.path.join(sim_output_path, emodl_name + ".emodl")
        emodl_to = os.path.join(sim_dir, "../emodl", emodl_name + "_resim.emodl").replace("/", "\\")
        csv_from = os.path.join(sim_output_path, 'sampled_parameters.csv').replace("/", "\\")
        csv_to = os.path.join(sim_dir, "../experiment_configs", "input_csv").replace("/", "\\")
        sim_dir = sim_dir.replace("/", "\\")

        """ Postprocessing batch files """
        file = open(os.path.join(temp_exp_dir, 'bat', f'{list(process_dict.keys())[0]}.bat'), 'w')
        file.write(f'cd {sim_dir} \n python {list(process_dict.values())[0]} --exp_name "{exp_name}" \n')

        file = open(os.path.join(temp_exp_dir, 'bat', f'{list(process_dict.keys())[1]}.bat'), 'w')
        file.write(
            f'cd {sim_dir} \npython {sim_dir}/{list(process_dict.values())[1]} --stem "{exp_name}" --Location "Local" \n')

        plotters_dir = os.path.join(sim_dir, "plotters")
        for i in range(2, len(process_dict.values())):
            file = open(os.path.join(temp_exp_dir, 'bat', f'{list(process_dict.keys())[i]}.bat'), 'w')
            file.write(
                f'cd {plotters_dir} \n python {list(process_dict.values())[i]} --stem "{exp_name}" >> "{sim_output_path}/log/{list(process_dict.keys())[i]}.txt" \n')


def shell_header(A='p30781', p='short', t='02:00:00', N=1, ntasks_per_node=1, memG=18, job_name='myjob', arrayJob=None):
    if 'b1139' in os.getcwd():
        A = 'b1139'
        p = 'b1139'
        t = '00:45:00'

    header = f'#!/bin/bash\n' \
             f'#SBATCH -A {A}\n' \
             f'#SBATCH -p {p}\n' \
             f'#SBATCH -t {t}\n' \
             f'#SBATCH -N {N}\n' \
             f'#SBATCH --ntasks-per-node={ntasks_per_node}\n' \
             f'#SBATCH --mem={memG}G\n' \
             f'#SBATCH --job-name="{job_name}"\n'
    if arrayJob is not None:
        array = arrayJob
        err = '#SBATCH --error=log/arrayJob_%A_%a.err\n'
        out = '#SBATCH --output=log/arrayJob_%A_%a.out\n'
        header = header + array + err + out
    else:
        err = f'#SBATCH --error=log/{job_name}.%j.err\n'
        out = f'#SBATCH --output=log/{job_name}.%j.out\n'
        header = header + err + out
    return header


def generateSubmissionFile_quest(scen_num, exp_name, experiment_config, trajectories_dir, sim_dir, temp_exp_dir,
                                 exe_dir, sim_output_path, model):
    # Generic shell submission script that should run for all having access to NU cluster allocation
    # submit_runSimulations.sh

    process_dict = get_process_dict()

    exp_name_short = exp_name[-20:]
    array = f'#SBATCH --array=1-{str(scen_num)}\n'
    header = shell_header(job_name=exp_name_short, arrayJob=array)
    header_post = shell_header(t="02:00:00", memG=64, job_name=exp_name_short)
    module = '\n\nmodule load singularity'
    slurmID = '${SLURM_ARRAY_TASK_ID}'
    singularity = '\n\nsingularity exec -B /projects:/projects/ /software/singularity/images/singwine-v1.img wine ' \
                  f'{exe_dir}/compartments.exe ' \
                  f'-c {sim_dir}/_temp/{exp_name}/model_{slurmID}.cfg ' \
                  f'-m {sim_dir}/_temp/{exp_name}/simulation_{slurmID}.emodl'
    file = open(os.path.join(trajectories_dir, 'runSimulations.sh'), 'w')
    file.write(header + module + singularity)
    file.close()

    plotters_dir = os.path.join(sim_dir, "postprocessing")
    pymodule = '\n\nmodule purge all\nmodule load python/anaconda3.6\nsource activate /projects/p30781/anaconda3/envs/team-test-py37\n'
    if 'b1139' in os.getcwd():
        pymodule = '\n\nmodule purge all\nmodule load python/anaconda3.6\nsource activate /projects/b1139/anaconda3/envs/team-test-py37\n'

    emodl_name = str([i for i in os.listdir(temp_exp_dir) if "emodl" in i][0]).replace('.emodl', '')
    emodl_from = os.path.join(sim_output_path, emodl_name + ".emodl")
    emodl_to = os.path.join(sim_dir, "../emodl", emodl_name + "_resim.emodl").replace("\\", "/")
    csv_from = os.path.join(sim_output_path, 'sampled_parameters.csv').replace("\\", "/")
    csv_to = os.path.join(sim_dir, "../experiment_configs", "input_csv").replace("\\", "/")
    sim_dir = sim_dir.replace("\\", "/")

    """Use this batch files for postprocessing multiple steps"""
    pycommand = f'\ncd {sim_dir}\npython {list(process_dict.values())[0]}  --exp_name "{exp_name}" --Location "NUCLUSTER" '
    file = open(os.path.join(temp_exp_dir, 'run_postprocessing.sh'), 'w')
    file.write(header_post + pymodule + pycommand)
    file.write(
        f'\n\ncd {sim_dir} \npython {sim_dir}/{list(process_dict.values())[1]} --stem "{exp_name}" --Location "NUCLUSTER"')
    file.write(
        f'\n\ncd {plotters_dir} \npython {plotters_dir}/{list(process_dict.values())[3]} --stem "{exp_name}" --Location "NUCLUSTER"')
    file.write(
        f'\npython {plotters_dir}/{list(process_dict.values())[2]} --stem "{exp_name}" --Location "NUCLUSTER" --plot')
    file.write(f'\npython {plotters_dir}/{list(process_dict.values())[3]} --stem "{exp_name}" --Location "NUCLUSTER"')
    file.write(f'\npython {plotters_dir}/{list(process_dict.values())[4]} --stem "{exp_name}" --Location "NUCLUSTER"')
    file.write(f'\npython {plotters_dir}/{list(process_dict.values())[5]} --stem "{exp_name}" --Location "NUCLUSTER"')
    file.close()

    """Shell files for single job submission """
    for i in range(2, len(process_dict.values())):
        pycommand = f'\ncd {sim_dir}\npython {list(process_dict.values())[i]}  --exp_name "{exp_name}" --Location "NUCLUSTER" '
        file = open(os.path.join(temp_exp_dir, 'sh', f'{list(process_dict.keys())[i]}.sh'), 'w')
        file.write(header_post + pymodule + pycommand)
        file.close()

    """Submit run simulation """
    submit_runSimulations = f'cd {temp_exp_dir}/trajectories/\ndos2unix runSimulations.sh\nsbatch runSimulations.sh\n'
    submit_combineSimulations = f'cd {temp_exp_dir}/\nsbatch --dependency=singleton run_postprocessing.sh'
    file = open(os.path.join(temp_exp_dir, 'submit_runSimulations.sh'), 'w')
    file.write(submit_runSimulations)
    file.write(submit_combineSimulations)
    file.close()

    array = '#SBATCH --array=1-11\n'
    header_post = shell_header(t="04:00:00", memG=64, job_name=f'rt1_{exp_name}', arrayJob=array)
    pycommand = f'\ncd {plotters_dir}\npython estimate_Rt_trajectores.py  --stem "{exp_name}" --Location "NUCLUSTER" ' \
                '--subregion ${SLURM_ARRAY_TASK_ID} '
    file = open(os.path.join(temp_exp_dir, 'sh', '4a_runRtEstimation_trajectories.sh'), 'w')
    file.write(header_post + pymodule + pycommand)
    file.close()

    ### Combine Rt estimates from parallel run
    header_post = shell_header(t="02:00:00", memG=32, job_name=f'rt2_{exp_name}')
    pycommand = f'\ncd {plotters_dir}\npython estimate_Rt_trajectores.py  --stem "{exp_name}" --Location "NUCLUSTER" --combine_and_plot '
    file = open(os.path.join(temp_exp_dir, 'sh', '4b_runRtEstimation_trajectories.sh'), 'w')
    file.write(header_post + pymodule + pycommand)
    file.close()

    submit_runSimulations = f'cd {temp_exp_dir}/sh/\nsbatch 4a_runRtEstimation_trajectories.sh\n'
    submit_combineSimulations = f'cd {temp_exp_dir}/sh/\nsbatch --dependency=singleton 4b_runRtEstimation_trajectories.sh'
    file = open(os.path.join(temp_exp_dir, 'submit_runRtEstimation_trajectories.sh'), 'w')
    file.write(submit_runSimulations)
    file.write(submit_combineSimulations)
    file.close()




def write_emodl(model, subregion, scenario, observeLevel, change_testDelay, expandModel, intervention_config,
                fit_params, emodl_name):
    from emodl_generator_locale import covidModel

    # covidModel.showOptions()
    ml = covidModel(subgroups=subregion,
                    add_interventions=scenario,
                    change_testDelay=change_testDelay,
                    observeLevel=observeLevel,
                    expandModel=expandModel,
                    intervention_config=intervention_config,
                    fit_params=fit_params,
                    emodl_name=emodl_name)
    emodl_name = ml.generate_emodl()
    return f'{emodl_name}.emodl'

def makeExperimentFolder(exp_name, emodl_dir, emodlname, cfg_dir, cfg_file, yaml_dir, DEFAULT_CONFIG, experiment_config,
                         intervention_config, temp_exp_dir=None, wdir=None, sim_dir=None):
    sim_output_path = os.path.join(wdir, 'simulation_output', exp_name)
    plot_path = sim_output_path

    if temp_exp_dir == None:
        temp_exp_dir = os.path.join(sim_dir, '_temp', exp_name)
    temp_dir = os.path.join(temp_exp_dir, 'simulations')
    trajectories_dir = os.path.join(temp_exp_dir, 'trajectories')
    if not os.path.exists(os.path.join(sim_dir, '_temp')):
        os.makedirs(os.path.join(os.path.join(sim_dir, '_temp')))
    if not os.path.exists(temp_exp_dir):
        os.makedirs(temp_exp_dir)
        os.makedirs(temp_dir)
        os.makedirs(trajectories_dir)
        os.makedirs(os.path.join(temp_exp_dir, 'log'))
        os.makedirs(os.path.join(trajectories_dir, 'log'))  # location of log file on quest
        os.makedirs(os.path.join(temp_exp_dir, '_plots'))
        os.makedirs(os.path.join(temp_exp_dir, '_plots', 'pdf'))
        os.makedirs(os.path.join(temp_exp_dir, 'bat'))
        os.makedirs(os.path.join(temp_exp_dir, 'sh'))
        os.makedirs(os.path.join(temp_exp_dir, 'sh', 'log'))

    ## Copy emodl and cfg file  to experiment folder
    shutil.copyfile(os.path.join(emodl_dir, emodlname), os.path.join(temp_exp_dir, emodlname))
    shutil.copyfile(os.path.join(cfg_dir, cfg_file), os.path.join(temp_exp_dir, cfg_file))
    shutil.copyfile(os.path.join(yaml_dir, experiment_config), os.path.join(temp_exp_dir, experiment_config))
    shutil.copyfile(os.path.join(yaml_dir, intervention_config), os.path.join(temp_exp_dir, intervention_config))
    if DEFAULT_CONFIG != experiment_config:
        shutil.copyfile(os.path.join(yaml_dir, DEFAULT_CONFIG), os.path.join(temp_exp_dir, DEFAULT_CONFIG))

    return temp_dir, temp_exp_dir, trajectories_dir, sim_output_path, plot_path


def runSamplePlot(exp_name, sim_output_path, plot_path, channel_list_name="master"):
    grp_list, grp_suffix, grp_numbers = get_group_names(exp_path=sim_output_path)
    sample_grp = grp_list[-1]
    df = load_sim_data(exp_name, region_suffix=f'_{sample_grp}', fname='trajectoriesDat.csv')

    if channel_list_name == "master":
        channel_list = ['susceptible', 'exposed', 'asymp', 'symp_mild',
                        'hospitalized', 'detected', 'critical', 'deaths', 'recovered']
    if channel_list_name == "detection":
        channel_list = ['detected', 'detected_cumul', 'asymp_det_cumul', 'hosp_det_cumul']
    if channel_list_name == "custom":
        channel_list = ['susceptible', 'infected', 'hospitalized', 'hosp_det',
                        'critical', 'crit_det', 'deaths', 'deaths_det']

    sampleplot(df, sample_grp, allchannels=channel_list,
               plot_fname=os.path.join(plot_path, f'{channel_list_name}_sample_plot.png'))


def sampleplot(df, sample_grp, allchannels, plot_fname=None):
    fig = plt.figure(figsize=(18, 8))
    fig.suptitle(sample_grp)
    palette = sns.color_palette('Set1', 10)

    axes = [fig.add_subplot(3, 3, x + 1) for x in range(len(allchannels))]
    fig.subplots_adjust(bottom=0.05, hspace=0.25, wspace=0.2, right=0.95, left=0.1)
    for c, channel in enumerate(allchannels):
        mdf = df.groupby('date')[channel].agg([np.min, CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75, np.max]).reset_index()
        ax = axes[c]
        ax.plot(mdf['date'], mdf['CI_50'], label=channel, color=palette[c])
        ax.fill_between(mdf['date'], mdf['CI_2pt5'], mdf['CI_97pt5'],
                        color=palette[c], linewidth=0, alpha=0.2)
        ax.fill_between(mdf['date'], mdf['CI_25'], mdf['CI_75'],
                        color=palette[c], linewidth=0, alpha=0.4)
        ax.fill_between(mdf['date'], mdf['amin'], mdf['amax'],
                        color=palette[c], linewidth=0, alpha=0.1)

        ax.set_title(channel, y=0.8)
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%b\n%y'))

    if plot_fname:
        log.info(f"Writing plot to {plot_fname}")
        plt.savefig(plot_fname)
    # plt.show()
