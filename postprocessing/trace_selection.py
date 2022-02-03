"""
Assigns negative log-likelihoods to each trace in a set of trajectories.
"""
import argparse
import scipy.stats
import sys
import matplotlib as mpl

mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns

sys.path.append('../')
from setup.processing_helpers import *


def parse_args():
    description = "Simulation run for modeling Covid-19"
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument(
        "-s",
        "--stem",
        type=str,
        help="Name of simulation experiment"
    )
    parser.add_argument(
        "-loc",
        "--Location",
        type=str,
        help="Local or NUCLUSTER",
        default="Local"
    )
    parser.add_argument(
        "--deaths_weight",
        type=float,
        help="Weight of deaths in negative log likelihood calculation. Default is 1.0.",
        default=0.0
    )
    parser.add_argument(
        "--crit_weight",
        type=float,
        help="Weight of ICU population in negative log likelihood calculation. Default is 1.0.",
        default=1.0
    )
    parser.add_argument(
        "--non_icu_weight",
        type=float,
        help="Weight of non-ICU population in negative log likelihood calculation. Default is 1.0.",
        default=0
    )
    parser.add_argument(
        "--cli_weight",
        type=float,
        help="Weight of CLI admissions in negative log likelihood calculation. Default is 1.0.",
        default=0
    )
    parser.add_argument(
        "--plot",
        action='store_true',
        help="If specified, plots with top 50% best-fitting trajectories will be generated.",
    )
    parser.add_argument(
        "--traces_to_keep_ratio",
        type=int,
        help="Ratio of traces to keep out of all trajectories",
        default=4
    )
    parser.add_argument(
        "--traces_to_keep_min",
        type=int,
        help="Minimum number of traces to keep, might overwrite traces_to_keep_ratio for small simulations",
        default=25
    )
    parser.add_argument(
        "--wt",
        action='store_true',
        help="If true, weights simulations differently over time. The weighting needs to be specified within the sum_nll function "
             "If true, it weights the deaths higher in the past than for more recent data, can be customized and also depends on --deaths_weight",
    )
    return parser.parse_args()


def plot_sim_and_ref(df, ems_nr, ref_df, channels, data_channel_names, titles, region_label,
                     first_day, last_day, plot_path, ymax=10000, logscale=False, plot_name_suffix=None):
    fig = plt.figure(figsize=(13, 6))
    palette = sns.color_palette('husl', 8)
    k = 0
    for c, channel in enumerate(channels):
        ax = fig.add_subplot(2, 3, c + 1)

        mdf = df.groupby('date')[channel].agg([CI_50, CI_2pt5, CI_97pt5, CI_25, CI_75]).reset_index()
        ax.plot(mdf['date'], mdf['CI_50'], color=palette[k])
        ax.fill_between(mdf['date'], mdf['CI_2pt5'], mdf['CI_97pt5'],
                        color=palette[k], linewidth=0, alpha=0.2)
        ax.fill_between(mdf['date'], mdf['CI_25'], mdf['CI_75'],
                        color=palette[k], linewidth=0, alpha=0.4)

        ax.set_title(titles[c], y=0.8, fontsize=12)

        ax.xaxis.set_major_formatter(mdates.DateFormatter('%b\n%y'))
        ax.set_xlim(first_day, last_day)
        ax.grid(b=True, which='major', color='#999999', linestyle='-', alpha=0.3)
        if logscale:
            ax.set_ylim(0.1, ymax)
            ax.set_yscale('log')

        ax.plot(ref_df['date'], ref_df[data_channel_names[c]], 'o', color='#303030', linewidth=0, ms=1)
        ax.plot(ref_df['date'], ref_df[data_channel_names[c]].rolling(window=7, center=True).mean(), c='k', alpha=1.0)

    fig.suptitle(region_label, y=1, fontsize=14)
    fig.tight_layout()
    fig.subplots_adjust(top=0.88)

    plot_name = 'compare_to_data_covidregion_' + str(ems_nr)
    if logscale == False:
        plot_name = plot_name + "_nolog"
    if plot_name_suffix is not None:
        plot_name = plot_name + plot_name_suffix
    plt.savefig(os.path.join(plot_path, plot_name + '.png'))
    plt.savefig(os.path.join(plot_path, 'pdf', plot_name + '.pdf'), format='PDF')


def sum_nll(df_values, ref_df_values, wt, wt_past=False):
    """remove NAs in data from both arrays"""
    na_pos = np.argwhere(np.isnan(ref_df_values))
    if len(na_pos) != 0:
        df_values = np.delete(df_values, na_pos)
        ref_df_values = np.delete(ref_df_values, na_pos)
    try:
        x = -np.log10(scipy.stats.poisson(mu=df_values).pmf(k=ref_df_values))
    except ValueError:
        print('ERROR: The simulation and reference arrays may not be the same length.')
        print('Length simulation: ' + str(len(df_values)))
        print('Length reference: ' + str(len(ref_df_values)))
    len_inf = len(list(i for i in list(x) if i == np.inf))
    if len_inf <= len(x) * 0.9:
        x[np.abs(x) == np.inf] = 0

    if wt:
        if wt_past:
            value_weight_array = [5] * 60 + [0.01] * (len(df_values) - 60)
        else:
            value_weight_array = [0.1] * (len(df_values) - 44) + [0.3] * 30 + [2] * 7 + [5] * 7
        value_weight_array = [weight / np.sum(value_weight_array) for weight in value_weight_array]
        x = x * value_weight_array

    return np.sum(x)


def rank_traces_nll(df, ems_nr, ref_df, weights_array=[1.0, 1.0, 1.0, 1.0], wt=False):
    # Creation of rank_df
    [deaths_weight, crit_weight, non_icu_weight, cli_weight] = weights_array

    """ Ensure common dates"""
    df_dates = df[df['date'].isin(ref_df['date'].unique())].date.unique()
    ref_df_dates = ref_df[ref_df['date'].isin(df['date'].unique())].date.unique()
    common_dates = df_dates[np.isin(df_dates, ref_df_dates)]
    df_trunc = df[df['date'].isin(common_dates)]
    ref_df_trunc = ref_df[ref_df['date'].isin(common_dates)]
    df_trunc.drop_duplicates(subset=['sample_num', 'date'], keep='last', inplace=True)
    ref_df_trunc = ref_df_trunc[ref_df_trunc['date'].isin(df_trunc['date'].unique())]

    """select unique samples, usually sample_num==scen_num, except if varying intervention_samples are defined"""
    """hence use WITHIN sampe_num to match trajectories later on"""
    df_trunc = df_trunc.loc[df_trunc.groupby(['run_num', 'sample_num', 'date', 'time']).scen_num.idxmin()]
    run_sample_scen_list = list(df_trunc.groupby(['run_num', 'sample_num']).size().index)
    rank_export_df = pd.DataFrame({'run_num': [], 'sample_num': [], 'nll': []})
    for x in run_sample_scen_list:
        total_nll = 0
        (run_num, sample_num) = x
        df_trunc_slice = df_trunc[(df_trunc['run_num'] == run_num) & (df_trunc['sample_num'] == sample_num)]
        total_nll += deaths_weight * sum_nll(df_trunc_slice['new_deaths_det'].values[:-timelag_days],
                                             ref_df_trunc['deaths'].values[:-timelag_days], wt, wt_past=True)
        total_nll += crit_weight * sum_nll(df_trunc_slice['crit_det'].values,
                                           ref_df_trunc['confirmed_covid_icu'].values, wt)
        total_nll += cli_weight * sum_nll(df_trunc_slice['new_hosp_det'].values, ref_df_trunc['inpatient'].values, wt)
        total_nll += non_icu_weight * sum_nll(df_trunc_slice['hosp_det'].values, ref_df_trunc['covid_non_icu'].values,
                                              wt)
        rank_export_df = rank_export_df.append(
            pd.DataFrame({'run_num': [run_num], 'sample_num': [sample_num], 'nll': [total_nll]}))
    rank_export_df = rank_export_df.dropna()
    rank_export_df['norm_rank'] = (rank_export_df['nll'].rank() - 1) / (len(rank_export_df) - 1)
    rank_export_df = rank_export_df.sort_values(by=['norm_rank']).reset_index(drop=True)
    csv_name = 'traces_ranked_region_' + str(ems_nr) + '.csv'
    # if wt:
    #    csv_name = 'traces_ranked_region_' + str(ems_nr) + '_wt.csv'
    rank_export_df.to_csv(os.path.join(output_path, csv_name), index=False)

    return rank_export_df


def compare_ems(exp_name, ems_nr, first_day, last_day, weights_array, wt,
                traces_to_keep_ratio=2, traces_to_keep_min=1, plot_trajectories=False):
    if ems_nr == 0:
        region_suffix = "_All"
        region_label = 'Illinois'
    else:
        region_suffix = "_EMS-" + str(ems_nr)
        region_label = region_suffix.replace('_EMS-', 'COVID-19 Region ')

    column_list = ['time', 'startdate', 'scen_num', 'sample_num', 'run_num']
    outcome_channels, channels, data_channel_names, titles = get_datacomparison_channels()

    for channel in outcome_channels:
        column_list.append(channel + region_suffix)

    ref_df = load_ref_df(ems_nr)
    ref_df = ref_df[ref_df['date'].between(first_day, last_day)]

    df = load_sim_data(exp_name, region_suffix=region_suffix, column_list=column_list)
    df = df[df['date'].between(first_day, ref_df['date'].max())]
    df = df[df['date'].between(first_day, last_day)]
    df['critical_with_suspected'] = df['critical']

    df_dates = df[df['date'].isin(ref_df['date'].unique())].date.unique()
    ref_df_dates = ref_df[ref_df['date'].isin(df['date'].unique())].date.unique()
    common_dates = df_dates[np.isin(df_dates, ref_df_dates)]
    df = df[df['date'].isin(common_dates)]
    ref_df = ref_df[ref_df['date'].isin(common_dates)]

    rank_export_df = rank_traces_nll(df, ems_nr, ref_df, weights_array=weights_array, wt=wt)

    # Creation of plots
    if plot_trajectories:
        plot_path = os.path.join(output_path, '_plots')

        n_traces_to_keep = int(len(rank_export_df) / traces_to_keep_ratio)
        if n_traces_to_keep < traces_to_keep_min and len(rank_export_df) >= traces_to_keep_min:
            n_traces_to_keep = traces_to_keep_min
        if len(rank_export_df) < traces_to_keep_min:
            n_traces_to_keep = len(rank_export_df)

        df = pd.merge(rank_export_df[0:int(n_traces_to_keep)], df)

        plot_name = f'_best_fit_{str(1 / traces_to_keep_ratio)}_n{str(n_traces_to_keep)}'
        if wt:
            plot_name = f'_best_fit_{str(1 / traces_to_keep_ratio)}_n{str(n_traces_to_keep)}_wt'

        plot_sim_and_ref(df, ems_nr, ref_df, channels=channels, data_channel_names=data_channel_names, titles=titles,
                         region_label=region_label, first_day=first_day, last_day=last_day, plot_path=plot_path,
                         plot_name_suffix=plot_name)


if __name__ == '__main__':

    args = parse_args()
    weights_array = [args.deaths_weight, args.crit_weight, args.non_icu_weight, args.cli_weight]
    stem = args.stem
    Location = args.Location

    """ For plotting"""
    traces_to_keep_ratio = args.traces_to_keep_ratio
    traces_to_keep_min = args.traces_to_keep_min

    """Custom timelag applied to nll calculation for deaths only"""
    timelag_days = 14

    first_plot_day = pd.Timestamp('2020-03-24')
    last_plot_day = pd.Timestamp('2020-09-01')

    datapath, wdir, exe_dir, sim_dir = load_paths(Location=Location)
    sim_output_path = os.path.join(wdir, 'simulation_output')

    exp_names = [x for x in os.listdir(sim_output_path) if stem in x]
    for exp_name in exp_names:
        print(exp_name)
        output_path = os.path.join(sim_output_path, exp_name)
        """Get group names"""
        grp_list, grp_suffix, grp_numbers = get_group_names(exp_path=output_path)

        for ems_nr in grp_numbers:
            print("Start processing region " + str(ems_nr))

            fname = 'traces_ranked_region_' + str(ems_nr) + '.csv'
            if os.path.exists(os.path.join(sim_output_path, exp_name, fname)):
                print(f'\nWARNING:{fname} exists and will be deleted to replace with new trace selection\n')
                os.remove(os.path.join(sim_output_path, exp_name, fname))

            compare_ems(exp_name,
                        ems_nr=int(ems_nr),
                        first_day=first_plot_day,
                        last_day=last_plot_day,
                        weights_array=weights_array,
                        wt=True,
                        plot_trajectories=True,
                        traces_to_keep_ratio=traces_to_keep_ratio,
                        traces_to_keep_min=traces_to_keep_min)
