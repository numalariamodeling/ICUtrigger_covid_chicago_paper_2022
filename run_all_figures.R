# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Run all figures (R scripts)

homedir <- Sys.getenv("HOME")
#setwd(paste0(homedir,"ICUtrigger_covid_chicago_paper_2021"))
data_files_exist <- TRUE

## Data files:
## - COVID-19_ICU_Chicago_2020.csv (shared in repository with permission from IDPH),
## - LL_data.csv for admissions and deaths data used for fitting (request from IDPH)

## Note each figure script calls the settings.R
## and changes to user-defined objects need to be made before running all
## i.e. whether to exclude traces or not, capacity level...
# source(file.path('setup/settings.R'))

##-------------------------
## Fitting
##-------------------------
# submit fitting simulations and run fit_to_data_spatial.R
# done in a separate step previous to running reopening ICU overflow scenarios.
# requires LL data, or alternatively modify to fit only to ICU.

##-------------------------
## Simulation
##-------------------------
# see submission_lines.txt

##-------------------------
## Postprocessing
##-------------------------
#combine_trajectory_chunks.R
#trace_selection.py
#estimate_Rt_trajectores.py
#filter_trigger_activated.R

##-------------------------
## Main figures
##-------------------------
if(data_files_exist)source(file.path('figure_scripts','main_fig_3.R'))

source(file.path('figure_scripts','main_fig_4.R'))
source(file.path('figure_scripts','main_fig_5A.R'))
source(file.path('figure_scripts','main_fig_5B.R'))
source(file.path('figure_scripts','main_fig_5C.R'))
source(file.path('figure_scripts','main_fig_6.R'))

##-------------------------
## Supplementary figures
##-------------------------
#source(file.path('figure_scripts','S1_fig_2and3.R')) # requires LLdata
#source(file.path('figure_scripts','S1_fig_4.R')) # requires LLdata
#source(file.path('figure_scripts','S1_fig_5.R')) # requires LLdata
#source(file.path('figure_scripts','S1_fig_6and7.R')) # requires LLdata
source(file.path('figure_scripts','S1_fig_8.R'))
source(file.path('figure_scripts','S1_fig_9.R'))
# S1_fig_10 generated in main figure scripts
source(file.path('figure_scripts','S1_fig_11.R'))
source(file.path('figure_scripts','S1_fig_12.R'))
# S1_fig_13 generated in main figure scripts
source(file.path('figure_scripts','S1_fig_14.R'))
source(file.path('figure_scripts','S1_fig_15.R'))
source(file.path('figure_scripts','S1_fig_16.R'))
source(file.path('figure_scripts','S1_fig_17.R'))
source(file.path('figure_scripts','S1_fig_18.R'))
# S1_fig_19 external figure
source(file.path('figure_scripts','S1_fig_20.R'))
# Tables
source(file.path('figure_scripts','S1_table_6.R'))
source(file.path('figure_scripts','S1_table_7.R'))
