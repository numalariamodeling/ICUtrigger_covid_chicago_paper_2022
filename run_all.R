# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Run all

homedir <- Sys.getenv("HOME")
#setwd(paste0(homedir,"ICUtrigger_covid_chicago_paper_2021"))
data_files_exist <- TRUE



##-------------------------
## Main figures
##-------------------------
# Data files: capacity_by_covid_region.csv, capacity_weekday_average_20200915.csv
if(data_files_exist)source(file.path('figure_scripts','main_fig_3.R'))

source(file.path('figure_scripts','main_fig_4.R'))
source(file.path('figure_scripts','main_fig_5A.R'))
source(file.path('figure_scripts','main_fig_5B.R'))
source(file.path('figure_scripts','main_fig_5C.R'))
source(file.path('figure_scripts','main_fig_6.R'))

##-------------------------
## Supplementary figures
##-------------------------
source(file.path('figure_scripts','S1_fig_2and3.R'))
source(file.path('figure_scripts','S1_fig_4.R'))
source(file.path('figure_scripts','S1_fig_5.R'))
source(file.path('figure_scripts','S1_fig_6and7.R'))
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

source(file.path('figure_scripts','S1_table_6.R'))
source(file.path('figure_scripts','S1_table_7.R'))
