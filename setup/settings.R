# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Settings, configurations, and user defined objects

pckg <- c("tidyverse", "zoo", "cowplot", "data.table", "RColorBrewer", "viridis", "Metrics", "scales", "gg.gap")
lapply(pckg, require, character.only = TRUE)
theme_set(theme_minimal())
cleanEnv <- TRUE

##---------------------------------------------
## Directories
##---------------------------------------------
wdir <- getwd()
data_path <- file.path(wdir, "data")
simulation_output <- file.path(wdir, 'simulation_output','20210517')
fig_dir <- file.path(wdir, "figures", "raw")
fig_dir_traces <- file.path(wdir, "figures", "raw_traces")
sim_dir <- file.path(simulation_output)

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir)
  dir.create(file.path(fig_dir, 'csv'))
}
if (!dir.exists(fig_dir_traces)) {
  dir.create(fig_dir_traces)
  dir.create(file.path(fig_dir_traces, 'csv'))
}

##---------------------------------------------
## Settings for analysis
##---------------------------------------------
Chicago_pop <- 2716921 # As used in experiment config yamls
assumed_capacity <- 516  # Extracted for mid September 2020 for Chicago

trace_selection <- TRUE
if (trace_selection) fig_dir <- fig_dir_traces

startdate <- as.Date("2020-01-01")
sim_end_date <- as.Date("2021-05-01")
first_plot_date <- as.Date("2020-03-01")
baseline_date <- as.Date("2020-09-01")
trigger_min_date <- as.Date("2020-10-01")
last_plot_date <- sim_end_date # as.Date("2020-12-31")

capacitycolor <- "#be1e2d"
transm_scen_cols <- c("deepskyblue4", "deepskyblue")
delay_scen_cols <- c("#dc362d", "firebrick4")
mitigation_cols <- c("#D95F02", "#7570B3", "#E7298A", "#66A61E")

ntraces_to_keep <- 100
exp_name_baseline <- "baseline"
counterfactual_exps <- c('100perc_counterfactual_reopen',
                         '50perc_counterfactual_reopen')

trigger_examples <- c('100perc_1daysdelay_pr6_triggeredrollback_reopen',
                      '50perc_1daysdelay_pr6_triggeredrollback_reopen')

mitigation_examples <- c('100perc_1daysdelay_pr2_triggeredrollback_reopen',
                         '100perc_1daysdelay_pr4_triggeredrollback_reopen',
                         '100perc_1daysdelay_pr6_triggeredrollback_reopen',
                         '100perc_1daysdelay_pr8_triggeredrollback_reopen')

mitigation50_examples <- gsub("100perc", "50perc", mitigation_examples)

exp_names_100_delay1 <- c('100perc_1daysdelay_pr2_triggeredrollback_reopen',
                          '100perc_1daysdelay_pr4_triggeredrollback_reopen',
                          '100perc_1daysdelay_pr6_triggeredrollback_reopen',
                          '100perc_1daysdelay_pr8_triggeredrollback_reopen')

exp_names_50_delay1 <- c('50perc_1daysdelay_pr2_triggeredrollback_reopen',
                         '50perc_1daysdelay_pr4_triggeredrollback_reopen',
                         '50perc_1daysdelay_pr6_triggeredrollback_reopen',
                         '50perc_1daysdelay_pr8_triggeredrollback_reopen')

exp_names_100_delay7 <- c('100perc_7daysdelay_pr2_triggeredrollback_reopen',
                          '100perc_7daysdelay_pr4_triggeredrollback_reopen',
                          '100perc_7daysdelay_pr6_triggeredrollback_reopen',
                          '100perc_7daysdelay_pr8_triggeredrollback_reopen')

exp_names_50_delay7 <- c('50perc_7daysdelay_pr2_triggeredrollback_reopen',
                         '50perc_7daysdelay_pr4_triggeredrollback_reopen',
                         '50perc_7daysdelay_pr6_triggeredrollback_reopen',
                         '50perc_7daysdelay_pr8_triggeredrollback_reopen')


exp_names_100_delay14 <- c('100perc_14daysdelay_pr2_triggeredrollback_reopen',
                           '100perc_14daysdelay_pr4_triggeredrollback_reopen',
                           '100perc_14daysdelay_pr6_triggeredrollback_reopen',
                           '100perc_14daysdelay_pr8_triggeredrollback_reopen')

exp_names_50_delay14 <- c('50perc_14daysdelay_pr2_triggeredrollback_reopen',
                          '50perc_14daysdelay_pr4_triggeredrollback_reopen',
                          '50perc_14daysdelay_pr6_triggeredrollback_reopen',
                          '50perc_14daysdelay_pr8_triggeredrollback_reopen')