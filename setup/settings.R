# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Settings

library(tidyverse)
library(zoo)
library(cowplot)
library(data.table)
library(RColorBrewer)

theme_set(theme_minimal())

wdir = getwd()
fig_dir <- file.path(wdir, "out", "figures", "raw")
fig_dir_traces <- file.path(wdir, "out", "figures/raw_traces")

simdate <- "20210517"
sim_dir <- file.path(simulation_output, simdate)

startdate <- as.Date("2020-01-01")
sim_end_date <- as.Date("2021-05-01")
first_plot_date <- as.Date("2020-03-01")
baseline_date <- as.Date("2020-09-01")
trigger_min_date <- as.Date("2020-10-01")
last_plot_date <- sim_end_date # as.Date("2020-12-31")

simcolor <- "#F6921E"
capacitycolor <- "#be1e2d"
transm_scen_colors <- c('deepskyblue4', 'deepskyblue')
ntraces_to_keep <- 100
colorscale <- c("#fa9fb5", "#f768a1", "#dd3497", "#e7298a", "#7a0177", "#49006a")

multipliers <- seq(0, 1, 0.2)
region_nr <- 11

brewer.pal(n = 8, name = "Dark2")
mitigation_colors <- c("#D95F02", "#7570B3", "#E7298A", "#66A61E")
counterfactual_col <- '#666666'
single_col <- '#1B9E77'
delay_col <- c("#E6AB02", "#A6761D")

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