# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 12

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

channels <- c('ICU_census')

f_combineData <- function(exp_name) {
  dat_list <- list()
  for (trace_selection in c(TRUE)) {
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name = exp_name, sim_dir = sim_dir, fname = "trajectoriesDat_region_11_trimfut.csv",
      add_peak_cols = FALSE,
      add_trigger_cols = TRUE,
      addRt = FALSE,
      trace_selection = trace_selection) %>%
      #filter(date >= baseline_date) %>%
      ungroup() %>%
      dplyr::select(date, sample_num, scen_num, capacity_multiplier, crit_det, N_no_trigger, N_trigger, trigger_activated) %>%
      rename(ICU_census = crit_det) %>%
      pivot_longer(cols = -c(date, sample_num, scen_num, capacity_multiplier, N_no_trigger, N_trigger, trigger_activated), names_to = "outcome") %>%
      mutate(trace_selection = trace_selection) %>%
      filter(outcome %in% channels) %>%
      filter(date <= last_plot_date)
  }
  dat <- dat_list %>%
    bind_rows() %>%
    as.data.table()

  ### add samples parameters
  sdf <- fread(file.path(sim_dir, exp_name, 'sampled_parameters.csv')) %>%
    dplyr::select(scen_num, sample_num1, time_to_infectious, time_to_symptoms, time_to_hospitalization, time_to_critical, time_to_death,
                  reduced_inf_of_det_cases, cfr, recovery_time_crit, recovery_time_hosp, time_to_detection_As, recovery_time_asymp,
                  d_Sys, d_Sys_incr1, d_Sys_incr2, d_Sys_incr3, d_Sys_incr4, d_Sys_incr5, d_Sys_incr6, d_Sys_incr7,
                  fraction_symptomatic, fraction_severe, fraction_critical, fraction_critical_incr1, fraction_critical_incr2, fraction_critical_incr3,
                  d_Sym_EMS_11, d_Sym_change1_EMS_11, d_Sym_change2_EMS_11, d_Sym_change3_EMS_11, d_Sym_change4_EMS_11, d_Sym_change5_EMS_11,
                  fraction_hospitalized, fraction_dead) %>%
    rename(sample_num = sample_num1)

  dat <- dat %>% left_join(sdf)
  rm(dat_list)
  return(dat)
}

scenario_exps <- c('50perc_1daysdelay_pr2_triggeredrollback_reopen',
                   '50perc_1daysdelay_pr4_triggeredrollback_reopen',
                   '50perc_1daysdelay_pr6_triggeredrollback_reopen',
                   '50perc_1daysdelay_pr8_triggeredrollback_reopen')

dat_list2 <- list()
for (exp_name in scenario_exps) {
  dat_list2[[length(dat_list2) + 1]] <- f_combineData(exp_name = exp_name) %>%
    mutate(exp_name = exp_name)

}
dat <- dat_list2 %>%
  bind_rows() %>%
  as.data.table()

table(dat$exp_name)
dat$trigger_activated <- factor(dat$trigger_activated, levels = c(0, 1), labels = c('no', 'yes'))


### add samples
datAggr <- dat %>%
  group_by(date, exp_name, capacity_multiplier, outcome, trace_selection, trigger_activated) %>%
  summarise(n = n(),
            median = median(value), mean = mean(value),
            lower = quantile(value, probs = 0.05, na.rm = TRUE),
            upper = quantile(value, probs = 0.95, na.rm = TRUE))

summary(datAggr$n)
tapply(datAggr$n, datAggr$trace_selection, summary)
table(dat$trace_selection, dat$outcome)

datAggr$date <- as.Date(datAggr$date)
dat$date <- as.Date(dat$date)
dat <- f_get_scenVars(dat)

textdat <- dat %>%
  filter(capacity_multiplier >= 0.5 &
           trace_selection == 'TRUE' &
           rollback_fct == '60' &
           outcome %in% channels) %>%
  dplyr::select(rollback_fct, capacity_multiplier_fct, capacity_multiplier) %>%
  unique()

pplotA <- ggplot(data = subset(dat, capacity_multiplier >= 0.5 &
  trace_selection == 'TRUE' &
  rollback_fct == '60' &
  outcome %in% channels)) +
  geom_line(aes(x = date, y = value, col = trigger_activated, group = scen_num), size = 0.6, alpha = 0.7) +
  facet_grid(capacity_multiplier_fct ~ rollback_fct) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  scale_y_continuous(lim = c(0, 1000), sec.axis = sec_axis(~. * 0, name = 'trigger at % occupancy')) +
  geom_hline(yintercept = 516, color = 'darkred', linetype = 'dashed') +
  geom_hline(aes(yintercept = 516 * capacity_multiplier), color = 'black', linetype = 'dashed') +
  #geom_text(label='ICU capacity', y=525, x=as.Date('2020-10-01'), color = 'darkred') +
  #geom_text(data=textdat,aes(label='occupancy\nthreshold', y=516* capacity_multiplier, x=as.Date('2020-10-01'))) +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Total number", color = "trigger\nactivated", fill = "trigger\nactivated") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "right",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank()) +
  theme(panel.spacing = unit(1, "lines"))

# f_save_plot(
#   plot_name = paste0("S1_fig_12_traj"), pplot = pplot,
#   plot_dir = file.path(fig_dir), width = 12, height = 16
# )

colnames(dat) <- gsub("_EMS_11", "", colnames(dat))
pdat <- dat %>%
  filter(as.character(date) == baseline_date) %>%
  dplyr::select(-c(date, exp_name, reopen, rollback, delay, N_no_trigger, capacity_multiplier_fct2, capacity_multiplier_fct,
                   N_trigger, outcome, value, trace_selection, reopen_fct, reopen_fct2, rollback_fct, time_to_detection_As)) %>%
  pivot_longer(cols = -c(sample_num, scen_num, capacity_multiplier, trigger_activated)) %>%
  mutate(value = round(value, 2))

#pdat$name_old<-pdat$name
#pdat$name<-pdat$name_old

pdat$name[pdat$name == 'cfr'] <- 'italic("cfr")'
pdat$name[pdat$name == 'd_Sym'] <- 'italic("dSym")'
pdat$name[pdat$name == 'd_Sym'] <- 'italic("dSym")'
pdat$name[pdat$name == 'd_Sym_change1'] <- 'italic("dSym_1")'
pdat$name[pdat$name == 'd_Sym_change2'] <- 'italic("dSym_2")'
pdat$name[pdat$name == 'd_Sym_change3'] <- 'italic("dSym_3")'
pdat$name[pdat$name == 'd_Sym_change4'] <- 'italic("dSym_4")'
pdat$name[pdat$name == 'd_Sym_change5'] <- 'italic("dSym_5")'

pdat$name[pdat$name == 'd_Sys'] <- 'italic("d_Sys")'
pdat$name[pdat$name == 'd_Sys_incr1'] <- 'italic("dSys_1")'
pdat$name[pdat$name == 'd_Sys_incr2'] <- 'italic("dSys_2")'
pdat$name[pdat$name == 'd_Sys_incr3'] <- 'italic("dSys_3")'
pdat$name[pdat$name == 'd_Sys_incr4'] <- 'italic("dSys_4")'
pdat$name[pdat$name == 'd_Sys_incr5'] <- 'italic("dSys_5")'
pdat$name[pdat$name == 'd_Sys_incr6'] <- 'italic("dSys_6")'
pdat$name[pdat$name == 'd_Sys_incr7'] <- 'italic("dSys_7")'

pdat$name[pdat$name == 'fraction_critical'] <- 'italic("f")[italic("C")]'
pdat$name[pdat$name == 'fraction_critical_incr1'] <- 'italic("f")[italic("C_1")]'
pdat$name[pdat$name == 'fraction_critical_incr2'] <- 'italic("f")[italic("C_2")]'
pdat$name[pdat$name == 'fraction_critical_incr3'] <- 'italic("f")[italic("C_3")]'

pdat$name[pdat$name == 'fraction_dead'] <- 'italic("f")[italic("D")]'
pdat$name[pdat$name == 'fraction_hospitalized'] <- 'italic("f")[italic("H")]'
pdat$name[pdat$name == 'fraction_severe'] <- 'italic("f")[italic("Ss")]'
pdat$name[pdat$name == 'fraction_symptomatic'] <- 'italic("f")[italic("S")]'

pdat$name[pdat$name == 'recovery_time_asymp'] <- 'gamma[1][italic("A")]'
pdat$name[pdat$name == 'recovery_time_crit'] <- 'gamma[1][italic("C")]'
pdat$name[pdat$name == 'recovery_time_hosp'] <- 'gamma[1][italic("H")]'

pdat$name[pdat$name == 'time_to_critical'] <- 'italic("t")[italic("C")]'
pdat$name[pdat$name == 'time_to_death'] <- 'italic("t")[italic("D")]'
pdat$name[pdat$name == 'time_to_hospitalization'] <- 'italic("t")[italic("H")]'
pdat$name[pdat$name == 'time_to_infectious'] <- 'italic("t")[italic("ASP")]'
pdat$name[pdat$name == 'time_to_symptoms'] <- 'italic("t")[italic("S")]'
pdat$name[pdat$name == 'reduced_inf_of_det_cases'] <- 'delta[0]'

pdat$value <- round(pdat$value, 2)

pplotB <- ggplot(data = pdat) +
  geom_density(aes(x = value, fill = trigger_activated, col = trigger_activated), alpha = 0.4) +
  facet_wrap(~name, scales = "free", ncol = 4, labeller = label_parsed) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  customTheme +
  labs(x = "value", y = "Density",
       color = "trigger\nactivated", fill = "trigger\nactivated") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(panel.spacing = unit(1, "lines"))


plegend <- get_legend(pplotB)
pplotA <- pplotA + theme(legend.position = 'None')
pplotB <- pplotB + theme(legend.position = 'None')
pplot <- plot_grid(pplotB, pplotA, ncol = 2, rel_widths = c(1, 0.4), labels = c('A', 'B'), align = 'hv')
pplot <- plot_grid(pplot, plegend, ncol = 2, rel_widths = c(1, 0.1))

f_save_plot(
  plot_name = paste0("S1_fig_12"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 20, height = 14
)

if(cleanEnv)rm(list = ls())