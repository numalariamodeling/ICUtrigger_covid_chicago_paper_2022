# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 13

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

trace_selection = TRUE
reopen = "100perc"

if (trace_selection) fig_dir = fig_dir_traces
if (reopen == "100perc") {
  exp_names <- c(exp_names_100_delay1, exp_names_100_delay7)
}
if (reopen == "50perc") {
  exp_names <- c(exp_names_50_delay1, exp_names_50_delay7)
}

f_combineData <- function(exp_names, sim_end_date, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name, fname = "trajectoriesDat_region_11_trimfut.csv", sim_dir,
      add_peak_cols = TRUE, add_trigger_cols = TRUE, addRt = TRUE, trace_selection = trace_selection) %>%
      filter(date <= sim_end_date &
               date_peak <= last_plot_date &
               capacity_multiplier == 0.6) %>%
      f_add_popvars() %>%
      dplyr::group_by(exp_name, group_id) %>%
      dplyr::mutate(rel_occupancy = crit_det / 516,
                    rel_occupancy_peak = crit_det_peak / 516,
                    time_since_reopen = as.numeric(date - as.Date("2020-09-01")),
                    time_to_peak_after_reopen = as.numeric(date_peak - as.Date("2020-09-01")),
                    time_to_trigger_after_reopen = as.numeric(triggerDate - as.Date("2020-09-01")),
                    time_since_trigger = round(as.numeric(date - triggerDate), 0),
                    time_to_peak_after_trigger = round(as.numeric(date_peak - triggerDate), 0)) %>%
      filter(trigger_activated == 1)
  }
  dat <- dat_list %>% bind_rows() %>% as.data.table()
  rm(dat_list)
  return(dat)
}

#### Mitigation dat
print(exp_names)
dat <- f_combineData(exp_names = exp_names, sim_end_date = sim_end_date, trace_selection = trace_selection)


triggerDat <- dat %>%
  f_trigger_dat() %>%
  dplyr::mutate(time_since_trigger = date - triggerDate,
                time_since_trigger = round(time_since_trigger, 0)) %>%
  filter(trigger_activated == 1)

plotdat <- triggerDat %>%
  ungroup() %>%
  mutate(time_since_trigger = round(time_since_trigger, 0)) %>%
  filter(time_since_trigger > -14 & !is.na(new_infected)) %>%
  dplyr::select(time_since_trigger, rollback, group_id, reopen, delay, capacity_multiplier, new_infected) %>%
  unique() %>%
  dplyr::group_by(time_since_trigger, rollback, reopen, delay, capacity_multiplier) %>%
  dplyr::summarize(
    n.val = n(),
    mean = mean(new_infected),
    median = median(new_infected),
    q5 = quantile(new_infected, probs = 0.05, na.rm = TRUE),
    q95 = quantile(new_infected, probs = 0.95, na.rm = TRUE)
  ) %>%
  mutate(time_since_trigger_wks = time_since_trigger / 7)


pplot <- ggplot(data = subset(plotdat, time_since_trigger_wks > -2 & time_since_trigger_wks <= 12)) +
  geom_ribbon(aes(x = time_since_trigger_wks, ymin = q5, ymax = q95,
                  group = interaction(rollback, capacity_multiplier),
                  fill = as.factor(rollback)), alpha = 0.3) +
  geom_line(aes(x = time_since_trigger_wks, y = mean,
                group = interaction(rollback, capacity_multiplier),
                col = as.factor(rollback)), alpha = 0.6, size = 0.9) +
  scale_fill_manual(values = mitigation_cols) +
  scale_color_manual(values = mitigation_cols) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = 'dashed') +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none") +
  labs(x = "weeks since ICU occupancy\nthreshold for action reached",
       y = "New infections", color = "", fill = "") +
  scale_x_continuous(breaks = seq(-2, 12, 1), labels = seq(-2, 12, 1), lim = c(-2, 12)) +
  facet_wrap(~delay, nrow = 1)

f_save_plot(
  plot_name = paste0("S1_fig_13", reopen), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 8, height = 4
)

