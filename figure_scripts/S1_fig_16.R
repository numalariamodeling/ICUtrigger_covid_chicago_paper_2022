# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 16

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

f_combineData <- function(exp_names, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    tempdat <- f_load_sim_data(exp_name = exp_name, sim_dir = sim_dir,
                               fname = "trajectoriesDat_region_11_trimfut.csv",
                               add_peak_cols = TRUE, addRt = FALSE, trace_selection = trace_selection) %>%
      filter(date >= baseline_date & date <= sim_end_date) %>%
      dplyr::group_by(exp_name, group_id) %>%
      filter(trigger_activated == 1 & capacity_multiplier %in% c(0.2, 0.4, 0.6, 0.8, 1)) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date, date_peak, trigger_activated, triggerDate, crit_det, crit_det_peak)

    dat_list[[length(dat_list) + 1]] <- tempdat %>%
      f_get_triggerDat() %>%
      dplyr::mutate(time_since_trigger = date - triggerDate,
                    time_since_trigger = round(time_since_trigger, 0))

    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

p16dat <- f_combineData(exp_names = c(exp_names_50_delay1, exp_names_100_delay1,
                                      exp_names_50_delay7, exp_names_100_delay7),
                        trace_selection = trace_selection)


above_threshold <- p16dat %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(scen_num, sample_num, capacity_multiplier, exp_name, date,
                group_id, reopen, delay, rollback, crit_det) %>%
  ungroup() %>%
  group_by(scen_num, exp_name, group_id, reopen, delay, capacity_multiplier, rollback) %>%
  filter(crit_det >= 516) %>%
  mutate(date_min = min(date, na.rm = TRUE),
         date_max = max(date, na.rm = TRUE),
         type = 'threshold',
         subtype = 'above_threshold') %>%
  filter(date == min(date))
above_threshold$days_above = as.numeric(as.Date(above_threshold$date_max) - as.Date(above_threshold$date_min))

above_thresholdAggr <- above_threshold %>%
  filter(capacity_multiplier %in% c(0.2, 0.4, 0.6, 0.8, 1)) %>%
  group_by(exp_name, reopen, delay, capacity_multiplier, rollback) %>%
  dplyr::summarize(
    n.val = n(),
    mean = mean(days_above),
    median = median(days_above),
    q5 = quantile(days_above, probs = 0.05, na.rm = TRUE),
    q95 = quantile(days_above, probs = 0.95, na.rm = TRUE)
  )


above_threshold$rollback <- factor(above_threshold$rollback,
                                   levels = c("pr2", "pr4", "pr6", "pr8"),
                                   labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

above_thresholdAggr$rollback <- factor(above_thresholdAggr$rollback,
                                       levels = c("pr2", "pr4", "pr6", "pr8"),
                                       labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))


p16Abar <- ggplot(data = subset(above_thresholdAggr, delay == "1daysdelay")) +
  geom_bar(aes(x = as.factor(capacity_multiplier * 100), y = n.val, fill = reopen,
               group = interaction(rollback, reopen)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat = "identity") +
  facet_grid(~rollback) +
  scale_color_manual(values = transm_scen_cols) +
  scale_fill_manual(values = transm_scen_cols) +
  labs(x = "ICU occupancy threshold to trigger mitigation (%)",
       y = "number of trajectories\nabove capacity") +
  theme(legend.position = "none")

p16Bbar <- ggplot(data = subset(above_thresholdAggr, reopen == "100perc")) +
  geom_bar(aes(x = as.factor(capacity_multiplier * 100), y = n.val, fill = delay,
               group = interaction(rollback, delay)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat = "identity") +
  facet_grid(~rollback) +
  scale_color_manual(values = delay_scen_cols) +
  scale_fill_manual(values = delay_scen_cols) +
  labs(x = "ICU occupancy threshold to trigger mitigation (%)",
       y = "number of trajectories\nabove capacity") +
  theme(legend.position = "none")

p16bar <- plot_grid(p16Abar, p16Bbar, ncol = 1, labels = c("A", "B"))
f_save_plot(
  plot_name = paste0("S1_fig_16"), pplot = p16bar,
  plot_dir = file.path(fig_dir), width = 10, height = 6, scale = 0.8
)

fwrite(above_threshold, file.path(fig_dir, "csv", "S1_fig_16.csv"))
fwrite(above_thresholdAggr, file.path(fig_dir, "csv", "S1_fig_16_aggr.csv"))
if (cleanEnv)rm(list = ls())