# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 6

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection <- TRUE
if (trace_selection) fig_dir = fig_dir_traces


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
      f_trigger_dat() %>%
      dplyr::mutate(time_since_trigger = date - triggerDate,
                    time_since_trigger = round(time_since_trigger, 0))

    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

p6dat <- f_combineData(exp_names = c(exp_names_50_delay1, exp_names_100_delay1,
                                     exp_names_50_delay7, exp_names_100_delay7),
                       trace_selection = trace_selection)

above_threshold <- p6dat %>%
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


print(counterfactual_exps)
dat_list <- list()
for (exp_name in counterfactual_exps) {
  print(exp_name)
  dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
    exp_name, fname = "trajectoriesDat.csv",
    sim_dir, add_peak_cols = FALSE, add_trigger_cols = FALSE, addRt = FALSE) %>%
    dplyr::filter(date >= baseline_date & date <= sim_end_date) %>%
    dplyr::select(scen_num, sample_num, exp_name, date, reopen, crit_det) %>%
    mutate(date = as.Date(date)) %>%
    dplyr::select(scen_num, sample_num, exp_name, date, reopen, crit_det) %>%
    ungroup() %>%
    group_by(scen_num, exp_name, reopen) %>%
    filter(crit_det >= 516) %>%
    mutate(date_min = min(date, na.rm = TRUE),
           date_max = max(date, na.rm = TRUE)) %>%
    filter(date == min(date))
}
above_threshold_counter <- dat_list %>% bind_rows()
rm(dat_list)

above_threshold_counter$days_above = as.numeric(as.Date(above_threshold_counter$date_max) - as.Date(above_threshold_counter$date_min))
nomitigation_50perc <- above_threshold_counter %>%
  filter(reopen == "50perc") %>%
  ungroup() %>%
  summarize(days_above = mean(days_above))
nomitigation_100perc <- above_threshold_counter %>%
  filter(reopen == "100perc") %>%
  ungroup() %>%
  summarize(days_above = mean(days_above))
nomitigation_50perc <- nomitigation_50perc$days_above
nomitigation_100perc <- nomitigation_100perc$days_above


above_threshold$rollback <- factor(above_threshold$rollback,
                                   levels = c("pr2", "pr4", "pr6", "pr8"),
                                   labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

above_thresholdAggr$rollback <- factor(above_thresholdAggr$rollback,
                                       levels = c("pr2", "pr4", "pr6", "pr8"),
                                       labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

## For supplement
p6Abar <- ggplot(data = subset(above_thresholdAggr, delay == "1daysdelay")) +
  geom_bar(aes(x = as.factor(capacity_multiplier * 100), y = n.val, fill = reopen,
               group = interaction(rollback, reopen)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat = "identity") +
  facet_grid(~rollback) +
  theme_minimal() +
  customTheme +
  scale_color_manual(values = transm_scen_cols) +
  scale_fill_manual(values = transm_scen_cols) +
  labs(x = "ICU occupancy threshold to trigger mitigation (%)",
       y = "number of trajectories\nabove capacity") +
  theme(legend.position = "none")

p6Bbar <- ggplot(data = subset(above_thresholdAggr, reopen == "100perc")) +
  geom_bar(aes(x = as.factor(capacity_multiplier * 100), y = n.val, fill = delay,
               group = interaction(rollback, delay)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat = "identity") +
  facet_grid(~rollback) +
  theme_minimal() +
  customTheme +
  scale_color_manual(values = c("#dc362d", "firebrick4")) +
  scale_fill_manual(values = c("#dc362d", "firebrick4")) +
  labs(x = "ICU occupancy threshold to trigger mitigation (%)",
       y = "number of trajectories\nabove capacity") +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

p6bar <- plot_grid(p6Abar, p6Bbar, ncol = 1, labels = c("A", "B"))
f_save_plot(
  plot_name = paste0("Fig6_bar_supp"), pplot = p6bar,
  plot_dir = file.path(fig_dir), width = 10, height = 6, scale = 0.8)

p6A <- ggplot(data = subset(above_threshold, delay == "1daysdelay")) +
  geom_jitter(aes(x = as.factor(capacity_multiplier * 100), y = days_above, fill = reopen, shape = reopen, alpha = reopen,
                  group = interaction(rollback, reopen)), position = position_jitterdodge(), col = 'black', size = 0.9) +
  geom_pointrange(data = subset(above_thresholdAggr, n.val >= 10 & delay == "1daysdelay"),
                  aes(x = as.factor(capacity_multiplier * 100), y = mean, ymin = mean, ymax = mean, fill = reopen,
                      group = interaction(rollback, reopen)), stat = 'identity',
                  position = position_dodge(1), shape = 21, color = "black", size = 0.6) +
  theme_minimal() +
  scale_color_manual(values = transm_scen_cols) +
  scale_fill_manual(values = transm_scen_cols) +
  geom_hline(yintercept = c(nomitigation_50perc), color = transm_scen_cols[2]) +
  geom_hline(yintercept = c(nomitigation_100perc), color = transm_scen_cols[1]) +
  scale_shape_manual(values = c(21, 21)) +
  scale_alpha_manual(values = c(0.5, 0.5)) +
  scale_y_continuous(breaks = seq(0, 110, 30), labels = seq(0, 110, 30)) +
  facet_grid(~rollback) +
  labs(x = "ICU occupancy threshold to trigger mitigation (%)",
       y = "number of days above capacity") +
  theme(legend.position = "none")

f_save_plot(
  plot_name = paste0("Fig6A"), pplot = p6A,
  plot_dir = file.path(fig_dir), width = 10, height = 3, scale = 0.8
)

for (reopen_level in c("100perc", "50perc")) {
  p6B <- ggplot(data = subset(above_threshold, reopen == reopen_level)) +
    geom_jitter(aes(x = as.factor(capacity_multiplier * 100), y = days_above, fill = delay, shape = delay, alpha = delay,
                    group = interaction(rollback, delay)), position = position_jitterdodge(), col = 'black', size = 0.9) +
    geom_pointrange(data = subset(above_thresholdAggr, n.val >= 10 & reopen == reopen_level),
                    aes(x = as.factor(capacity_multiplier * 100), y = mean, ymin = mean, ymax = mean, fill = delay,
                        group = interaction(rollback, delay)), stat = 'identity',
                    position = position_dodge(1), shape = 21, color = "black", size = 0.6) +
    theme_minimal() +
    scale_color_manual(values = c("#dc362d", "firebrick4")) +
    scale_fill_manual(values = c("#dc362d", "firebrick4")) +
    #geom_hline(yintercept=c(nomitigation_50perc,nomitigation_100perc)) +
    scale_shape_manual(values = c(21, 21)) +
    scale_alpha_manual(values = c(0.5, 0.5)) +
    scale_y_continuous(breaks = seq(0, 110, 30), labels = seq(0, 110, 30)) +
    facet_grid(~rollback) +
    labs(x = "ICU occupancy threshold to trigger mitigation (%)",
         y = "number of days above capacity") +
    theme(legend.position = "none", panel.grid.major.x = element_blank())

  f_save_plot(
    plot_name = paste0("Fig6B_", reopen_level), pplot = p6B,
    plot_dir = file.path(fig_dir), width = 10, height = 3, scale = 0.8
  )
}

fwrite(above_threshold_counter, file.path(fig_dir, "csv", "p6dat_counter.csv"))
fwrite(above_threshold, file.path(fig_dir, "csv", "p6dat.csv"))
fwrite(above_thresholdAggr, file.path(fig_dir, "csv", "p6dat_aggr.csv"))


#### For text
above_threshold <- fread(file.path(fig_dir, "csv", "p6dat.csv"))
above_thresholdAggr <- fread(file.path(fig_dir, "csv", "p6dat_aggr.csv"))

above_thresholdAggr %>%
  filter(delay == "1daysdelay") %>%
  group_by(reopen, rollback) %>%
  summarize(mean = mean(mean))


above_thresholdAggr %>%
  select(rollback, capacity_multiplier, reopen, delay, mean) %>%
  group_by(rollback, reopen, delay) %>%
  summarize(mean = mean(mean)) %>%
  pivot_wider(names_from = delay, values_from = mean) %>%
  mutate(delay_diff = `1daysdelay` - `7daysdelay`)
