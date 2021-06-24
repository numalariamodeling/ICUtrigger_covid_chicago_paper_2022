# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Table 7

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

trace_selection <- TRUE
capacity_multipliers <- c(0.4, 0.6, 0.8, 1)
exp_names <- list.dirs(sim_dir, full.names = FALSE)
exp_names <- exp_names[grep("triggeredrollback_reopen", exp_names)]

column_names <- c("ICU_peak", "ICU_peak_date", "trigger_to_peak",
                  "ICUoverflow", "ICUoverflow_date", "trigger_to_overflow")
scen_table <- matrix(NA, length(exp_names) * length(capacity_multipliers), length(column_names) + 2)
colnames(scen_table) <- c("exp_name", "capacity_multiplier", column_names)
scen_table <- as.data.frame(scen_table)

dat_list <- list()
for (exp_name in exp_names) {
  print(exp_name)
  dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
    exp_name, fname = "trajectoriesDat_region_11_trimfut.csv", sim_dir,
    add_peak_cols = TRUE, add_trigger_cols = TRUE, addRt = FALSE, trace_selection = trace_selection) %>%
    filter(date <= sim_end_date &
             date_peak <= last_plot_date &
             capacity_multiplier %in% capacity_multipliers) %>%
    dplyr::group_by(exp_name, group_id) %>%
    dplyr::mutate(rel_occupancy = crit_det / 516,
                  rel_occupancy_peak = crit_det_peak / 516,
                  time_since_reopen = as.numeric(date - baseline_date),
                  time_to_peak_after_reopen = as.numeric(date_peak - baseline_date),
                  time_to_trigger_after_reopen = as.numeric(triggerDate - baseline_date),
                  time_since_trigger = round(as.numeric(date - triggerDate), 0),
                  time_to_peak_after_trigger = round(as.numeric(date_peak - triggerDate), 0)) %>%
    dplyr::filter(trigger_activated == 1) %>%
    ungroup() %>%
    dplyr::select(exp_name, capacity_multiplier, rel_occupancy_peak, time_to_peak_after_reopen,
                  time_to_trigger_after_reopen, time_to_peak_after_trigger) %>%
    pivot_longer(cols = -c(exp_name, capacity_multiplier), names_to = "outcome") %>%
    dplyr::group_by(exp_name, capacity_multiplier, outcome) %>%
    dplyr::summarise(n = n(),
                     median = median(value),
                     lower = quantile(value, probs = 0.05, na.rm = TRUE),
                     upper = quantile(value, probs = 0.95, na.rm = TRUE))
}

dat <- dat_list %>%
  bind_rows() %>%
  filter(outcome != "group_id") %>%
  mutate(exp_name = exp_name) %>%
  separate(exp_name, into = c("reopen", "delay", "mitigation"), sep = "_")

dat$mitigation <- factor(dat$mitigation,
                         levels = c("pr2", "pr4", "pr6", "pr8"),
                         labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

dat$delay <- factor(dat$delay,
                    levels = c("1daysdelay", "7daysdelay"),
                    labels = c("1 day", "7 days"))

dat$reopen <- factor(dat$reopen,
                     levels = c("100perc", "50perc"),
                     labels = c("High", "Low"))

dat <- dat %>%
  pivot_wider(names_from = outcome,
              values_from = c('median', 'lower', 'upper'),
              names_glue = "{outcome}_{.value}") %>%
  select(order(colnames(.))) %>%
  select(reopen, mitigation, delay, capacity_multiplier, everything())

fwrite(dat, file.path(git_dir, "out", "S1_table_7.csv"))
