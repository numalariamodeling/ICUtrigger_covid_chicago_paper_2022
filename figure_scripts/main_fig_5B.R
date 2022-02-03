# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 5B

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection = TRUE
if (trace_selection) fig_dir = fig_dir_traces

f_combineData <- function(exp_names, sim_end_date, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    tempdat <- f_load_sim_data(exp_name = exp_name, sim_dir = sim_dir,
                               fname = "trajectoriesDat_region_11_trimfut.csv",
                               add_peak_cols = TRUE, addRt = FALSE, trace_selection = trace_selection) %>%
      filter(date >= baseline_date & date <= sim_end_date) %>%
      dplyr::group_by(exp_name, group_id) %>%
      filter(trigger_activated == 1 & date == date_peak) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date, date_peak, trigger_activated, triggerDate, crit_det, crit_det_peak)

    groupVars = c('rollback', 'delay', 'reopen', 'capacity_multiplier')
    dat_prob_raw <- f_sample_trajectories(tempdat, groupVars = groupVars)
    dat_list[[length(dat_list) + 1]] <- dat_prob_raw %>%
      dplyr::group_by_at(groupVars) %>%
      dplyr::summarize(n.val = n(),
                       prob_lower = min(perc_above),
                       prob_upper = max(perc_above),
                       prob_median = median(perc_above),
                       prob = mean(perc_above))

    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

p5Bdat <- f_combineData(exp_names = c(exp_names_50_delay1, exp_names_100_delay1,
                                      exp_names_50_delay7, exp_names_100_delay7),
                        sim_end_date = sim_end_date,
                        trace_selection = trace_selection)


p5Bdat$rollback <- factor(p5Bdat$rollback,
                          levels = c("pr2", "pr4", "pr6", "pr8"),
                          labels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

p5Bdat$delay <- factor(p5Bdat$delay,
                       levels = c("1daysdelay", "7daysdelay"),
                       labels = c("immediate mitigation:\n1 day after threshold reached",
                                  "delayed mitigation:\n7 days after threshold reached"))

p5Bdat$reopen <- factor(p5Bdat$reopen,
                        levels = c("100perc", "50perc"),
                        labels = c("High increase in transmission:\nRt approx. 1.28",
                                   "Low increase in transmission:\nRt approx. 1.16"))

p5B <- f_prob_plot_base(dat = subset(p5Bdat, rollback != "counterfactual")) +
  facet_grid(reopen ~ delay) +
  geom_hline(yintercept = 0.25, col = 'darkgrey', size = 0.8)
#print(p5B)

fwrite(p5Bdat, file.path(fig_dir, "csv", "p5Bdat.csv"))

f_save_plot(
  plot_name = paste0("Fig5B"), pplot = p5B,
  plot_dir = file.path(fig_dir), width = 12, height = 10
)

### For text
p5Bdat <- fread(file.path(fig_dir, "csv", "p5Bdat.csv"))


p5Bdat %>%
  filter(delay == "immediate mitigation:\n1 day after threshold reached") %>%
  ungroup() %>%
  select(reopen, delay, rollback, capacity_multiplier, prob) %>%
  pivot_wider(names_from = reopen, values_from = prob) %>%
  mutate(prob_diff = `High increase in transmission:\nRt approx. 1.28` -
    `Low increase in transmission:\nRt approx. 1.16`) %>%
  group_by(delay) %>%
  summarize(mean = mean(prob_diff, na.rm = TRUE),
            q5 = round(quantile(prob_diff, probs = 0.05, na.rm = TRUE), 1),
            q95 = round(quantile(prob_diff, probs = 0.95, na.rm = TRUE), 1))

p5Bdat %>%
  filter(delay == "immediate mitigation:\n1 day after threshold reached") %>%
  ungroup() %>%
  select(reopen, delay, rollback, capacity_multiplier, prob) %>%
  pivot_wider(names_from = reopen, values_from = prob) %>%
  mutate(prob_diff = `High increase in transmission:\nRt approx. 1.28` -
    `Low increase in transmission:\nRt approx. 1.16`) %>%
  group_by(delay, rollback) %>%
  summarize(mean = mean(prob_diff, na.rm = TRUE),
            q5 = round(quantile(prob_diff, probs = 0.05, na.rm = TRUE), 1),
            q95 = round(quantile(prob_diff, probs = 0.95, na.rm = TRUE), 1))

p5Bdat %>%
  filter(delay == "immediate mitigation:\n1 day after threshold reached") %>%
  ungroup() %>%
  select(reopen, delay, rollback, capacity_multiplier, prob) %>%
  group_by(reopen, rollback) %>%
  summarize(mean = mean(prob, na.rm = TRUE),
            q5 = round(quantile(prob, probs = 0.05, na.rm = TRUE), 1),
            q95 = round(quantile(prob, probs = 0.95, na.rm = TRUE), 1))

p5Bdat %>%
  filter(delay == "immediate mitigation:\n1 day after threshold reached") %>%
  group_by(reopen, delay, rollback) %>%
  summarize(mean = mean(prob),
            prob_lower = mean(prob_lower),
            prob_upper = mean(prob_upper))


p5Bdat %>%
  filter(reopen == "High increase in transmission:\nRt approx. 1.28" &
           rollback == "moderate (40%)") %>%
  group_by(reopen, delay, rollback) %>%
  summarize(mean = mean(prob),
            prob_lower = mean(prob_lower),
            prob_upper = mean(prob_upper))

p5Bdat %>%
  filter(reopen != "High increase in transmission:\nRt approx. 1.28" &
           rollback == "very strong (80%)" &
           capacity_multiplier == 0.8)

p5Bdat %>%
  filter(reopen == "High increase in transmission:\nRt approx. 1.28" &
           rollback == "very strong (80%)" &
           capacity_multiplier == 0.6)