# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 test  probabilities weighted by fitting likelihood

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

exp_name <- '50perc_1daysdelay_pr8_triggeredrollback_reopen'  # selected test experiment
tempdat <- f_load_sim_data(exp_name = exp_name, sim_dir = sim_dir,
                           fname = "trajectoriesDat_region_11_trimfut.csv",
                           add_peak_cols = TRUE, addRt = FALSE, trace_selection = trace_selection) %>%
  filter(date >= baseline_date & date <= sim_end_date) %>%
  dplyr::group_by(exp_name, group_id) %>%
  filter(trigger_activated == 1 & date == date_peak) %>%
  dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                date, date_peak, trigger_activated, triggerDate, crit_det, crit_det_peak)


### add likelihood from fitting step
traces <- fread(file.path(sim_dir, "sample_num_traces_all.csv"))

traces_dat <- fread(file.path(sim_dir, "traces_dat_all.csv"))
traces_dat$nll <- 1 / traces_dat$nll
traces_dat$ll_wt <- traces_dat$nll / mean(traces_dat$nll)
sum(traces_dat$ll_wt)
tempdat <- tempdat %>% left_join(traces_dat)

groupVars <- c('rollback', 'delay', 'reopen', 'capacity_multiplier')
dat_prob_raw <- f_sample_trajectories(tempdat, groupVars = groupVars, weighted = T)


groupVars <- c('rollback', 'delay', 'reopen', 'capacity_multiplier', 'wt')
plotdat <- dat_prob_raw %>%
  rename(above_y__unw = above_y,
         above_y__wt = above_y_wt,
         perc_above__unw = perc_above,
         perc_above__wt = perc_above_wt) %>%
  pivot_longer(cols = c(above_y__unw, above_y__wt, perc_above__unw, perc_above__wt)) %>%
  separate(name, into = c('outcome', 'wt'), sep = '__') %>%
  pivot_wider(names_from = outcome, values_from = value) %>%
  dplyr::group_by_at(groupVars) %>%
  dplyr::summarize(n.val = n(),
                   prob_lower = min(perc_above),
                   prob_upper = max(perc_above),
                   prob_median = median(perc_above),
                   prob = mean(perc_above))

ggplot(data = plotdat) +
  geom_vline(xintercept = 50, col = 'grey', alpha = 0.8) +
  geom_hline(yintercept = 0.5, col = 'grey', alpha = 0.8) +
  geom_ribbon(aes(
    x = capacity_multiplier * 100,
    y = prob,
    ymin = prob_lower,
    ymax = prob_upper,
    fill = wt,
    group = interaction(rollback, wt)
  ), alpha = 0.3) +
  geom_line(aes(
    x = capacity_multiplier * 100, y = prob,
    col = wt,
    group = interaction(rollback, wt)
  ), size = 1.3) +
  scale_y_continuous(
    lim = c(0, 1.01),
    breaks = seq(0, 1, 0.2),
    labels = seq(0, 1, 0.2) * 100,
    minor_breaks = seq(0, 1, 0.1)
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), lim = c(0, 100)) +
  customTheme +
  labs(title = exp_name)

dat_prob_raw %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  #geom_line(aes(x = perc_above, y = perc_above)) +
  geom_point(aes(
    x = perc_above, y = perc_above_wt,
    fill = capacity_multiplier,
    group = interaction(resample_n, capacity_multiplier)
  ), shape = 21, size = 3, alpha = 0.8, col = 'black') +
  scale_x_continuous(breaks = seq(0, 1, 0.20), labels = seq(0, 1, 0.20), lim = c(-0.01, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1, 0.20), labels = seq(0, 1, 0.20), lim = c(-0.01, 1.05)) +
  scale_fill_viridis_c() +
  labs(title = exp_name, subtitle = '', fill = 'Capacity\nthreshold',
       x = 'perc_above', y = 'perc_above_wt') +
  customTheme



