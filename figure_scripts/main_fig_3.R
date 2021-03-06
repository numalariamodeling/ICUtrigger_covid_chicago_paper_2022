# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 3
# Data files: capacity_by_covid_region.csv, capacity_weekday_average_20200915.csv

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

ref_dat <- fread(file.path(data_path, 'COVID-19_ICU_Chicago_2020.csv')) %>%
  mutate(assumed_capacity = assumed_capacity)

f_combineData <- function(exp_names, sim_end_date, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name, fname = "trajectoriesDat.csv", sim_dir,
      add_peak_cols = TRUE, add_trigger_cols = FALSE, addRt = TRUE, trace_selection = trace_selection) %>%
      filter(date <= sim_end_date & date_peak <= sim_end_date)
  }
  dat <- dat_list %>%
    bind_rows() %>%
    filter(date <= last_plot_date) %>%
    as.data.table()
  rm(dat_list)
  return(dat)
}

print(counterfactual_exps)

run_data_from_sim <- F
if (run_data_from_sim) {
  dat <- f_combineData(exp_names = counterfactual_exps, sim_end_date = sim_end_date, trace_selection = trace_selection)

  dim(dat)
  table(dat$capacity_multiplier)
  table(dat$reopen)
  tapply(dat$date_peak, dat$reopen, summary)

  f_n_scenarios(dat)

  ### Ki
  dat %>%
    filter(date >= baseline_date & date <= as.Date("2020-10-15")) %>%
    group_by(reopen) %>%
    summarize(ki_min = min(Ki_t),
              ki_max = max(Ki_t)) %>%
    mutate(incr = (ki_max / ki_min))

  ### Reach peak before Dec 2020
  dat %>%
    filter(date >= baseline_date) %>%
    dplyr::filter(crit_det == max(crit_det)) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::group_by(reopen) %>%
    dplyr::mutate(peak_before_2021 = ifelse(date <= last_plot_date, 1, 0)) %>%
    dplyr::group_by(group_id, reopen, peak_before_2021) %>%
    add_tally() %>%
    dplyr::group_by(reopen, peak_before_2021) %>%
    dplyr::summarize(n = sum(n),
                     date = mean(date),
                     crit_det = mean(crit_det)) %>%
    dplyr::mutate(rel_occupancy = crit_det / 516) %>%
    arrange(peak_before_2021)


  ### Reach capacity
  dat %>%
    filter(date >= baseline_date) %>%
    dplyr::group_by(reopen) %>%
    dplyr::mutate(peak_above_capacity = ifelse(crit_det_peak >= 516, 1, 0)) %>%
    dplyr::group_by(group_id, reopen, peak_above_capacity) %>%
    add_tally() %>%
    dplyr::group_by(reopen, peak_above_capacity) %>%
    dplyr::summarize(n = sum(n),
                     date = mean(date),
                     crit_det = mean(crit_det)) %>%
    dplyr::mutate(rel_occupancy = crit_det / 516) %>%
    arrange(peak_above_capacity)

  dat <- dat %>% mutate(peak_beforeDec = ifelse(date_peak <= last_plot_date, 1, 0))
}
dat <- fread(file.path(fig_dir, "csv", "p3A_dat.csv"))

dat %>%
  dplyr::select(group_id, scen_num, exp_name, reopen, peak_beforeDec) %>%
  unique() %>%
  group_by(peak_beforeDec) %>%
  tally()


p3A <- ggplot(data = subset(dat, date >= first_plot_date & date <= last_plot_date)) +
  geom_line(aes(x = date, y = crit_det, group = interaction(scen_num, reopen), col = reopen), alpha = 0.5) +
  geom_point(data = subset(ref_dat, date <= baseline_date), aes(x = date, y = confirmed_covid_icu), shape = 21, fill = 'black', size = 1) +
  geom_point(data = subset(ref_dat, date >= baseline_date & date <= last_plot_date), aes(x = date, y = confirmed_covid_icu), shape = 21, fill = 'lightgrey', size = 1) +
  geom_line(data = subset(ref_dat, date <= baseline_date), aes(x = date, y = icu_availforcovid_7avrg), col = capacitycolor, alpha = 1, size = 1.2) +
  geom_line(data = subset(ref_dat, date >= baseline_date & date <= last_plot_date), aes(x = date, y = assumed_capacity), linetype = 'dashed', col = capacitycolor, alpha = 1, size = 1.2) +
  geom_line(data = subset(ref_dat, date >= baseline_date & date <= last_plot_date), aes(x = date, y = icu_availforcovid_7avrg), col = capacitycolor, alpha = 0.5, size = 1.2) +
  scale_color_manual(values = transm_scen_cols) +
  geom_vline(xintercept = c(baseline_date)) +
  geom_vline(xintercept = c(last_plot_date), linetype = 'dashed') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "ICU occupancy", color = "") +
  theme(legend.position = "none", , panel.grid.minor = element_blank())

p3B_dat <- subset(dat, date >= first_plot_date & date <= last_plot_date) %>%
  group_by(date, reopen) %>%
  summarize(Ki_t = mean(Ki_t))

p3B_dat %>%
  ungroup() %>%
  group_by(reopen) %>%
  filter(date == max(date))

p3B_dat %>%
  ungroup() %>%
  group_by(reopen) %>%
  filter(date == min(date))

p3B <- ggplot(p3B_dat) +
  geom_line(aes(x = date, y = Ki_t, group = interaction(reopen), col = reopen), alpha = 0.5) +
  scale_color_manual(values = transm_scen_cols) +
  geom_vline(xintercept = c(baseline_date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Transmission rate", color = "") +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_y_continuous(lim = c(0, 1.15), breaks = c(0, 0.170, 0.225, 1.14))


p3C_dat <- dat %>%
  filter(date >= first_plot_date & date <= last_plot_date) %>%
  dplyr::select(date, rt_median, rt_lower, scen_num, rt_upper, reopen)

p3C <- ggplot(data = p3C_dat) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper, group = interaction(scen_num, reopen),
                  fill = reopen), alpha = 0.3) +
  geom_line(aes(x = date, y = rt_median, group = interaction(scen_num, reopen), col = reopen), alpha = 0.5) +
  scale_color_manual(values = transm_scen_cols) +
  geom_vline(xintercept = c(baseline_date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  labs(x = "", y = "Rt", color = "")


p3C_split <- gg.gap(plot = p3C, segments = list(c(1.5, 1.6)), ylim = c(0.5, 6), rel_heights = c(3, 0.2, 1.5),
                    tick_width = c(0.25, 2), margin = c(top = 2, right = 1, bottom = 1, left = 1))

p3BC <- plot_grid(p3B, p3C, ncol = 2, align = "hv", labels = c("B", "C"))
pplot <- plot_grid(p3A, p3BC, rel_heights = c(1, 0.5), ncol = 1, labels = c("A", ""))

f_save_plot(
  plot_name = paste0("Fig3"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 10
)
f_save_plot(
  plot_name = paste0("Fig3C_split"), pplot = p3C_split,
  plot_dir = file.path(fig_dir), width = 10, height = 7, scale = 0.7
)

## Save csvs
fwrite(dat, file.path(fig_dir, "csv", "p3A_dat.csv"))
fwrite(p3B_dat, file.path(fig_dir, "csv", "p3B_dat.csv"))
fwrite(p3C_dat, file.path(fig_dir, "csv", "p3C_dat.csv"))


#----------------------
### For text
#----------------------
dat <- fread(file.path(fig_dir, "csv", "p3A_dat.csv"))
p3B_dat <- fread(file.path(fig_dir, "csv", "p3B_dat.csv"))
p3C_dat <- fread(file.path(fig_dir, "csv", "p3C_dat.csv"))

### ICU occupancy
dat %>%
  dplyr::filter(date >= trigger_min_date) %>%
  dplyr::group_by(exp_name, group_id, scen_num, sample_num, reopen) %>%
  filter(crit_det == max(crit_det)) %>%
  select(crit_det, exp_name, group_id, scen_num, sample_num, reopen, date) %>%
  dplyr::group_by(reopen) %>%
  dplyr::summarize(crit_det_mean = mean(crit_det, na.rm = TRUE),
                   crit_det_median = median(crit_det, na.rm = TRUE),
                   crit_det_q5 = quantile(crit_det, probs = 0.05, na.rm = TRUE),
                   crit_det_q95 = quantile(crit_det, probs = 0.95, na.rm = TRUE))


dat %>%
  dplyr::filter(date >= trigger_min_date) %>%
  dplyr::group_by(exp_name, group_id, scen_num, sample_num, reopen) %>%
  filter(crit_det == max(crit_det)) %>%
  select(crit_det, exp_name, group_id, scen_num, sample_num, reopen, date) %>%
  dplyr::group_by() %>%
  dplyr::summarize(crit_det_mean = mean(crit_det, na.rm = TRUE),
                   crit_det_median = median(crit_det, na.rm = TRUE),
                   crit_det_q5 = quantile(crit_det, probs = 0.05, na.rm = TRUE),
                   crit_det_q95 = quantile(crit_det, probs = 0.95, na.rm = TRUE))


### increase in Ki
p3B_dat %>%
  dplyr::filter(date >= baseline_date & date <= trigger_min_date + 30) %>%
  dplyr::group_by(reopen) %>%
  dplyr::summarize(Ki_t_min = min(Ki_t),
                   Ki_t_max = max(Ki_t)) %>%
  mutate(rel_incr = (Ki_t_max - Ki_t_min) / Ki_t_min)

### reduction in Ki
p3B_dat %>%
  dplyr::filter(date <= as.Date(sim_end_date)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(Ki_t = mean(Ki_t)) %>%
  dplyr::group_by(Ki_t) %>%
  dplyr::summarize(date = min(date)) %>%
  arrange(date)


### reduction in Rt
p3C_dat %>%
  dplyr::filter(!is.na(rt_median)) %>%
  dplyr::group_by(reopen, scen_num) %>%
  dplyr::filter(date == min(date) | (date <= as.Date(sim_end_date) & rt_median == min(rt_median))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(rt_median_mean = mean(rt_median, na.rm = TRUE),
                   rt_median_q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
                   rt_median_q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE))

### baseline in Rt before reopening
p3C_dat %>%
  dplyr::filter(as.character(date) == as.character(baseline_date)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(rt_median_mean = mean(rt_median, na.rm = TRUE),
                   rt_median_q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
                   rt_median_q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE))


### baseline in Rt after reopening
p3C_dat %>%
  dplyr::filter(as.character(date) == as.character("2020-10-01")) %>%
  dplyr::group_by(reopen, date, scen_num) %>%
  filter(rt_median == max(rt_median, na.rm = TRUE)) %>%
  dplyr::group_by(reopen, date) %>%
  dplyr::summarize(rt_median_mean = mean(rt_median, na.rm = TRUE),
                   rt_median_q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
                   rt_median_q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE))

### icu_availforcovid
summary(ref_dat[ref_dat$date <= as.Date("2020-12-31"), "icu_availforcovid"])
summary(ref_dat[ref_dat$date <= as.Date("2020-12-31"), "icu_availforcovid_7avrg"])
summary(ref_dat$date)

ref_dat %>%
  filter(date <= as.Date("2020-12-31")) %>%
  arrange(icu_availforcovid) %>%
  head()

if (cleanEnv)rm(list = ls())