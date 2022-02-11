# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 20

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

f_combineData <- function(exp_names, sim_end_date, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name, fname = "trajectoriesDat_region_11_trimfut.csv", sim_dir,
      add_peak_cols = TRUE, add_trigger_cols = TRUE, addRt = TRUE, trace_selection = trace_selection) %>%
      filter(date <= sim_end_date &
               date_peak <= last_plot_date) %>%
      f_add_popvars() %>%
      dplyr::group_by(exp_name, group_id) %>%
      dplyr::mutate(rel_occupancy = crit_det / 516,
                    rel_occupancy_peak = crit_det_peak / 516,
                    time_since_reopen = as.numeric(date - as.Date("2020-09-01")),
                    time_to_peak_after_reopen = as.numeric(date_peak - as.Date("2020-09-01")),
                    time_to_trigger_after_reopen = as.numeric(triggerDate - as.Date("2020-09-01")),
                    time_since_trigger = round(as.numeric(date - triggerDate), 0),
                    time_to_peak_after_trigger = round(as.numeric(date_peak - triggerDate), 0)) %>%
      filter(trigger_activated == 1 & time_since_trigger <= 120)
  }
  dat <- dat_list %>% bind_rows() %>% as.data.table()
  rm(dat_list)
  return(dat)
}

# c(exp_names_50_delay1, exp_names_100_delay1, exp_names_50_delay7, exp_names_100_delay7)
dat <- f_combineData(exp_names = c(exp_names_50_delay1, exp_names_100_delay1),
                     sim_end_date = sim_end_date, trace_selection = trace_selection)


datAggr <- dat %>%
  group_by(date, triggerDate, time_since_trigger, reopen, delay, rollback, capacity_multiplier) %>%
  summarize(rt_median = mean(rt_median, na.rm = TRUE),
            Ki_t = mean(Ki_t, na.rm = TRUE),
            crit_det = mean(crit_det, na.rm = TRUE))

dat_triggerDate_prev2wk <- dat %>% filter(as.character(date) == as.character(triggerDate - 14))

table(dat$capacity_multiplier)
summary(dat$date)
tapply(dat$triggerDate, dat$reopen, summary)

##-----------------------------------
### Rt 14 days prior mitigation
Rt_before <- dat %>%
  filter(time_since_trigger == -14) %>%
  select(reopen, delay, rollback, scen_num, capacity_multiplier, rt_median) %>%
  rename(rt_median_before = rt_median)

Rt_after <- dat %>%
  filter(time_since_trigger > 0 & time_since_trigger <= 14) %>%
  group_by(reopen, delay, rollback, capacity_multiplier, scen_num) %>%
  filter(rt_median == min(rt_median, na.rm = TRUE)) %>%
  select(reopen, delay, rollback, capacity_multiplier, scen_num, rt_median) %>%
  rename(rt_median_after = rt_median)

Rt_diff <- Rt_before %>%
  left_join(Rt_after) %>%
  mutate(rt_relred = (1 - (rt_median_after / rt_median_before)) * 100)

Rt_diff %>%
  dplyr::group_by(reopen, delay, rollback, capacity_multiplier) %>%
  dplyr::summarize(rt_before_mean = mean(rt_median_before, na.rm = TRUE),
                   rt_before_q5 = quantile(rt_median_before, probs = 0.05, na.rm = TRUE),
                   rt_before_q95 = quantile(rt_median_before, probs = 0.95, na.rm = TRUE),
                   rt_after_mean = mean(rt_median_after, na.rm = TRUE),
                   rt_after_q5 = quantile(rt_median_after, probs = 0.05, na.rm = TRUE),
                   rt_after_q95 = quantile(rt_median_after, probs = 0.95, na.rm = TRUE),
                   rt_relred_mean = mean(rt_relred, na.rm = TRUE),
                   rt_relred_q5 = quantile(rt_relred, probs = 0.05, na.rm = TRUE),
                   rt_relred_q95 = quantile(rt_relred, probs = 0.95, na.rm = TRUE)) %>%
  fwrite(file.path(fig_dir, "csv", "S1_fig_20_Rt_diff_prv14wks.csv"))

Rt_diff %>%
  dplyr::group_by(reopen, delay, rollback, capacity_multiplier) %>%
  dplyr::summarize(rt_relred_mean = mean(rt_relred, na.rm = TRUE),
                   rt_relred_q5 = quantile(rt_relred, probs = 0.05, na.rm = TRUE),
                   rt_relred_q95 = quantile(rt_relred, probs = 0.95, na.rm = TRUE)) %>%
  pivot_wider(names_from = rollback, values_from = c(rt_relred_mean, rt_relred_q5, rt_relred_q95))

plotdat <- Rt_diff %>% mutate(rollback_num = as.numeric(gsub("pr", "", rollback)) * 10)
plotdat_aggr <- plotdat %>%
  dplyr::group_by(reopen, delay, rollback_num, capacity_multiplier) %>%
  dplyr::summarize(rt_median_before = mean(rt_median_before, na.rm = TRUE),
                   rt_relred_mean = mean(rt_relred, na.rm = TRUE),
                   rt_relred_q5 = quantile(rt_relred, probs = 0.05, na.rm = TRUE),
                   rt_relred_q95 = quantile(rt_relred, probs = 0.95, na.rm = TRUE))

### Add 0, 100 points for loess line
plotdat_aggr_0 <- plotdat_aggr %>% filter(rollback_num == 20)
plotdat_aggr_0$rollback_num <- 0
plotdat_aggr_0$rt_relred_mean <- 0
plotdat_aggr_0$rt_relred_q5 <- 0
plotdat_aggr_0$rt_relred_q95 <- 0
plotdat_aggr_100 <- plotdat_aggr %>% filter(rollback_num == 20)
plotdat_aggr_100$rollback_num <- 100
plotdat_aggr_100$rt_relred_mean <- 100
plotdat_aggr_100$rt_relred_q5 <- 100
plotdat_aggr_100$rt_relred_q95 <- 100
plotdat_aggr <- plotdat_aggr %>% rbind(plotdat_aggr_0, plotdat_aggr_100)
fwrite(plotdat_aggr, file.path(fig_dir, "csv", "S1_fig_20dat.csv"))

plotdat_aggr$reopen <- factor(plotdat_aggr$reopen,
                              levels = c("100perc", "50perc"),
                              labels = c("High increase in transmission",
                                         "Low increase in transmission"))

pplot <- ggplot(data = plotdat_aggr,
                aes(x = rollback_num, y = rt_relred_mean,
                    ymin = rt_relred_q5, ymax = rt_relred_q95,
                    col = capacity_multiplier * 100,
                    fill = capacity_multiplier * 100,
                    group = capacity_multiplier)) +
  geom_smooth(size = 1, se = F) +
  scale_y_continuous(lim = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(lim = c(0, 100), breaks = seq(0, 100, 20),
                     labels = seq(0, 100, 20), expand = c(0, 0)) +
  geom_abline(intercept = 0, slope = 1, size = 0.3, col = "grey") +
  geom_point(col = "black", size = 4, shape = 21) +
  labs(x = 'Reduction in transmission rate (%)',
       y = 'Reduction in Rt (%)',
       fill = "ICU occupancy\nthreshold (%)",
       color = "ICU occupancy\nthreshold (%)") +
  customTheme +
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_color_viridis() +
  scale_fill_viridis() +
  facet_wrap(~reopen)

plotdat %>%
  group_by(reopen) %>%
  summarize(min = min(rt_median_before, na.rm = TRUE),
            max = max(rt_median_before, na.rm = TRUE))


f_save_plot(
  plot_name = paste0("S1_Fig_20"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 10, height = 6
)
if (cleanEnv)rm(list = ls())
