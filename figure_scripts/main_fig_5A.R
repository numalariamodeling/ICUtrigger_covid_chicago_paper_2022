# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 5A

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

f_combineData <- function(exp_names, sim_end_date, trace_selection) {
  dat_list <- list()
  for (exp_name in exp_names) {
    print(exp_name)
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name = exp_name, sim_dir = sim_dir, fname = "trajectoriesDat_region_11_trimfut.csv",
      add_peak_cols = TRUE, add_trigger_cols = TRUE, addRt = FALSE, trace_selection = trace_selection) %>%
      filter(date <= sim_end_date) %>%
      filter(trigger_activated == 1 & capacity_multiplier %in% c(0.4, 0.6, 0.8, 1)) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date, date_peak, trigger_activated, triggerDate, crit_det, crit_det_peak) %>%
      dplyr::mutate(rel_occupancy = crit_det / 516,
                    time_since_trigger = round(as.numeric(date - triggerDate), 0),
                    time_to_peak_after_trigger = round(as.numeric(date_peak - triggerDate), 0))
  }
  dat <- dat_list %>% bind_rows() %>% as.data.table()
  rm(dat_list)
  return(dat)
}


p5Adat <- f_combineData(exp_names = c(exp_names_100_delay1[3], exp_names_50_delay1[3]),
                        sim_end_date = sim_end_date, trace_selection = trace_selection)
table(p5Adat$reopen)

p5A <- ggplot(data = p5Adat) +
  geom_line(aes(x = time_since_trigger, y = rel_occupancy,
                group = interaction(scen_num, group_id, exp_name, capacity_multiplier),
                col = as.factor(reopen)),
            alpha = 0.5, size = 0.5) +
  scale_color_manual(values = transm_scen_cols) +
  scale_fill_manual(values = transm_scen_cols) +
  geom_hline(yintercept = 1, col = capacitycolor, linetype = 'dashed') +
  geom_hline(data = unique(p5Adat[, c("capacity_multiplier", "reopen", "delay")]),
             aes(yintercept = capacity_multiplier), col = 'grey', linetype = 'dashed') +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  customTheme +
  labs(x = "weeks since ICU occupancy threshold for action reached",
       y = "ICU occupancy\nas fraction of ICU capacity", color = "") +
  scale_x_continuous(breaks = seq(-28, 90, 14), labels = round(seq(-28, 90, 14) / 7, 0), lim = c(-28, 90)) +
  facet_wrap(~capacity_multiplier, nrow = 1)
#print(p5A)

fwrite(p5Adat, file.path(fig_dir, "csv", "p5Adat.csv"))

f_save_plot(
  plot_name = paste0("Fig5A"), pplot = p5A,
  plot_dir = file.path(fig_dir), width = 14, height = 4
)


### Addition --- 5A time between trigger and capacity exploration
p5Adat <- fread(file.path(fig_dir, "csv", "p5Adat.csv"))

capacityDates_dat <- p5Adat %>%
  filter(time_since_trigger >= 0 & crit_det >= 516) %>%
  group_by(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay, triggerDate) %>%
  summarize(date_capacity = min(date),
            time_since_trigger = min(time_since_trigger))

plodat <- p5Adat %>%
  left_join(capacityDates_dat) %>%
  mutate(date_capacity=as.Date(date_capacity),
         triggerDate=as.Date(triggerDate),
         time_to_capacity_after_trigger = round(as.numeric(date_capacity - triggerDate), 0))
#summary(plodat$time_to_capacity_after_trigger)

plodatAggr <- plodat %>%
  group_by(capacity_multiplier, reopen, rollback, delay) %>%
  summarize(min = min(time_to_capacity_after_trigger, na.rm = TRUE),
            mean = mean(time_to_capacity_after_trigger, na.rm = TRUE),
            max = max(time_to_capacity_after_trigger, na.rm = TRUE))

print(plodatAggr)

pplot <- ggplot(data = plodat) +
  geom_jitter(aes(x = time_to_capacity_after_trigger, y = reopen, col = reopen), width = 0, height = 0.05) +
  geom_pointrange(data = plodatAggr, aes(y = reopen, x = mean, xmin = min, xmax = max)) +
  facet_wrap(~capacity_multiplier, nrow = 1) +
  scale_color_manual(values = transm_scen_cols) +
  scale_fill_manual(values = transm_scen_cols)

f_save_plot(
  plot_name = paste0("Fig5A_addon_explore_trigger_to_capacity"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 6, height = 3
)

if(cleanEnv)rm(list = ls())