# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 4

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

trace_selection = TRUE
reopen = "100perc"

if (reopen == "100perc") {
  exp_names <- exp_names_100_delay1
  counterfactual_exp <- counterfactual_exps[1]
}
if (reopen == "50perc") {
  exp_names <- exp_names_50_delay1
  counterfactual_exp <- counterfactual_exps[2]
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

### Counterfactual dat
counterfactualDat <- f_load_sim_data(exp_name = counterfactual_exp, sim_dir = sim_dir, fname = "trajectoriesDat.csv",
                                     add_peak_cols = TRUE, add_trigger_cols = FALSE,
                                     addRt = TRUE, trace_selection = trace_selection) %>%
  mutate(capacity_multiplier = 0.6, rel_occupancy = crit_det / 516, rel_occupancy_peak = crit_det_peak / 516)

#### Mitigation dat
print(exp_names)
dat <- f_combineData(exp_names = exp_names, sim_end_date = sim_end_date, trace_selection = trace_selection)

### Descriptives for checking
setDT(dat)
summary(dat$date)
length(unique(dat$date))
table(dat$sample_num, dat$date)
table(dat$rollback, dat$sample_num)
tapply(dat$date, dat$rollback, summary)

triggerDat <- dat
### Subset for example plots
dat_selected_scens <- triggerDat %>%
  ungroup() %>%
  group_by(capacity_multiplier, rollback) %>%
  filter(triggerDate == min(triggerDate)) %>%
  sample_n(1) %>%
  dplyr::select(group_id, scen_num, sample_num, capacity_multiplier, trigger_activated, exp_name)

dat_sub <- dat_selected_scens %>% left_join(triggerDat)
table(dat_sub$capacity_multiplier, dat_sub$perc_trigger)
table(dat_sub$scen_num)

triggerDat_peak <- triggerDat %>% f_get_peakDat(
  groupVARS = c("group_id", "sample_num", "rollback", "reopen", "capacity_multiplier"),
  keepVARS = c("time_since_trigger", "time_to_peak_after_trigger", "group_id", "sample_num", "rollback", "reopen",
               "capacity_multiplier", "date_peak", "crit_det_peak"))

triggerDat_peakAggr <- triggerDat_peak %>%
  group_by(capacity_multiplier, reopen, rollback) %>%
  mutate(date_peak = as.Date(date_peak)) %>%
  dplyr::summarize(
    trigger_n.val = n(),
    trigger_mean = mean(crit_det_peak),
    trigger_median = median(crit_det_peak),
    trigger_q5 = quantile(crit_det_peak, probs = 0.05, na.rm = TRUE),
    trigger_q95 = quantile(crit_det_peak, probs = 0.95, na.rm = TRUE)
  )

triggerDate_counterfactual <- counterfactualDat %>%
  filter(date >= as.Date("2020-10-01")) %>%
  group_by(sample_num, scen_num) %>%
  filter(crit_det > 516 * 0.6) %>%
  filter(date == min(date)) %>%
  rename(triggerDate = date) %>%
  select(sample_num, scen_num, triggerDate)

counterfactualDat <- counterfactualDat %>%
  left_join(triggerDate_counterfactual) %>%
  dplyr::mutate(time_since_trigger = date - triggerDate,
                time_since_trigger = round(time_since_trigger, 0))

counterfactualDat_peak <- f_get_peakDat(counterfactualDat,
                                        groupVARS = c("group_id", "sample_num", "reopen"),
                                        keepVARS = c("time_since_trigger", "group_id", "sample_num", "reopen", "date_peak", "crit_det_peak")) %>%
  mutate(date_peak = as.Date(date_peak)) %>%
  rename(date_peak_counter = date_peak, crit_det_peak_counter = crit_det_peak)

counterfactualDat_peakAggr <- counterfactualDat_peak %>%
  group_by(reopen) %>%
  dplyr::summarize(
    counter_n.val = n(),
    counter_mean = mean(crit_det_peak_counter),
    counter_median = median(crit_det_peak_counter),
    counter_q5 = quantile(crit_det_peak_counter, probs = 0.05, na.rm = TRUE),
    counter_q95 = quantile(crit_det_peak_counter, probs = 0.95, na.rm = TRUE)
  )

temp_dat1 <- counterfactualDat_peakAggr
temp_dat2 <- triggerDat_peakAggr
colnames(temp_dat1) <- gsub("counter_", "", colnames(temp_dat1))
colnames(temp_dat2) <- gsub("trigger_", "", colnames(temp_dat2))
temp_dat1 <- temp_dat1 %>%
  dplyr::mutate(rollback = "pr0", capacity_multiplier = 0) %>%
  dplyr::select(colnames(temp_dat2))
p4E_dat <- temp_dat1 %>% rbind(temp_dat2)

p4E <- ggplot(p4E_dat) +
  geom_bar(aes(x = rollback, y = mean / 516, fill = rollback), stat = "identity") +
  geom_errorbar(aes(x = rollback, ymin = q5 / 516, ymax = q95 / 516), width = 0) +
  geom_hline(aes(yintercept = 1), col = capacitycolor, linetype = 'dashed') +
  scale_fill_manual(values = c("darkgrey", mitigation_cols)) +
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  theme(legend.position = "none")

### 4B
temp_dat1 <- counterfactualDat_peak %>%
  mutate(time_since_trigger = as.numeric(time_since_trigger))

temp_dat2 <- triggerDat_peak %>%
  select(-time_to_peak_after_trigger) %>%
  mutate(time_since_trigger = as.numeric(time_since_trigger))

colnames(temp_dat1) <- gsub("_counter", "", colnames(temp_dat1))
colnames(temp_dat2) <- gsub("trigger_", "", colnames(temp_dat2))

temp_dat1 <- temp_dat1 %>%
  dplyr::mutate(rollback = "pr0", capacity_multiplier = 0) %>%
  dplyr::select(colnames(temp_dat2))

p4B_dat <- temp_dat1 %>% rbind(temp_dat2)
p4B_dat$rollback_fct <- factor(p4B_dat$rollback,
                               levels = c("pr0", "pr2", "pr4", "pr6", "pr8"),
                               labels = c("none (0%)", "weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"))

p4B <- ggplot(data = p4B_dat) +
  geom_jitter(aes(x = time_since_trigger, y = rollback_fct, col = rollback_fct), width = 0, height = 0.1) +
  scale_color_manual(values = c("darkgrey", mitigation_cols)) +
  scale_x_continuous(breaks = seq(-14, 90, 14), labels = seq(-14, 90, 14) / 7, lim = c(-14, 90)) +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none") +
  labs(x = "weeks between reaching ICU occupancy threshold for action\nand peak ICU occupancy",
       y = "mitigation strength")

p4A <- ggplot(data = triggerDat) +
  geom_line(data = counterfactualDat, aes(x = time_since_trigger, y = rel_occupancy,
                                          group = interaction(reopen, exp_name, sample_num, scen_num, capacity_multiplier)),
            col = 'grey', alpha = 0.6, size = 0.9) +
  geom_line(aes(x = time_since_trigger, y = rel_occupancy,
                group = interaction(reopen, exp_name, sample_num, scen_num, rollback, capacity_multiplier),
                col = as.factor(rollback)), alpha = 0.6, size = 0.9) +
  scale_color_manual(values = mitigation_cols) +
  geom_hline(aes(yintercept = 1), col = capacitycolor, linetype = 'dashed') +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none") +
  labs(x = "weeks since ICU occupancy\nthreshold for action reached",
       y = "ICU occupancy\nrelative to capacity", color = "") +
  scale_y_continuous(breaks = seq(0, 7, 0.5)) +
  scale_x_continuous(breaks = seq(-14, 90, 14), labels = seq(-14, 90, 14) / 7, lim = c(-14, 90))

p4C <- dat_sub %>%
  ggplot() +
  geom_line(aes(x = time_since_trigger, y = Ki_t, group = interaction(group_id, rollback, capacity_multiplier),
                col = as.factor(rollback)), alpha = 1, size = 1.2) +
  scale_color_manual(values = mitigation_cols) +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none") +
  labs(x = "weeks since ICU occupancy\nthreshold for action reached",
       y = "Transmission\nrate", color = "") +
  scale_x_continuous(breaks = seq(-14, 90, 14), labels = seq(-14, 90, 14) / 7, lim = c(-14, 90)) +
  scale_y_continuous(breaks = seq(0, 0.25, 0.05),
                     expand = c(0, 0), lim = c(0, 0.26))

##---------------------------
## Aggregated Rt plot
p4D_dat <- triggerDat %>%
  ungroup() %>%
  mutate(time_since_trigger = round(time_since_trigger, 0)) %>%
  filter(time_since_trigger > -14 & !is.na(rt_median)) %>%
  dplyr::select(time_since_trigger, rollback, group_id, capacity_multiplier, rt_median) %>%
  unique() %>%
  dplyr::group_by(time_since_trigger, rollback, capacity_multiplier) %>%
  dplyr::summarize(
    n.val = n(),
    mean = mean(rt_median),
    median = median(rt_median),
    q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
    q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE)
  ) %>%
  mutate(time_since_trigger_wks = time_since_trigger / 7)

p4D_dat_raw <- triggerDat %>%
  ungroup() %>%
  mutate(time_since_trigger = round(time_since_trigger, 0)) %>%
  filter(time_since_trigger > -14 & !is.na(rt_median)) %>%
  dplyr::select(scen_num, sample_num,time_since_trigger, rollback, group_id, capacity_multiplier, rt_median, rt_lower, rt_upper) %>%
  unique() %>%
  mutate(time_since_trigger_wks = time_since_trigger / 7)

p4D <- ggplot(data = subset(p4D_dat, time_since_trigger_wks > -2 & time_since_trigger_wks <= 12)) +
  geom_ribbon(data = subset(p4D_dat_raw, time_since_trigger_wks > -2 & time_since_trigger_wks <= 12),
              aes(x = time_since_trigger_wks, ymin = rt_lower, ymax = rt_upper,
                 fill = as.factor(rollback),
                  group = interaction(scen_num, sample_num, rollback, capacity_multiplier)), alpha = 0.01) +
  geom_ribbon(aes(x = time_since_trigger_wks, ymin = q5, ymax = q95,
                  group = interaction(rollback, capacity_multiplier),
                  fill = as.factor(rollback)), alpha = 0.3) +
  geom_line(aes(x = time_since_trigger_wks, y = mean,
                group = interaction(rollback, capacity_multiplier),
                col = as.factor(rollback)), alpha = 0.6, size = 0.9) +
  scale_fill_manual(values = mitigation_cols) +
  scale_color_manual(values = mitigation_cols) +
  geom_hline(aes(yintercept = 1), col = "black", linetype = 'dashed') +
  theme_minimal() +
  customTheme +
  theme(legend.position = "none") +
  labs(x = "weeks since ICU occupancy\nthreshold for action reached",
       y = expr("R"[t]), color = "", fill = "") +
  scale_x_continuous(breaks = seq(-2, 12, 1), labels = seq(-2, 12, 1), lim = c(-2, 12))


##-----------------------
## COMBINE AND SAVE
##-----------------------
p4AB <- plot_grid(p4A, p4B, nrow = 2, rel_heights = c(1, 0.4), align = "hv", labels = c("A", "B"))
p4CDE <- plot_grid(p4C, p4D, p4E, ncol = 1, align = "hv", labels = c("C", "D", "E"))
pplot <- plot_grid(p4AB, p4CDE, nrow = 1, rel_widths = c(1, 0.4))

f_save_plot(
  plot_name = paste0("Fig4_", reopen), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 14, height = 8
)


## Save csvs
fwrite(triggerDat, file.path(fig_dir, "csv", paste0("p4A_dat_trigger_", reopen, ".csv")))
fwrite(counterfactualDat, file.path(fig_dir, "csv", paste0("p4A_dat_counterfactual_", reopen, ".csv")))
fwrite(p4B_dat, file.path(fig_dir, "csv", paste0("p4B_dat_", reopen, ".csv")))
fwrite(dat_sub, file.path(fig_dir, "csv", paste0("p4C_dat_", reopen, ".csv")))
fwrite(p4D_dat, file.path(fig_dir, "csv", paste0("p4D_dat_", reopen, ".csv")))
fwrite(p4E_dat, file.path(fig_dir, "csv", paste0("p4E_dat_", reopen, ".csv")))


##------------------------------
### For text
##------------------------------
triggerDat <- fread(file.path(fig_dir, "csv", paste0("p4A_dat_trigger_", reopen, ".csv")))
counterfactualDat <- fread(file.path(fig_dir, "csv", paste0("p4A_dat_counterfactual_", reopen, ".csv")))
p4B_dat <- fread(file.path(fig_dir, "csv", paste0("p4B_dat_", reopen, ".csv")))
dat_sub <- fread(file.path(fig_dir, "csv", paste0("p4C_dat_", reopen, ".csv")))
p4D_dat <- fread(file.path(fig_dir, "csv", paste0("p4D_dat_", reopen, ".csv")))
p4E_dat <- fread(file.path(fig_dir, "csv", paste0("p4E_dat_", reopen, ".csv")))

tapply(as.Date(p4B_dat$date_peak), p4B_dat$rollback, summary)

p4B_dat %>%
  ungroup() %>%
  select(c(reopen, sample_num, rollback, crit_det_peak)) %>%
  group_by(reopen, sample_num, rollback) %>%
  unique() %>%
  pivot_wider(names_from = rollback, values_from = c(crit_det_peak)) %>%
  mutate(counterfactual = pr0) %>%
  pivot_longer(cols = -c(reopen, sample_num, counterfactual)) %>%
  mutate(red = (1 - (value / counterfactual)) * 100) %>%
  group_by(reopen, name) %>%
  summarize(mean = round(mean(red, na.rm = TRUE), 1),
            median = round(median(red, na.rm = TRUE), 1),
            q5 = round(quantile(red, probs = 0.05, na.rm = TRUE), 1),
            q95 = round(quantile(red, probs = 0.95, na.rm = TRUE), 1)) %>%
  as.data.frame()

### 4D

## Aggregated dataset (as shown in plot)
p4D_dat %>%
  filter(time_since_trigger_wks < 0) %>%
  group_by(rollback) %>%
  filter(median == max(median)) %>%
  group_by() %>%
  summarize(mean = mean(mean),
            median = mean(median),
            q5 = mean(q5),
            q95 = mean(q95))

## Rt values 2 weeks within mitgation
p4D_dat %>%
  filter(time_since_trigger_wks > 0 & time_since_trigger_wks <= 2) %>%
  group_by(rollback) %>%
  filter(median == min(median)) %>%
  summarize(mean_min = mean(mean),
            median_min = mean(median),
            q5 = mean(q5),
            q95 = mean(q95))

## Unaggregated dataset
triggerDat %>%
  filter(time_since_trigger == -14) %>%
  dplyr::group_by(reopen, delay) %>%
  dplyr::summarize(rt_median_mean = mean(rt_median, na.rm = TRUE),
                   rt_median_q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
                   rt_median_q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE))

### Minimum Rt values 2 weeks within mitgation
triggerDat %>%
  filter(time_since_trigger > 0 & time_since_trigger <= 14) %>%
  group_by(reopen, delay, rollback, scen_num) %>%
  filter(rt_median == min(rt_median, na.rm = TRUE)) %>%
  dplyr::group_by(reopen, delay, rollback) %>%
  dplyr::summarize(rt_median_mean = mean(rt_median, na.rm = TRUE),
                   rt_median_q5 = quantile(rt_median, probs = 0.05, na.rm = TRUE),
                   rt_median_q95 = quantile(rt_median, probs = 0.95, na.rm = TRUE))


### Reductions in Rt
Rt_before <- triggerDat %>%
  filter(time_since_trigger == -14) %>%
  select(reopen, delay, rollback, scen_num, capacity_multiplier, rt_median) %>%
  rename(rt_median_before = rt_median)

Rt_after <- triggerDat %>%
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
  fwrite(file.path(fig_dir,"csv", paste0("Rt_diff", reopen,".csv")))

Rt_diff %>%
  dplyr::group_by(reopen, delay, rollback, capacity_multiplier) %>%
  dplyr::summarize(rt_relred_mean = mean(rt_relred, na.rm = TRUE),
                   rt_relred_q5 = quantile(rt_relred, probs = 0.05, na.rm = TRUE),
                   rt_relred_q95 = quantile(rt_relred, probs = 0.95, na.rm = TRUE))

if(cleanEnv)rm(list = ls())