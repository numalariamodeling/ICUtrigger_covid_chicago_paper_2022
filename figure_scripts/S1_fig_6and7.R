# Title     : COVID-19 ICU overflow analysis
# Objective : S1 Figure 6 and 7


source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

channels <- c('admissions', 'med_surg_census', 'ICU_census', 'deaths')

f_combineData <- function(exp_name) {
  dat_list <- list()
  for (trace_selection in c(TRUE, FALSE)) {
    dat_list[[length(dat_list) + 1]] <- f_load_sim_data(
      exp_name = exp_name, sim_dir = sim_dir, fname = "trajectoriesDat.csv",
      add_peak_cols = FALSE, add_trigger_cols = FALSE, addRt = FALSE,
      trace_selection = trace_selection) %>%
      filter(date >= first_plot_date & date <= baseline_date) %>%
      ungroup() %>%
      dplyr::select(date, sample_num, scen_num, hosp_det, crit_det,
                    infected_cumul, hosp_det_cumul, crit_det_cumul, death_det_cumul,
                    new_infected, new_hosp_det, new_crit_det, new_death_det) %>%
      rename(ICU_census = crit_det,
             med_surg_census = hosp_det,
             deaths = new_death_det,
             admissions = new_hosp_det) %>%
      pivot_longer(cols = -c(date, sample_num, scen_num), names_to = "outcome") %>%
      mutate(trace_selection = trace_selection)
  }
  dat <- dat_list %>%
    bind_rows() %>%
    filter(date <= baseline_date) %>%
    as.data.table()
  rm(dat_list)
  return(dat)
}

dat <- f_combineData(exp_name = counterfactual_exps[2]) %>% filter(outcome %in% channels)
head(dat)

datAggr <- dat %>%
  group_by(date, outcome, trace_selection) %>%
  summarise(n = n(),
            median = median(value), mean = mean(value),
            lower = quantile(value, probs = 0.05, na.rm = TRUE),
            upper = quantile(value, probs = 0.95, na.rm = TRUE))

summary(datAggr$n)
tapply(datAggr$n, datAggr$trace_selection, summary)
table(dat$trace_selection, dat$outcome)

ref_dat <- fread(file.path('emresource_chicago_2020.csv'))

ref_dat$date <- as.character(ref_dat$date)
datAggr$date <- as.character(datAggr$date)

fitdat_mean <- datAggr %>%
  merge(ref_dat, by = c("date", "outcome"), all.x = TRUE) %>%
  mutate(date = as.Date(date),
         month = month(date)) %>%
  group_by(outcome, trace_selection) %>%
  arrange(date) %>%
  mutate(mae = mae(mean, value_7avrg),
         mse = mse(mean, value_7avrg)) %>%
  group_by(outcome, month, trace_selection) %>%
  mutate(mae_mth = round(mae(mean, value_7avrg), 2),
         mse_mth = round(mse(mean, value_7avrg), 2)) %>%
  group_by(outcome, month, date, trace_selection) %>%
  mutate(mae_dt = round(mae(mean, value_7avrg), 2),
         mse_dt = round(mse(mean, value_7avrg), 2))
table(fitdat_mean$trace_selection, fitdat_mean$outcome)

ref_dat$date <- as.character(ref_dat$date)
dat$date <- as.character(dat$date)
fitdat <- dat %>%
  merge(ref_dat, by = c("date", "outcome"), all.x = TRUE) %>%
  filter(!is.na(data_val)) %>%
  mutate(date = as.Date(date),
         month = month(date)) %>%
  group_by(outcome, scen_num, sample_num, trace_selection) %>%
  arrange(date) %>%
  mutate(mae = mae(value, value_7avrg),
         mse = mse(value, value_7avrg)) %>%
  group_by(outcome, scen_num, sample_num, month, trace_selection) %>%
  mutate(mae_mth = round(mae(value, value_7avrg), 2),
         mse_mth = round(mse(value, value_7avrg), 2)) %>%
  group_by(outcome, scen_num, sample_num, month, date, trace_selection) %>%
  mutate(mae_dt = round(mae(value, value_7avrg), 2),
         mse_dt = round(mse(value, value_7avrg), 2))

table(fitdat$trace_selection, fitdat$outcome)
ref_dat$date <- as.Date(ref_dat$date)
datAggr$date <- as.Date(datAggr$date)


pplot <- ggplot() +
  geom_line(data = subset(datAggr, outcome %in% channels & trace_selection == 'TRUE'),
            aes(x = date, y = mean), col = "#d76127") +
  geom_ribbon(data = subset(datAggr, outcome %in% channels & trace_selection == 'FALSE'),
              aes(x = date, ymin = lower, ymax = upper, fill = trace_selection), alpha = 0.2) +
  geom_ribbon(data = subset(datAggr, outcome %in% channels & trace_selection == 'TRUE'),
              aes(x = date, ymin = lower, ymax = upper, fill = trace_selection), alpha = 0.4) +
  geom_point(data = subset(ref_dat, outcome %in% channels), aes(x = date, y = data_val)) +
  geom_line(data = subset(ref_dat, outcome %in% channels), aes(x = date, y = value_7avrg)) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Total number", color = "", fill = "") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#d76127", "#d76127"))

f_save_plot(
  plot_name = paste0("S1_fig_6"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 14, height = 3.5
)

pplot <- ggplot() +
  geom_point(data = subset(fitdat_mean, outcome %in% channels), aes(x = value_7avrg, y = mean, col = trace_selection)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Total number", color = "", fill = "") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


### MAE
fitdat %>%
  group_by(outcome, trace_selection) %>%
  summarize(MAE_avrg = mean(mae_dt, na.rm = TRUE))

fitdat_avrg <- fitdat %>%
  group_by(scen_num, sample_num, outcome, trace_selection) %>%
  summarize(mae = mean(mae_dt, na.rm = TRUE))
table(fitdat_avrg$trace_selection, fitdat_avrg$outcome)

pA <- ggplot(data = subset(fitdat_mean, !is.na(trace_selection))) +
  geom_point(aes(x = date, y = mae_dt, fill = trace_selection, col = trace_selection), alpha = 0.4) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  customTheme +
  labs(x = "", y = "daily MAE", color = "Trace selection", fill = "Trace selection") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

pB <- ggplot(data = subset(fitdat_avrg, !is.na(trace_selection))) +
  geom_density(aes(x = mae, fill = trace_selection, col = trace_selection), alpha = 0.4) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  customTheme +
  labs(x = "average MAE", y = "Density", color = "Trace selection", fill = "Trace selection") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

pC <- ggplot(data = subset(fitdat_avrg, !is.na(trace_selection))) +
  geom_histogram(aes(x = mae, fill = trace_selection, col = trace_selection),
                 alpha = 0.4) +
  facet_wrap(~outcome, scales = "free", nrow = 1) +
  customTheme +
  labs(x = "average MAE", y = "N trajectories",
       color = "Trace selection", fill = "Trace selection") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

pplot <- plot_grid(pA, pB, pC, labels = c("A", "B", "C"), ncol = 1)

f_save_plot(
  plot_name = paste0("S1_fig_7"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 14, height = 8
)

### For text
head(fitdat)
summary(fitdat$date)
table(fitdat$trace_selection)
fitdat %>%
  filter(outcome == "ICU_census" & trace_selection == TRUE) %>%
  ungroup() %>%
  dplyr::summarize(mean = mean(mae, na.rm = TRUE),
                   lower = quantile(mae, probs = 0.05, na.rm = TRUE),
                   upper = quantile(mae, probs = 0.95, na.rm = TRUE))

if(cleanEnv)rm(list = ls())