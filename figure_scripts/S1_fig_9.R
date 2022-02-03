# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 9

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

trace_selection <- TRUE
if (trace_selection) fig_dir = fig_dir_traces

exp_name = "baseline"
traces <- fread(file.path(sim_dir, "sample_num_traces_all.csv"))

dat <- fread(file.path(sim_dir, exp_name, 'trajectoriesDat.csv'),
             select = c("infected_cumul_EMS-11", "symp_mild_cumul_EMS-11", "symp_severe_cumul_EMS-11",
                        "asymp_cumul_EMS-11", "crit_cumul_EMS-11", "hosp_cumul_EMS-11",
                        "deaths_EMS-11", "recovered_EMS-11", "sample_num", "scen_num", "time")) %>%
  dplyr::mutate(date = as.Date("2020-01-01") + time) %>%
  dplyr::filter(date >= as.Date("2020-03-01") & date <= baseline_date) %>%
  dplyr::select(-time)

colnames(dat) <- gsub("_EMS-11", "", colnames(dat))
colnames(dat) <- gsub("_cumul", "", colnames(dat))


dat <- dat %>%
  pivot_longer(cols = -c(sample_num, scen_num, date), names_to = "outcome") %>%
  dplyr::group_by(sample_num, scen_num, outcome) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(value_new = value - lag(value)) %>%
  dplyr::group_by(outcome, date) %>%
  dplyr::summarise(n = n(),
                   mean = mean(value),
                   median = median(value),
                   lower = quantile(value, probs = 0.05, na.rm = TRUE),
                   upper = quantile(value, probs = 0.95, na.rm = TRUE),
                   mean_new = mean(value_new),
                   median_new = median(value_new),
                   lower_new = quantile(value_new, probs = 0.05, na.rm = TRUE),
                   upper_new = quantile(value_new, probs = 0.95, na.rm = TRUE))


dat_det <- fread(file.path(sim_dir, exp_name, 'trajectoriesDat.csv'),
                 select = c("symp_mild_det_cumul_EMS-11", "symp_severe_det_cumul_EMS-11",
                            "asymp_det_cumul_EMS-11", "crit_det_cumul_EMS-11", "hosp_det_cumul_EMS-11",
                            "death_det_cumul_EMS-11", "recovered_det_EMS-11", "sample_num", "scen_num", "time")) %>%
  mutate(date = as.Date("2020-01-01") + time) %>%
  filter(date >= as.Date("2020-03-01") & date <= baseline_date) %>%
  select(-time)

colnames(dat_det) <- gsub("_EMS-11", "", colnames(dat_det))
colnames(dat_det) <- gsub("EMS-11", "", colnames(dat_det))
colnames(dat_det) <- gsub("_det_", "", colnames(dat_det))
colnames(dat_det) <- gsub("_det", "", colnames(dat_det))
colnames(dat_det) <- gsub("cumul", "", colnames(dat_det))


dat_det <- dat_det %>%
  pivot_longer(cols = -c(sample_num, scen_num, date), names_to = "outcome") %>%
  dplyr::group_by(sample_num, scen_num, outcome) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(value_new = value - lag(value)) %>%
  dplyr::group_by(outcome, date) %>%
  dplyr::summarise(n = n(),
                   mean = mean(value),
                   median = median(value),
                   lower = quantile(value, probs = 0.05, na.rm = TRUE),
                   upper = quantile(value, probs = 0.95, na.rm = TRUE),
                   mean_new = mean(value_new),
                   median_new = median(value_new),
                   lower_new = quantile(value_new, probs = 0.05, na.rm = TRUE),
                   upper_new = quantile(value_new, probs = 0.95, na.rm = TRUE))


dat$detected = "All"
dat_det$detected = "Detected"
dat$outcome_fct <- factor(dat$outcome,
                          levels = c('infected', 'asymp', 'symp_mild', 'symp_severe',
                                     'hosp', 'crit', 'deaths', 'recovered'),
                          labels = c('infected', 'asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                     'hospitalized', 'critical', 'deaths', 'recovered'))

dat_det$outcome_fct <- factor(dat_det$outcome,
                              levels = c('asymp', 'symp_mild', 'symp_severe',
                                         'hosp', 'crit', 'death', 'recovered'),
                              labels = c('asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                         'hospitalized', 'critical', 'deaths', 'recovered'))

table(dat$outcome, dat$outcome_fct, exclude = NULL)
table(dat_det$outcome, dat_det$outcome_fct, exclude = NULL)

pplot <- ggplot(data = dat) +
  geom_line(aes(x = date, y = mean_new, col = detected)) +
  geom_ribbon(aes(x = date, ymin = lower_new, ymax = upper_new, fill = detected), alpha = 0.2) +
  geom_line(data = dat_det, aes(x = date, y = mean_new, col = detected)) +
  geom_ribbon(data = dat_det, aes(x = date, ymin = lower_new, ymax = upper_new, fill = detected), alpha = 0.2) +
  facet_wrap(~outcome_fct, scales = "free") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Daily new", color = "", fill = "") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


f_save_plot(
  plot_name = paste0("S1_fig_9"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 8
)

if(cleanEnv)rm(list = ls())