# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Scenario ICU predictions

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()
require(scales)

trace_selection <- TRUE
if (trace_selection) fig_dir = fig_dir_traces

exp_name = "baseline"
traces <- fread(file.path(sim_dir, "sample_num_traces_all.csv"))

dat <- fread(file.path(sim_dir, exp_name, 'trajectoriesDat.csv'),
             select = c("exposed_EMS-11", "symptomatic_mild_EMS-11", "symptomatic_severe_EMS-11",
                        "presymptomatic_EMS-11", "asymptomatic_EMS-11", "critical_EMS-11", "hospitalized_EMS-11", "deaths_EMS-11", "recovered_EMS-11",
                        "sample_num", "scen_num", "time")) %>%
  mutate(date = as.Date("2020-01-01") + time) %>%
  filter(date >= as.Date("2020-03-01") & date <= baseline_date) %>%
  select(-time)
colnames(dat) <- gsub("_EMS-11", "", colnames(dat))
dat <- dat %>%
  pivot_longer(cols = -c(sample_num, scen_num, date), names_to = "outcome") %>%
  dplyr::group_by(outcome, date) %>%
  dplyr::summarise(n = n(),
                   mean = mean(value),
                   median = median(value),
                   lower = quantile(value, probs = 0.05, na.rm = TRUE),
                   upper = quantile(value, probs = 0.95, na.rm = TRUE))


dat_det <- fread(file.path(sim_dir, exp_name, 'trajectoriesDat.csv'),
                 select = c("symptomatic_mild_det_EMS-11", "symptomatic_severe_det_EMS-11",
                            "presymptomatic_detEMS-11", "asymptomatic_det_EMS-11", "crit_det_EMS-11", "hosp_det_EMS-11",
                            "deaths_det_EMS-11", "recovered_det_EMS-11", "sample_num", "scen_num", "time")) %>%
  mutate(date = as.Date("2020-01-01") + time) %>%
  filter(date >= as.Date("2020-03-01") & date <= baseline_date) %>%
  select(-time)
colnames(dat_det) <- gsub("_EMS-11", "", colnames(dat_det))
colnames(dat_det) <- gsub("EMS-11", "", colnames(dat_det))
colnames(dat_det) <- gsub("_det_", "", colnames(dat_det))
colnames(dat_det) <- gsub("_det", "", colnames(dat_det))
dat_det <- dat_det %>%
  pivot_longer(cols = -c(sample_num, scen_num, date), names_to = "outcome") %>%
  dplyr::group_by(outcome, date) %>%
  dplyr::summarise(n = n(),
                   mean = mean(value),
                   median = median(value),
                   lower = quantile(value, probs = 0.05, na.rm = TRUE),
                   upper = quantile(value, probs = 0.95, na.rm = TRUE))

dat$detected = "All"
dat_det$detected = "Detected"
dat$outcome_fct <- factor(dat$outcome,
                          levels = c('exposed', 'presymptomatic', 'asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                     'hospitalized', 'critical', 'deaths', 'recovered'),
                          labels = c('exposed', 'presymptomatic', 'asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                     'hospitalized', 'critical', 'deaths', 'recovered'))

dat_det$outcome_fct <- factor(dat_det$outcome,
                              levels = c('presymptomatic', 'asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                         'hosp', 'crit', 'deaths', 'recovered'),
                              labels = c('presymptomatic', 'asymptomatic', 'symptomatic_mild', 'symptomatic_severe',
                                         'hospitalized', 'critical', 'deaths', 'recovered'))

table(dat$outcome, dat$outcome_fct, exclude = NULL)
table(dat_det$outcome, dat_det$outcome_fct, exclude = NULL)

pplot <- ggplot(data = dat) +
  geom_line(aes(x = date, y = median, col = detected)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = detected), alpha = 0.2) +
  geom_line(data = dat_det, aes(x = date, y = median, col = detected)) +
  geom_ribbon(data = dat_det, aes(x = date, ymin = lower, ymax = upper, fill = detected), alpha = 0.2) +
  facet_wrap(~outcome_fct, scales = "free") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  customTheme +
  labs(x = "", y = "Total number", color = "", fill = "") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


f_save_plot(
  plot_name = paste0("S1_fig_8"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 8
)

