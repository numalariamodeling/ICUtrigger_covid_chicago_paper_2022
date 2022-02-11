# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 17
# Data files: capacity_by_covid_region.csv, emresource_by_region.csv, capacity_weekday_average_20200915.csv

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

ref_dat <- fread(file.path(data_path, 'COVID-19_ICU_Chicago_2020.csv')) %>%
  mutate(icu_covid = suspected_and_confirmed_covid_icu,
         assumed_capacity = assumed_capacity,
         percICU = suspected_and_confirmed_covid_icu / icu_availforcovid) %>%
  mutate(icu_covid_7avrg = rollmean(icu_covid, 7, align = 'right', fill = NA),
         icu_total_7avrg = rollmean(icu_total, 7, align = 'right', fill = NA),
         percICU_7avrg = rollmean(percICU, 7, align = 'right', fill = NA),
         icu_noncovid_frac = icu_noncovid / icu_total,
         icu_availforcovid_frac = 1 - (icu_availforcovid / icu_total),
         icu_noncovid_frac = icu_noncovid / icu_total,
         icu_availforcovid_frac = 1 - (icu_availforcovid / icu_total))

mean(ref_dat$icu_noncovid_frac, na.rm = TRUE)

pplot <- ggplot(data = ref_dat) +
  geom_line(aes(x = date, y = icu_noncovid_frac)) +
  geom_hline(yintercept = mean(ref_dat$icu_noncovid_frac, na.rm = TRUE)) +
  labs(y = "Fraction occupied by non-COVID patients") +
  customTheme

f_save_plot(
  plot_name = paste0("S1_fig_17_supp"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 10, height = 6
)

pplot <- ggplot(data = ref_dat) +
  geom_ribbon(aes(x = date, ymin = 0, ymax = icu_total), fill = "grey", alpha = 0.7) +
  geom_ribbon(aes(x = date, ymin = 0, ymax = icu_covid + icu_noncovid), fill = "dodgerblue2", alpha = 0.9) +
  geom_ribbon(aes(x = date, ymin = 0, ymax = icu_covid), fill = "darkorange", alpha = 0.9) +
  geom_line(aes(x = date, y = icu_availforcovid_7avrg), col = capacitycolor, size = 0.5) +
  geom_line(aes(x = date, y = icu_covid), col = "darkorange") +
  geom_hline(yintercept = 516, col = capacitycolor, linetype = "dashed") +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 1250)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_vline(xintercept = as.Date("2020-11-20")) +
  customTheme +
  labs(x = "", y = "Number of ICU beds")

f_save_plot(
  plot_name = paste0("S1_fig_17"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 10, height = 6
)

### Capacity and rel occupancy during peak at 1st wave
(first_wave_peak <- ref_dat %>%
  filter(date <= as.Date("2020-09-01")) %>%
  filter(icu_covid_7avrg == max(icu_covid_7avrg, na.rm = TRUE)))
first_wave_peak$icu_availforcovid_7avrg - 516

### Average beds per pop
ref_dat <- ref_dat %>%
  mutate(icu_beds_covid_capacity_per10000 = (icu_availforcovid_7avrg / Chicago_pop) * 10000,
         icu_beds_total_capacity_per10000 = (icu_total_7avrg / Chicago_pop) * 10000) %>%
  mutate(first_wave = ifelse(date <= as.Date("2020-07-01"), 1, 0))

summary(ref_dat$icu_beds_covid_capacity_per10000)
summary(ref_dat$icu_beds_total_capacity_per10000)

## Separate before/after July 1st
tapply(ref_dat$icu_beds_covid_capacity_per10000, ref_dat$first_wave, summary)
tapply(ref_dat$icu_beds_total_capacity_per10000, ref_dat$first_wave, summary)

#### Additional descriptives
(mean(ref_dat$icu_total) / Chicago_pop) * 10000
(mean(ref_dat$icu_availforcovid, na.rm = TRUE) / Chicago_pop) * 10000

### Capacity as 2nd stay at home order was implemented in November
capacityNovLabel <- ref_dat$percICU[ref_dat$date > as.Date("2020-11-21") & ref_dat$date <= as.Date("2020-11-22")]
pplot <- ggplot(data = subset(ref_dat)) +
  geom_hline(yintercept = c(capacityNovLabel, 0.6, 0.80), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-11-22"), linetype = "dashed") +
  geom_line(aes(x = date, y = percICU, group = 1), alpha = 1, col = "deepskyblue3", size = 1.2) +
  scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 1)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme +
  labs(x = "", y = "% of ICU beds available\nfor COVID-19 filled")

f_save_plot(
  plot_name = paste0("S1_fig_17_supp1"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 10, height = 6
)


mean(ref_dat$icu_noncovid_frac, na.rm = TRUE)
pplot <- ggplot(data = ref_dat) +
  geom_line(aes(x = date, y = icu_noncovid_frac)) +
  geom_hline(yintercept = mean(ref_dat$icu_noncovid_frac, na.rm = TRUE)) +
  labs(y = "Fraction occupied by non-COVID patients") +
  customTheme

f_save_plot(
  plot_name = paste0("S1_fig_17_supp2"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 10, height = 6
)
if (cleanEnv)rm(list = ls())
