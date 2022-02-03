# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 18

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()

dat <- fread(file.path(sim_dir, "baseline_20210113", "nu_20210113.csv")) %>%
  filter(geography_modeled == "covidregion_11") %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2020-09-01") & date <= as.Date("2020-12-31"))

ref_dat <- fread(file.path('emresource_chicago_2020.csv'))

p1 <- ggplot(data = dat) +
  geom_ribbon(aes(x = date, ymin = icu_det_lower, ymax = icu_det_upper), alpha = 0.5, fill = "deepskyblue4") +
  geom_line(aes(x = date, y = icu_det_median), col = "deepskyblue4") +
  geom_point(data = subset(ref_dat, Date >= baseline_date & Date <= as.Date("2020-12-31")),
             aes(x = Date, y = confirmed_covid_icu), shape = 21, fill = 'black', size = 1) +
  geom_vline(xintercept = as.Date("2020-11-20")) +
  geom_hline(yintercept = 516, col = capacitycolor, linetype = "dashed") +
  scale_x_date(date_breaks = c("1 month"), date_labels = "%b") +
  labs(title = "", y = "Predicted ICU occupancy", x = "") +
  customTheme

p2 <- ggplot(data = dat) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), alpha = 0.5, fill = "deepskyblue4") +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4") +
  geom_vline(xintercept = as.Date("2020-11-20")) +
  geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
  scale_x_date(date_breaks = c("1 month"), date_labels = "%b") +
  labs(title = "", y = "Estimated Rt", x = "") +
  customTheme

fwrite(dat,file.path(fig_dir, "csv", "S1_fig_18.csv"))
pplot <- plot_grid(p1, p2, nrow = 2, labels = c("A", "B"))

f_save_plot(
  plot_name = paste0("S1_fig_18"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 8, height = 8
)
if (cleanEnv)rm(list = ls())