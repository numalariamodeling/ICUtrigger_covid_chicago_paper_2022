# Title     : COVID-19 ICU overflow analysis
# Objective : S1 Figure 2 and 3

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme(fontscl = 0.75)

###========================================================================
### dSys
dat_dSys <- fread(file.path(data_path,"detection_estimation", "underreporting_skellam_20201123.csv"))
dat_dSys$date <- as.Date(dat_dSys$date)
dat_dSys <- dat_dSys %>%
  mutate(p975 = ifelse(p975 > 1, 1, p975),
         med = ifelse(med > 1, 1, med),
         month = month(date)) %>%
  filter(date <= as.Date("2020-09-01")) %>%
  group_by(month) %>%
  mutate(med_avrgmth = mean(med))

### Parameters used in yaml
dates <- seq(min(dat_dSys$date), max(dat_dSys$date), by = "1 day")
param = as.data.frame(dates)
param$dSys_lo <- 0.0
param$dSys_lo[param$dates >= as.Date("2020-03-07")] <- 0.029
param$dSys_lo[param$dates >= as.Date("2020-03-14")] <- 0.090
param$dSys_lo[param$dates >= as.Date("2020-03-21")] <- 0.192
param$dSys_lo[param$dates >= as.Date("2020-03-28")] <- 0.330
param$dSys_lo[param$dates >= as.Date("2020-04-04")] <- 0.450
param$dSys_lo[param$dates >= as.Date("2020-04-18")] <- 0.600
param$dSys_lo[param$dates >= as.Date("2020-06-01")] <- 0.800
param$dSys_up <- 0.0185
param$dSys_up[param$dates >= as.Date("2020-03-07")] <- 0.090
param$dSys_up[param$dates >= as.Date("2020-03-14")] <- 0.192
param$dSys_up[param$dates >= as.Date("2020-03-21")] <- 0.330
param$dSys_up[param$dates >= as.Date("2020-03-28")] <- 0.450
param$dSys_up[param$dates >= as.Date("2020-04-04")] <- 0.600
param$dSys_up[param$dates >= as.Date("2020-04-18")] <- 0.800
param$dSys_up[param$dates >= as.Date("2020-06-01")] <- 1.000

p1 <- ggplot(data = dat_dSys) +
  geom_line(aes(x = date, y = med)) +
  geom_ribbon(aes(x = date, ymin = p025, ymax = p975), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = date, y = med), col = "black") +
  #geom_line(aes(x=date , y= med_avrgmth), col="dodgerblue4")+
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1)) +
  labs(title = "",
       y = "fraction of COVID-19 deaths detected\nall ages",
       x = "") +
  theme_minimal() +
  customTheme

p2 <- ggplot(data = param) +
  geom_ribbon(aes(x = dates, ymin = dSys_lo, ymax = dSys_up), fill = "dodgerblue4") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1)) +
  labs(title = "",
       y = "fraction severe detected",
       x = "") +
  theme_minimal() +
  customTheme


pplot <- plot_grid(p1, p2, nrow = 1, labels = c("A", "B"))

f_save_plot(
  plot_name = paste0("S1_fig_3"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 6, scale = 0.8
)


###========================================================================
### dSym
dat_Sym <- fread(file.path(data_path,"detection_estimation", "f_inf_det_Illinois_201124_excessdeaths_and_nonstationary.csv"))
dat_Sym$date <- as.Date(dat_Sym$date)
dat_Sym <- dat_Sym %>%
  mutate(p975_f_inf_det = ifelse(p975_f_inf_det > 1, 1, p975_f_inf_det),
         median_f_inf_det = ifelse(median_f_inf_det > 1, 1, median_f_inf_det),
         month = month(date)) %>%
  filter(date <= as.Date("2020-09-01")) %>%
  group_by(month) %>%
  mutate(median_f_inf_det_avrgmth = mean(median_f_inf_det))

dSym_change_dates <- c("2020-03-15", "2020-04-15", "2020-05-01", "2020-06-01", "2020-07-01")

### Parameters used in yaml
dates <- seq(min(dat_Sym$date), max(dat_Sym$date), by = "1 day")
param = as.data.frame(dates)
param$dSym_lo <- 0.0
param$dSym_lo[param$dates >= as.Date("2020-03-15")] <- 0.01
param$dSym_lo[param$dates >= as.Date("2020-04-15")] <- 0.05
param$dSym_lo[param$dates >= as.Date("2020-05-01")] <- 0.10
param$dSym_lo[param$dates >= as.Date("2020-06-01")] <- 0.10
param$dSym_lo[param$dates >= as.Date("2020-07-01")] <- 0.10
param$dSym_up <- 0.01
param$dSym_up[param$dates >= as.Date("2020-03-15")] <- 0.08
param$dSym_up[param$dates >= as.Date("2020-04-15")] <- 0.10
param$dSym_up[param$dates >= as.Date("2020-05-01")] <- 0.20
param$dSym_up[param$dates >= as.Date("2020-06-01")] <- 0.30
param$dSym_up[param$dates >= as.Date("2020-07-01")] <- 0.40


p1 <- ggplot(data = dat_Sym) +
  geom_ribbon(aes(x = date, ymin = p25_f_inf_det, ymax = p975_f_inf_det), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = date, y = median_f_inf_det)) +
  #  geom_line(aes(x=date , y= median_f_inf_det_avrgmth),col="dodgerblue4") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1)) +
  labs(title = "",
       y = "fraction of infections detected\naccounting for nonst.IFR and unreported deaths\n",
       x = "") +
  theme_minimal() +
  customTheme

p2 <- ggplot(data = param) +
  geom_ribbon(aes(x = dates, ymin = dSym_lo, ymax = dSym_up), fill = "dodgerblue4") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), lim = c(0, 1)) +
  labs(title = "",
       y = "fraction mild symptomatic detected",
       x = "") +
  theme_minimal() +
  customTheme

pplot <- plot_grid(p1, p2, nrow = 1, labels = c("A", "B"))

f_save_plot(
  plot_name = paste0("S1_fig_2"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 6, scale = 0.8
)