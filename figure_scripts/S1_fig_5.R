# Title     : COVID-19 ICU overflow analysis
# Objective : S1 Figure 5


source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
theme_set(theme_cowplot())
customTheme <- f_getCustomTheme(fontscl = 0.75)
PrevMed_dir <- "R:/PrevMed/Covid-19-Modeling/IDPH line list/"
dat <- fread(file.path(PrevMed_dir, "LL_210111_JGcleaned_no_race.csv"))

dat$count = 1
dat$crit = NA
dat$crit[dat$icu == "Yes"] <- 1
dat$crit[dat$icu == "No"] <- 0

dat$admitted = NA
dat$admitted[dat$admitted_to_hospital == "Yes"] <- 1
dat$admitted[dat$admitted_to_hospital == "No"] <- 0

dat$dead = NA
dat$dead[dat$died_from_disease == "Yes"] <- 1
dat$dead[dat$died_from_disease == "No"] <- 0

dat$specimen_collection <- as.Date(dat$specimen_collection)
dat$specimen_collection_week <- week(dat$specimen_collection)
dat$specimen_collection_month <- month(dat$specimen_collection)

dat <- subset(dat, specimen_collection <= as.Date("2020-09-01"))
datAggr <- dat %>%
  filter(!is.na(died_from_disease) & !is.na(icu)) %>%
  filter(admitted_to_hospital == "Yes" & icu == "Yes") %>%
  group_by(specimen_collection, specimen_collection_month) %>%
  summarize(count = sum(count),
            crit = sum(crit, na.rm = TRUE),
            admitted = sum(admitted, na.rm = TRUE),
            dead = sum(dead, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fraction_dead = dead / crit,
         fraction_dead_avrg = round(rollmean(fraction_dead, 7, align = "right", fill = 0), 2)) %>%
  group_by(specimen_collection_month) %>%
  mutate(fraction_dead_avrgmth = mean(fraction_dead_avrg))


### Parameters used in yaml
dates <- seq(min(datAggr$specimen_collection), max(datAggr$specimen_collection), by = "1 day")
param = as.data.frame(dates)
param$cfr_lo <- 0.01
param$cfr_lo[param$dates >= as.Date("2020-06-01")] <- 0.01 * (2 / 3)
param$cfr_lo[param$dates >= as.Date("2020-07-01")] <- 0.01 * (1 / 3)
param$cfr_up <- 0.03
param$cfr_up[param$dates >= as.Date("2020-06-01")] <- 0.03 * (2 / 3)
param$cfr_up[param$dates >= as.Date("2020-07-01")] <- 0.03 * (1 / 3)

p1 <- ggplot(data = datAggr) +
  geom_rect(xmin = as.Date("2020-05-15"), xmax = as.Date("2020-07-15"), ymin = -Inf, ymax = Inf, alpha = 0.002) +
  geom_line(aes(x = specimen_collection, y = fraction_dead), col = "grey", size = 0.3) +
  geom_line(aes(x = specimen_collection, y = fraction_dead_avrg), col = "black") +
  geom_line(aes(x = specimen_collection, y = fraction_dead_avrgmth), col = "dodgerblue4") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05), lim = c(0, 0.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "", x = "Specimen collection", y = "Dead / hospital admissions") +
  theme_minimal() +
  customTheme

p2 <- ggplot(data = datAggr) +
  #geom_line( aes(x=specimen_collection, y =fraction_dead_avrgmth ),col="dodgerblue4")+
  geom_rect(xmin = as.Date("2020-05-15"), xmax = as.Date("2020-07-15"), ymin = -Inf, ymax = Inf, alpha = 0.002) +
  geom_ribbon(data = param, aes(x = dates, ymin = cfr_lo, ymax = cfr_up), alpha = 1, fill = "dodgerblue4") +
  scale_y_continuous(breaks = seq(0, 0.1, 0.01), minor_breaks = seq(0, 0.1, 0.01), lim = c(0, 0.05)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "", x = "Specimen collection", y = "Case fatality rate") +
  theme_minimal() +
  customTheme

pplot <- plot_grid(p1, p2, nrow = 1, labels = c("A", "B"))
pplot
f_save_plot(
  plot_name = paste0("S1_fig_5"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 6, scale = 0.8
)

if(cleanEnv)rm(list = ls())