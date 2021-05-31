# Title     : COVID-19 ICU overflow analysis
# Objective : Fraction critical | admitted

library(tidyverse)
library(data.table)
library(cowplot)
library(zoo)

source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))
customTheme <- f_getCustomTheme(fontscl=0.75)

PrevMed_dir <- "R:/PrevMed/Covid-19-Modeling/IDPH line list/"
dat <- fread(file.path(PrevMed_dir, "LL_210111_JGcleaned_no_race.csv"))

### Parameters used in yaml
dates <- seq(min(datAggr$specimen_collection),max(datAggr$specimen_collection),by="1 day")
param = as.data.frame(dates)
param$fraction_critical_lo <- 0.20
param$fraction_critical_lo[param$dates>= as.Date("2020-04-01")] <- 0.13
param$fraction_critical_lo[param$dates>= as.Date("2020-05-01")] <- 0.07
param$fraction_critical_lo[param$dates>= as.Date("2020-06-01")] <- 0.05
param$fraction_critical_up <- 0.35
param$fraction_critical_up[param$dates>= as.Date("2020-04-01")] <- 0.23
param$fraction_critical_up[param$dates>= as.Date("2020-05-01")] <- 0.12
param$fraction_critical_up[param$dates>= as.Date("2020-06-01")] <- 0.09

dat$count=1
dat$crit =NA
dat$crit[dat$icu=="Yes"] <- 1
dat$crit[dat$icu=="No"] <- 0

dat$admitted =NA
dat$admitted[dat$admitted_to_hospital=="Yes"] <- 1
dat$admitted[dat$admitted_to_hospital=="No"] <- 0

dat$specimen_collection <- as.Date(dat$specimen_collection)
dat$specimen_collection_week <- week(dat$specimen_collection )
dat$specimen_collection_month <- month(dat$specimen_collection )

dat <- subset(dat, specimen_collection  <=as.Date("2020-09-01"))
datAggr <- dat %>%
  filter(!is.na(admitted) & !is.na(icu)) %>%
  filter(admitted_to_hospital=="Yes") %>%
  group_by(specimen_collection,specimen_collection_month) %>%
  summarize(count=sum(count),crit=sum(crit, na.rm=TRUE), admitted=sum(admitted, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(fraction_critical = crit/admitted,
         fraction_critical_avrg = round(rollmean(fraction_critical, 7, align = "right", fill = 0),2)) %>%
  group_by(specimen_collection_month) %>%
  mutate(fraction_critical_avrgmth = mean(fraction_critical_avrg))


datAggr %>% group_by(specimen_collection_month) %>%
  summarise(fraction_critical_avrgmth=mean(fraction_critical_avrgmth)) %>%
  pivot_wider(names_from=specimen_collection_month, values_from = fraction_critical_avrgmth) %>%
  mutate(redApr = `4`/ `3`,redMay = `5`/ `4`,redJun = `6`/ `5` )
param %>% select(fraction_critical_lo,fraction_critical_up) %>% unique() %>%
  mutate(red_lo =fraction_critical_lo/lag(fraction_critical_lo),
         red_up =fraction_critical_up/lag(fraction_critical_up))

p1 <- ggplot(data=datAggr) +
  geom_line( aes(x=specimen_collection, y =fraction_critical ),col="grey",size=0.3)+
  geom_line( aes(x=specimen_collection, y =fraction_critical_avrg ),col="black")+
  geom_line( aes(x=specimen_collection, y =fraction_critical_avrgmth ),col="dodgerblue4")+
  scale_y_continuous(breaks=seq(0,1,0.1), minor_breaks = seq(0,1,0.05),lim=c(0,1)) +
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  labs(title="",
       x="Specimen collection",
       y="ICU admission / hospital admissions")+
  theme_minimal() + customTheme


p2 <- ggplot(data=datAggr) +
  #geom_line( aes(x=specimen_collection, y =fraction_critical ),col="grey",size=0.3)+
  #geom_line( aes(x=specimen_collection, y =fraction_critical_avrg ),col="black")+
  geom_line( aes(x=specimen_collection, y =fraction_critical_avrgmth ),col="dodgerblue4")+
  geom_ribbon(data=param, aes(x=dates,ymin=fraction_critical_lo, ymax=fraction_critical_up),alpha=1, fill="dodgerblue4") +
  scale_y_continuous(breaks=seq(0,1,0.1), minor_breaks = seq(0,1,0.05),lim=c(0,1)) +
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  labs(title="",
       x="Specimen collection",
       y="ICU admission / hospital admissions")+
  theme_minimal() + customTheme

pSI5 <- plot_grid(p1,p2, nrow=1, labels=c("A","B"))
f_save_plot(
    plot_name = paste0("FigSI5"), pplot = pSI5,
    plot_dir = file.path(fig_dir), width =12, height = 6,scale=0.8
  )