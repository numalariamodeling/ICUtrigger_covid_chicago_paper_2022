# Title     : Observed ICU capacity and availability in Chicago, 2020
# Objective : Describe ICU capacity data over time

source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))

customTheme <- f_getCustomTheme()

Chicago_pop = 2716921 #as used in experiment config yamls
capacityDat <- load_new_capacity(11, filedate="20200915")
ref_dat <- f_load_ref_df(data_path) %>%
  filter(region==11) %>%
  mutate(Date=as.Date(Date))

ccdat <- fread(file.path(data_path, "/covid_IDPH/Corona virus reports/capacity_by_covid_region.csv")) %>%
        dplyr::mutate(date=as.Date(date))%>%
        dplyr::filter(date <= as.Date("2020-12-31") & geography_level=="covid region" & geography_name==11) %>%
        dplyr::select(date,icu_used,icu_covid,icu_total,icu_noncovid,icu_availforcovid) %>%
        arrange(date) %>%
        dplyr::mutate(icu_covid_7avrg = rollmean(icu_covid, 7, align='right', fill=NA),
        icu_availforcovid_7avrg = rollmean(icu_availforcovid, 7, align='right', fill=NA),
                      icu_total_7avrg = rollmean(icu_total, 7, align='right', fill=NA))

pplot <- ggplot(data=ccdat) +
  geom_ribbon(aes(x=date, ymin=0, ymax=icu_total), fill="grey", alpha=0.7)+
  geom_ribbon(aes(x=date, ymin=0, ymax=icu_availforcovid), fill="dodgerblue2", alpha=0.9)+
  geom_line(aes(x=date,y=icu_availforcovid), col="dodgerblue3") +
  geom_line(aes(x=date,y=icu_availforcovid_7avrg), col=capacitycolor,size=0.5) +
  geom_line(aes(x=date,y=icu_covid), col="black")+
  geom_hline(yintercept=516, col=capacitycolor, linetype="dashed")+
  scale_y_continuous(expand=c(0,0), lim=c(0, 1250))+
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  geom_vline(xintercept=as.Date("2020-11-20")) +
  customTheme +
  labs(x="", y="Number of ICU beds")

f_save_plot(
  plot_name = paste0("S1_fig_15"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =10, height = 6
)

### Capacity and rel occupancy during peak at 1st wave
(ccdat_1st_wave_peak <- ccdat %>% filter(date <= as.Date("2020-09-01")) %>%
  filter(icu_covid_7avrg==max(icu_covid_7avrg,na.rm=TRUE)))

ccdat_1st_wave_peak$icu_availforcovid_7avrg - capacityDat$icu_available

### Average beds per pop
ccdat <- ccdat %>%
  mutate(icu_beds_covid_capacity_per10000 = (icu_availforcovid_7avrg / Chicago_pop) * 10000,
         icu_beds_total_capacity_per10000 = (icu_total_7avrg / Chicago_pop) * 10000 ) %>%
  mutate(first_wave=ifelse(date<=as.Date("2020-07-01"),1,0))

summary(ccdat$icu_beds_covid_capacity_per10000)
summary(ccdat$icu_beds_total_capacity_per10000)

## Separate before/after July 1st
tapply(ccdat$icu_beds_covid_capacity_per10000, ccdat$first_wave, summary)
tapply(ccdat$icu_beds_total_capacity_per10000, ccdat$first_wave, summary)

#### Additional descriptives
dat <- fread(file.path(data_path, "/covid_IDPH/Corona virus reports/emresource_by_region.csv")) 
table(dat$covid_region )
tapply(dat$n_hospitals, dat$covid_region, summary)
dat <- dat %>% filter( covid_region  == 11) 

dat <-fread(file.path(data_path, "/covid_IDPH/Corona virus reports/capacity_by_covid_region.csv")) %>%
  dplyr::mutate(date=as.Date(date))%>%
  dplyr::filter(geography_level=="covid region" &
                  geography_name==11) %>%
  mutate(medsurg_availforcovid = medsurg_total - medsurg_noncovid)

summary(dat$medsurg_total)

(mean(dat$medsurg_total) /Chicago_pop) *10000
(mean(dat$icu_total) /Chicago_pop) *10000
(mean(dat$vent_total) /Chicago_pop) *10000

(mean(dat$medsurg_availforcovid) /Chicago_pop) *10000
(mean(dat$icu_availforcovid,na.rm=TRUE) /Chicago_pop) *10000

### Capacity as 2nd stay at home order was implemented in November
capacityNovLabel<- capacityNov$percICU[capacityNov$date> as.Date("2020-11-21") & capacityNov$date<= as.Date("2020-11-22")]
pplot <- ggplot(data = subset(ccdat )) +
  geom_hline(yintercept = c(capacityNovLabel,80),linetype="dashed")+
  #geom_vline(xintercept =as.Date("2020-11-22"),linetype="dashed")+
  geom_line(aes(x = date, y = percICU, group = 1), alpha=1,col = "deepskyblue3",size=1.2) +
  scale_x_date(date_breaks = "30 days", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), lim=c(0,100)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  customTheme + labs( x = "", y = "% of ICU beds available\nfor COVID-19 filled")

f_save_plot(
  plot_name = paste0("S1_fig_15_supp"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =10, height = 6
)