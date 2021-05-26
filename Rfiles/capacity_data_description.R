# Title     : Observed ICU capacity and availability in Chicago, 2020
# Objective : Describe ICU capacity data over time


source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))

customTheme <- f_getCustomTheme()
region  <- c("illinois",c(1:11))
popdat <- as.data.frame(cbind(region,pop_2018))
popdat$pop_2018 <- as.numeric(popdat$pop_2018)

capacityDat <- load_new_capacity(11, filedate="20200915")
ref_dat <- f_load_ref_df(data_path) %>%
  filter(region==11) %>%
  mutate(Date=as.Date(Date))

ccdat<- fread(file.path(data_path, "/covid_IDPH/Corona virus reports/capacity_by_covid_region.csv")) %>%
        dplyr::mutate(date=as.Date(date))%>%
        dplyr::filter(geography_level=="covid region" & geography_name==11) %>%
        dplyr::select(date,icu_used,icu_covid,icu_total,icu_noncovid,icu_availforcovid) %>%
        arrange(date) %>%
        dplyr::mutate(icu_covid_7avrg = rollmean(icu_covid, 7, align='right', fill=NA),
        icu_availforcovid_7avrg = rollmean(icu_availforcovid, 7, align='right', fill=NA),
                      icu_total_7avrg = rollmean(icu_total, 7, align='right', fill=NA))

ccdat %>%
  dplyr::select(c(date,icu_used,icu_covid,icu_total,icu_noncovid,icu_availforcovid)) %>%
  pivot_longer(cols=-c('date')) %>%
  ggplot()+
  geom_line(aes(x=date, y=value, col=name))+
  scale_x_date(date_breaks="1 month", date_labels="%b\n%Y")+
  labs(x="",color="")

ccdat %>%
  ggplot()+
  geom_point(aes(x=date, y=icu_covid/icu_availforcovid, col='compared against actual capacity'),alpha=0.7)+
  geom_point(aes(x=date, y=icu_covid/capacityDat$icu_available,col='assumed fixed capacity Sep'),alpha=0.7)+
  geom_line(aes(x=date, y=icu_covid_7avrg/icu_availforcovid_7avrg, col='compared against actual capacity'),size=1.1)+
  geom_line(aes(x=date, y=icu_covid_7avrg/capacityDat$icu_available,col='assumed fixed capacity Sep'),size=1.1)+
  scale_x_date(date_breaks="1 month", date_labels="%b\n%Y")+
  labs(x="",color="")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position='top')+
  scale_y_continuous(breaks=seq(0,1,0.1),
                     labels=seq(0,1,0.1),lim=c(0,1))

### Capacity and rel occupancy during peak at 1st wave
(ccdat_1st_wave_peak <- ccdat %>% filter(date <= as.Date("2020-09-01")) %>%
  filter(icu_covid_7avrg==max(icu_covid_7avrg,na.rm=TRUE)))

ccdat_1st_wave_peak$icu_availforcovid_7avrg - capacityDat$icu_available

### Average beds per pop
ccdat <- ccdat %>%
  mutate(region="11") %>%
  left_join(popdat) %>%
  mutate(icu_beds_covid_capacity_per10000 = (icu_availforcovid_7avrg / pop_2018) * 10000,
         icu_beds_total_capacity_per10000 = (icu_total_7avrg / pop_2018) * 10000 ) %>%
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

(mean(dat$medsurg_total) /popdat[popdat$region==11,"pop_2018"]) *10000
(mean(dat$icu_total) /popdat[popdat$region==11,"pop_2018"]) *10000
(mean(dat$vent_total) /popdat[popdat$region==11,"pop_2018"]) *10000


(mean(dat$medsurg_availforcovid) /popdat[popdat$region==11,"pop_2018"]) *10000
(mean(dat$icu_availforcovid,na.rm=TRUE) /popdat[popdat$region==11,"pop_2018"]) *10000
