# Title     : COVID-19 ICU overflow analysis
# Objective : Figure  comparison to data

source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))
customTheme <- f_getCustomTheme()
#trace_selection = TRUE
#if(trace_selection) fig_dir = fig_dir_traces

exp_name_counterfactual_1 <- "20201212_IL_regreopen50perc_counterfactual"
exp_name_counterfactual_2 <- "20201212_IL_regreopen100perc_counterfactual"
exp_name_baseline <- "20201212_IL_fit_to_Sep_baseline"

f_combineData <- function(exp_name){
  dat_list <- list()
  for(trace_selection in c(TRUE, FALSE)){
    dat_list[[length(dat_list)+1]] <- f_load_sim_data(
      exp_name=exp_name,sim_dir=sim_dir,fname="trajectoriesDat.csv",
      add_peak_cols=FALSE, add_trigger_cols=FALSE,addRt=FALSE,trace_selection=trace_selection) %>%
      filter( date <= baseline_date) %>%
      ungroup() %>%
      dplyr::select(date,sample_num,scen_num, hosp_det, crit_det,
                  infected_cumul, hosp_det_cumul, crit_det_cumul, death_det_cumul,
                  new_infected, new_hosp_det, new_crit_det, new_death_det ) %>%
      rename(ICU_census = crit_det,
             med_surg_census = hosp_det,
             deaths = new_death_det,
             admissions = new_hosp_det )  %>%
      pivot_longer(cols=-c(date,sample_num,scen_num), names_to = "outcome") %>%
      mutate(trace_selection=trace_selection)
  }
  dat <- dat_list %>% bind_rows() %>% filter(date <= baseline_date) %>% as.data.table()
  rm(dat_list)
  return(dat)
}

dat <- f_combineData(exp_name=counterfactual_exps[1])
head(dat)
datAggr <- dat %>% group_by(date,outcome,trace_selection) %>%
  summarise(n=n(),
            median = median(value),mean = mean(value),
            lower = quantile(value, probs=0.05, na.rm = TRUE),
            upper = quantile(value, probs=0.95, na.rm = TRUE))
summary(datAggr$n)

capacityDat <- load_new_capacity(11, filedate="20200915")
ref_dat <- f_load_ref_df(data_path) %>%
  mutate(date=as.Date(Date)) %>%
  filter(region==11 & date <= baseline_date) %>%
  rename(deaths=LL_deaths,
         admissions=LL_admissions) %>%
  select(date,confirmed_covid_icu, suspected_and_confirmed_covid_icu, covid_non_icu, deaths,admissions ) %>%
  rename(ICU_census = confirmed_covid_icu,
         suspected_and_confirmed_covid_icu = suspected_and_confirmed_covid_icu,
         med_surg_census = covid_non_icu,
         deaths = deaths) %>%
  pivot_longer(cols=-c(date), names_to = "outcome") %>%
  group_by(outcome) %>%
  arrange(date) %>%
  mutate( value_7avrg = rollmean(value, 7, align='right', fill=NA))

ref_dat$date <- as.character(ref_dat$date )
datAggr$date <- as.character(datAggr$date )
fitdat_median <- ref_dat %>%
  filter(outcome %in% channels & !is.na(value_7avrg)) %>%
  left_join(datAggr) %>%
  mutate(date=as.Date(date),
         month=month(date)) %>%
  group_by(outcome,trace_selection) %>%
  arrange(date) %>%
  mutate(mae = mae(median, value_7avrg),
         mse = mse(median, value_7avrg)) %>%
  group_by(outcome, month,trace_selection) %>%
  mutate(mae_mth = round(mae(median, value_7avrg),2),
         mse_mth = round(mse(median, value_7avrg),2)) %>%
  group_by(outcome, month,date,trace_selection) %>%
  mutate(mae_dt = round(mae(median, value_7avrg),2),
         mse_dt = round(mse(median, value_7avrg),2))

ref_dat$date <- as.character(ref_dat$date )
dat$date <- as.character(dat$date )
fitdat <- ref_dat %>%
  filter(outcome %in% channels  & !is.na(value_7avrg)) %>%
  left_join(dat) %>%
  mutate(date=as.Date(date),
         month=month(date)) %>%
  group_by(outcome,scen_num , sample_num, trace_selection) %>%
  arrange(date) %>%
  mutate(mae = mae(value, value_7avrg),
         mse = mse(value, value_7avrg)) %>%
  group_by(outcome,scen_num , sample_num, month,trace_selection) %>%
  mutate(mae_mth = round(mae(value, value_7avrg),2),
         mse_mth = round(mse(value, value_7avrg),2)) %>%
  group_by(outcome,scen_num , sample_num, month,date,trace_selection) %>%
  mutate(mae_dt = round(mae(value, value_7avrg),2),
         mse_dt = round(mse(value, value_7avrg),2))

ref_dat$date <- as.Date(ref_dat$date )
datAggr$date <- as.Date(datAggr$date )

channels <- c('admissions','med_surg_census', 'ICU_census','deaths')
pplot <- ggplot() +
  geom_line(data=subset(datAggr, outcome %in% channels & trace_selection =='TRUE'), aes(x=date, y=median),col="#d76127")+
  geom_ribbon(data=subset(datAggr, outcome %in% channels & trace_selection =='FALSE'),
              aes(x=date, ymin=lower, ymax=upper,fill=trace_selection),alpha=0.2)+
  geom_ribbon(data=subset(datAggr, outcome %in% channels & trace_selection =='TRUE'),
              aes(x=date, ymin=lower, ymax=upper, fill=trace_selection),alpha=0.4)+
  geom_point(data=subset(ref_dat, outcome %in% channels), aes(x=date, y=value))+
  geom_line(data=subset(ref_dat, outcome %in% channels), aes(x=date, y=value_7avrg))+
  facet_wrap(~outcome, scales="free",nrow=1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal()+customTheme+
  labs(x="",y="Total number",color="",fill="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#d76127","#d76127"))
  #scale_color_brewer(palette="Dark2")+
  #scale_fill_brewer(palette="Dark2")


#width =14, height = 3.5
f_save_plot(
  plot_name = paste0("data_comparison"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =14, height = 3.5
)
f_save_plot(
  plot_name = paste0("data_comparison"), pplot = pplot,
  plot_dir = file.path(fig_dir_traces), width =14, height = 3.5
)


pplot <- ggplot() +
  geom_point(data=subset(fitdat_median, outcome %in% channels), aes(x=value_7avrg, y=median, col=trace_selection))+
  geom_abline(intercept = 0, slope=1) +
  facet_wrap(~outcome, scales="free",nrow=1) +
  theme_minimal()+customTheme+
  labs(x="",y="Total number",color="",fill="")+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

f_save_plot(
  plot_name = paste0("data_comparison_scatter"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =14, height = 4
)
f_save_plot(
  plot_name = paste0("data_comparison_scatter"), pplot = pplot,
  plot_dir = file.path(fig_dir_traces), width =14, height = 4
)


### MAE
require(Metrics)
MAEdat <- fitdat %>% group_by(outcome)%>% summarize(MAE_avrg=mean(mae_dt))

pplot1 <- ggplot(data=subset(fitdat_median, !is.na(trace_selection))) +
  geom_point(aes(x=date, y=mae_dt,fill=trace_selection,col=trace_selection),alpha=0.4)+
  #geom_line(aes(x=date, y=mae_mth,fill=trace_selection,col=trace_selection),alpha=1)+
  facet_wrap(~outcome,scales="free",nrow=1) +
  customTheme+
  labs(x="",y="daily MAE",color="Trace selection",fill="Trace selection")+
  geom_vline(data=MAEdat, aes( xintercept=MAE_avrg)) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

pplot2 <- ggplot(data=subset(fitdat, !is.na(trace_selection))) +
  geom_density(aes(x=mae_dt,fill=trace_selection,col=trace_selection),alpha=0.4)+
  facet_wrap(~outcome,scales="free",nrow=1) +
  customTheme+
  labs(x="MAE",y="Density",color="Trace selection",fill="Trace selection")+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

pplot3 <- ggplot(data=subset(fitdat, !is.na(trace_selection))) +
  geom_histogram(aes(x=mae,fill=trace_selection,col=trace_selection),alpha=0.4)+
  facet_wrap(~outcome,scales="free",nrow=1) +
  customTheme+
  labs(x="MAE",y="N trajectories",color="Trace selection",fill="Trace selection")+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

pplot <- plot_grid(pplot1, pplot2, pplot3, labels=c("A","B","C"),ncol=1)

f_save_plot(
    plot_name = paste0("MAE_trace_selection"), pplot = pplot,
    plot_dir = file.path(fig_dir), width =14, height = 8
  )
f_save_plot(
    plot_name = paste0("MAE_trace_selection"), pplot = pplot,
    plot_dir = file.path(fig_dir_traces), width =14, height = 8
  )

