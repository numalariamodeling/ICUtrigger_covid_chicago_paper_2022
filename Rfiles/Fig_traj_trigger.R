# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 6

source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection <-  TRUE
if(trace_selection) fig_dir = fig_dir_traces


f_combineData <- function(exp_names,trace_selection){
  dat_list <- list()
  for(exp_name in exp_names ){
    print(exp_name)
    tempdat <- f_load_sim_data(exp_name=exp_name,sim_dir = sim_dir ,
                               fname="trajectoriesDat_region_11_trimfut.csv",
                               add_peak_cols=TRUE,addRt=FALSE,trace_selection=trace_selection) %>%
      filter(date>=baseline_date &  date <= sim_end_date) %>%
      dplyr::group_by(exp_name, group_id) %>%
      filter(trigger_activated==1) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date,date_peak, trigger_activated,triggerDate , crit_det, crit_det_peak) %>%
      filter(date==max(date)) %>%
      select(exp_name, reopen, rollback, delay,group_id, sample_num, scen_num, capacity_multiplier) %>%
      unique() %>%
      group_by(exp_name, reopen, rollback, delay,capacity_multiplier) %>%
      summarize( n.val = n())

    dat_list[[length(dat_list)+1]]  <- tempdat
    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

dat <- f_combineData(exp_names = c(exp_names_50_delay1,exp_names_100_delay1),
                       trace_selection=trace_selection)

dat$rollback_fct <- factor(dat$rollback,
                           levels=c("pr2","pr4","pr6","pr8"),
                           labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))
dat$reopen_fct <- factor(dat$reopen,
                           levels=c("50perc","100perc"),
                           labels=c("Low transmission increase","High transmission increase"))



## For supplement
pplot <- ggplot(data=subset(dat, delay=="1daysdelay")) +
  geom_bar(aes(x=as.factor(capacity_multiplier*100),  y=n.val, fill=reopen,
               group=interaction(rollback,reopen)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat="identity") +
  facet_grid(reopen_fct~rollback_fct) +
  theme_minimal()+
  #customTheme+
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  labs(x="ICU occupancy threshold to trigger mitigation (%)",
       y="number of trajectories\nabove capacity") +
  theme(legend.position="none", axis.ticks = element_line(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


f_save_plot(
  plot_name = paste0("Fig_SI_trigger_barplot"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =10, height = 6 ,scale=0.8)
