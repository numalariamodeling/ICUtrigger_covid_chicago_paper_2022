# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 5C

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection <-  TRUE
delay = "1daysdelay"

if(trace_selection) fig_dir = fig_dir_traces
if(delay == "7daysdelay"){
  exp_names <- c(exp_names_50_delay7,exp_names_100_delay7)
  subtitle="immediate mitigation:\n1 day after threshold reached"
}
if(delay == "1daysdelay"){
  subtitle="delayed mitigation:\n7 days after threshold reached"
  exp_names <- c(exp_names_50_delay1,exp_names_100_delay1)
}

f_combineData <- function(exp_names, sim_end_date, trace_selection){
  dat_list <- list()
  for(exp_name in exp_names ){
    print(exp_name)
    tempdat <- f_load_sim_data(exp_name=exp_name,sim_dir = sim_dir ,
                               fname="trajectoriesDat_region_11_trimfut.csv",
                               add_peak_cols=TRUE,addRt=FALSE,
                               trace_selection =trace_selection) %>%
      filter(date>=baseline_date &  date <= sim_end_date) %>%
      dplyr::group_by(exp_name, group_id) %>%
      filter(trigger_activated==1 & date==date_peak) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date,date_peak, trigger_activated,triggerDate , crit_det, crit_det_peak)

    dat_list[[length(dat_list)+1]]   <- tempdat %>%
      dplyr::group_by(exp_name, group_id) %>%
      dplyr::mutate(rel_occupancy_peak = crit_det_peak/516) %>%
      dplyr::group_by(rollback,delay, capacity_multiplier,  reopen) %>%
      dplyr::summarize(mean_val=mean(rel_occupancy_peak,na.rm=TRUE),
                       median_val=median(rel_occupancy_peak,na.rm=TRUE),
                       q5=quantile(rel_occupancy_peak,0.05,na.rm=TRUE),
                       q95=quantile(rel_occupancy_peak,0.95,na.rm=TRUE))
    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

print(exp_names)
p5C_dat <- f_combineData(exp_names = exp_names, sim_end_date, trace_selection)

p5C_dat$rollback <- factor(p5C_dat$rollback,
                           levels=c("pr2","pr4","pr6","pr8"),
                           labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))

p5C <- ggplot(data=p5C_dat)+
  geom_ribbon( aes(x=capacity_multiplier,ymin=q5,ymax=q95,fill=rollback),alpha=0.3) +
  geom_line( aes(x=capacity_multiplier,y=median_val,col=rollback)) +
  facet_wrap(~reopen,scales='free_y', ncol=1) +
  scale_y_continuous(lim=c(0,4), breaks=seq(0,4,0.5), labels=seq(0,4,0.5)) +
  scale_x_continuous(breaks=seq(0,1,0.2), labels=seq(0,100,20)) +
  geom_hline(yintercept=1,col=capacitycolor,linetype='dashed')+
  scale_fill_manual(values=mitigation_colors)+
  scale_color_manual(values=mitigation_colors)+
  labs(title="",
       subtitle=subtitle,
       x='ICU occupancy threshold\nto trigger mitigation',
       color='Mitigation\nstrengths',
       fill='Mitigation\nstrengths',
       y='peak ICU occupancy as fraction of ICU capacity')+
  customTheme

fwrite(p5C_dat, file.path(fig_dir,"csv",paste0("p5Cdat_",delay,".csv")))

f_save_plot(
  plot_name = paste0("Fig5C_",delay), pplot = p5C,
  plot_dir = file.path(fig_dir), width =7, height = 8
)
