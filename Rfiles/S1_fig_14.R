# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 6

source(file.path('Rfiles/settings.R'))
source(file.path('Rfiles/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection <-  FALSE
if(trace_selection) fig_dir = fig_dir_traces


f_combineData <- function(exp_names){
  dat_list <- list()
  for(exp_name in exp_names ){
    print(exp_name)
    tempdat <- f_load_sim_data(exp_name=exp_name,sim_dir = sim_dir ,
                               fname="trajectoriesDat_region_11_trimfut.csv",
                               add_peak_cols=TRUE,addRt=FALSE) %>%
      filter(date>=baseline_date &  date <= sim_end_date) %>%
      dplyr::group_by(exp_name, group_id) %>%
      filter(trigger_activated==1 & capacity_multiplier %in% c(0.2,0.4,0.6,0.8,1)) %>%
      dplyr::select(exp_name, group_id, sample_num, scen_num, capacity_multiplier, reopen, rollback, delay,
                    date,date_peak, trigger_activated,triggerDate , crit_det, crit_det_peak)

    dat_list[[length(dat_list)+1]]  <- tempdat %>%
      f_trigger_dat() %>%
      dplyr::mutate(time_since_trigger=date-triggerDate,
                      time_since_trigger=round(time_since_trigger,0))

    rm(tempdat)
  }

  dat <- dat_list %>% bind_rows()
  rm(dat_list)
  return(dat)
}

p6dat <- f_combineData(exp_names = c(exp_names_50_delay14,exp_names_100_delay14))

above_threshold <-  p6dat %>%
  mutate(date=as.Date(date)) %>%
  dplyr::select(scen_num, sample_num,capacity_multiplier, exp_name, date,
                group_id, reopen, delay, rollback,crit_det) %>%
  ungroup() %>%
  group_by(scen_num, exp_name,group_id,reopen,delay,capacity_multiplier, rollback) %>%
  filter(crit_det>=516) %>%
  mutate(date_min=min(date,na.rm=TRUE) ,
         date_max=max(date,na.rm=TRUE),
         type='threshold',
         subtype='above_threshold') %>%
  filter(date==min(date))
above_threshold$days_above = as.numeric(as.Date(above_threshold$date_max) - as.Date(above_threshold$date_min))

above_thresholdAggr <- above_threshold %>%
  filter(capacity_multiplier %in% c(0.2,0.4,0.6,0.8,1)) %>%
  group_by( exp_name,reopen,delay,capacity_multiplier, rollback) %>%
  dplyr::summarize(
    n.val = n(),
    mean = mean(days_above),
    median =median(days_above),
    q5 = quantile(days_above, probs = 0.05, na.rm = TRUE),
    q95 = quantile(days_above, probs = 0.95, na.rm = TRUE)
  )


print(counterfactual_exps)
dat_list <- list()
for(exp_name in counterfactual_exps){
  print(exp_name)
  dat_list[[length(dat_list)+1]] <- f_load_sim_data(
    exp_name,fname="trajectoriesDat.csv",
    sim_dir,add_peak_cols=FALSE, add_trigger_cols=FALSE,addRt=FALSE)%>%
    dplyr::filter(date>=baseline_date &  date <= sim_end_date) %>%
    dplyr::select(scen_num, sample_num, exp_name, date, reopen, crit_det)  %>%
    mutate(date=as.Date(date)) %>%
    dplyr::select(scen_num, sample_num,  exp_name, date, reopen,  crit_det) %>%
    ungroup() %>%
    group_by(scen_num, exp_name,reopen) %>%
    filter(crit_det>=516) %>%
    mutate(date_min=min(date,na.rm=TRUE) ,
           date_max=max(date,na.rm=TRUE)) %>%
    filter(date==min(date))
}
above_threshold_counter <- dat_list %>% bind_rows()
rm(dat_list)

above_threshold_counter$days_above = as.numeric(as.Date(above_threshold_counter$date_max) - as.Date(above_threshold_counter$date_min))
nomitigation_50perc <- above_threshold_counter %>% filter(reopen=="50perc") %>% ungroup() %>%  summarize(days_above=mean(days_above))
nomitigation_100perc <- above_threshold_counter %>% filter(reopen=="100perc") %>% ungroup() %>%  summarize(days_above=mean(days_above))
nomitigation_50perc <- nomitigation_50perc$days_above
nomitigation_100perc <- nomitigation_100perc$days_above


above_threshold$rollback <- factor(above_threshold$rollback,
                                   levels=c("pr2","pr4","pr6","pr8"),
                                   labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))

above_thresholdAggr$rollback <- factor(above_thresholdAggr$rollback,
                                            levels=c("pr2","pr4","pr6","pr8"),
                                       labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))

## For supplement
p6Abar <- ggplot(data=subset(above_thresholdAggr, delay=="1daysdelay")) +
  geom_bar(aes(x=as.factor(capacity_multiplier*100),  y=n.val, fill=reopen,
               group=interaction(rollback,reopen)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat="identity") +
  facet_grid(~rollback) +
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  labs(x="ICU occupancy threshold to trigger mitigation (%)",
       y="number of trajectories\nabove capacity") +
  theme(legend.position="none")

p6Bbar <- ggplot(data=subset(above_thresholdAggr, reopen=="100perc")) +
  geom_bar(aes(x=as.factor(capacity_multiplier*100),  y=n.val, fill=delay,
               group=interaction(rollback,delay)),
           position = position_dodge2(width = 0.9, preserve = "single"),
           stat="identity") +
  facet_grid(~rollback) +
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  labs(x="ICU occupancy threshold to trigger mitigation (%)",
       y="number of trajectories\nabove capacity") +
  theme(legend.position="none")

p6bar <- plot_grid(p6Abar,p6Bbar, ncol=1, labels=c("A","B"))
f_save_plot(
  plot_name = paste0("Fig6_bar_supp"), pplot = p6bar,
  plot_dir = file.path(fig_dir), width =10, height = 6 ,scale=0.8)

p6A <-  ggplot(data=subset(above_threshold,delay=="1daysdelay")) +
  geom_jitter(aes(x=as.factor(capacity_multiplier*100), y=days_above,fill=reopen,shape=reopen,alpha=reopen,
                  group=interaction(rollback,reopen)),position = position_jitterdodge(),col='black',size=0.9)+
  geom_pointrange(data=subset(above_thresholdAggr, n.val>=10 & delay=="1daysdelay") ,
                  aes(x=as.factor(capacity_multiplier*100), y=mean,ymin=mean, ymax=mean, fill=reopen,
                      group=interaction(rollback,reopen)),stat='identity',
                  position=position_dodge(1),shape=21,color="black",size=0.6)+
  theme_minimal()+
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  geom_hline(yintercept=c(nomitigation_50perc,nomitigation_100perc)) +
  scale_shape_manual(values=c(21,21)) +
  scale_alpha_manual(values=c(0.5,0.5)) +
  scale_y_continuous(breaks=seq(0,110, 30), labels=seq(0,110, 30) ) +
  facet_grid(~rollback)+
  labs(x="ICU occupancy threshold to trigger mitigation (%)",
       y="number of days above capacity") +
  theme(legend.position="none")

f_save_plot(
  plot_name = paste0("Fig6A"), pplot = p6A,
  plot_dir = file.path(fig_dir), width =10, height = 3,scale=0.8
  )

p6B <-  ggplot(data=subset(above_threshold,reopen=="100perc")) +
  geom_jitter(aes(x=as.factor(capacity_multiplier*100), y=days_above,fill=delay,shape=delay,alpha=delay,
                  group=interaction(rollback,delay)),position = position_jitterdodge(),col='black',size=0.9)+
  geom_pointrange(data=subset(above_thresholdAggr, n.val>=10 & reopen=="100perc") ,
                  aes(x=as.factor(capacity_multiplier*100), y=mean,ymin=mean, ymax=mean, fill=delay,
                      group=interaction(rollback,delay)),stat='identity',
                  position=position_dodge(1),shape=21,color="black",size=0.6)+
  theme_minimal()+
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  geom_hline(yintercept=c(nomitigation_50perc,nomitigation_100perc)) +
  scale_shape_manual(values=c(21,21)) +
  scale_alpha_manual(values=c(0.5,0.5)) +
  scale_y_continuous(breaks=seq(0,110, 30), labels=seq(0,110, 30) ) +
  facet_grid(~rollback)+
  labs(x="ICU occupancy threshold to trigger mitigation (%)",
       y="number of days above capacity") +
  theme(legend.position="none")


f_save_plot(
  plot_name = paste0("S1_fig_14"), pplot = p6B,
  plot_dir = file.path(fig_dir), width =10, height = 3,scale=0.8
  )

fwrite(above_threshold, file.path(fig_dir,"csv","S1_fig_14.csv"))
fwrite(above_thresholdAggr, file.path(fig_dir,"csv","S1_fig_14.csv"))

