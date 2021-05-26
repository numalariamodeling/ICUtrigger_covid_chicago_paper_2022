# Title     : TODO
# Objective : TODO
# Created by: mrm9534
# Created on: 5/19/2021

require(data.table)
require(dplyr)

simoutdir <- "/home/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/"
stem <- "20210517_IL_localeEMS_11"
exp_names <- list.files(simoutdir, pattern=stem)
exp_names_counterfactual <- c("20210516_IL_localeEMS_11_100perc_counterfactual_reopen","20210516_IL_localeEMS_11_50perc_counterfactual_reopen")

##### Get sample_num where trigger is activated
iter=0
for(exp_name in exp_names){
  print(exp_name)
  iter=iter+1
  dat <- fread(file.path(simoutdir,exp_name,'trajectoriesDat_region_11_trimfut.csv'))
  scens <- dat %>%  filter(`crit_det_EMS-11` >= 516 * `capacity_multiplier`) %>%
  dplyr::select(scen_num,sample_num,capacity_multiplier) %>% unique()  
  if(iter==1)scensAll = scens
  if(iter>1){
    scensAll = merge(scensAll,scens,all=FALSE)
  }
  fwrite(scens, file.path(simoutdir, exp_name ,"sample_num_trigger.csv"))
}
fwrite(scensAll, file.path(simoutdir,"sample_num_trigger_all.csv"))

#### Get same best 100 traces
##check if ranking the same
traces1 <- fread(file.path(simoutdir,exp_names_counterfactual[1],'traces_ranked_region_11.csv')) 
traces2 <- fread(file.path(simoutdir,exp_names_counterfactual[2],'traces_ranked_region_11.csv'))
#traces1$sample_num == traces2$sample_num
traces1 <- traces1 %>% filter(sample_num %in% unique(scensAll$sample_num ))
traces2 <- traces2 %>% filter(sample_num %in% unique(scensAll$sample_num ))
traces <- traces1$sample_num[traces1$sample_num %in% traces2$sample_num][c(1:100)]
fwrite(as.data.frame(traces), file.path(simoutdir,"sample_num_traces_all.csv"))


for(exp_name in exp_names_counterfactual){
  print(exp_name)
  dat <- fread(file.path(simoutdir,exp_name,'trajectoriesDat.csv'))
  ### Filter by trigger activated
  dat <- dat %>% filter(sample_num %in% unique(scensAll$sample_num ))
  fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trigger.csv"))
  ### Filter by traces
  dat <- dat %>% filter(sample_num %in% traces)
  fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_triggertraces.csv"))
}  


for(exp_name in exp_names){
  print(exp_name)
  dat <- fread(file.path(simoutdir,exp_name,'trajectoriesDat_region_11_trimfut.csv'))
  ### Filter by trigger activated
  dat <- dat %>% filter(sample_num %in% unique(scensAll$sample_num ))
  fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trimfuttrigger.csv"))
  ### Select best (same) 100 traces
  dat <- dat %>% filter(sample_num %in% traces)
  fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trimfuttriggertraces.csv"))
}



