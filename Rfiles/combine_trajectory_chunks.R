
require(data.table)
require(dplyr)
simoutdir <- "/home/mrm9534/Box/NU-malaria-team/projects/covid_chicago/cms_sim/simulation_output/"
stem <- "IL_localeEMS_11"
exp_names <- list.files(simoutdir, pattern=stem)

f_combine_csv <- function(exp_name){

  if(file.exists(file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trim.csv"))){
    dat <- fread(file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trim.csv")) %>% dplyr::select(-sample_num)
    sampledf <- fread(file.path(simoutdir,exp_name,"sampled_parameters.csv"))    
    sampledf <- sampledf[,c("scen_num","sample_num1","capacity_multiplier")]
    dat <- dat %>% left_join(sampledf) %>% rename(sample_num=sample_num1)   
    dat <- dat[dat$time>=244,]
    fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trimfut.csv"))
  }else{
    trajectories <- list.files(file.path(simoutdir,exp_name), pattern = '_trim')
    trajectories <- trajectories[!(trajectories %in% "trajectoriesDat_region_11_trim.csv")]
    trajectories <- trajectories[!(trajectories %in% "trajectoriesDat_region_11_trimfut.csv")]
    datList <- list()
    for(traj in trajectories){
      dat <- fread(file.path(simoutdir,exp_name,traj))
      sampledf <- fread(file.path(simoutdir,exp_name,"sampled_parameters.csv"))    
      sampledf <- sampledf[,c("scen_num","sample_num1","capacity_multiplier")]
  
      dat <- dat[,c("time","startdate","scen_num","Ki_t_EMS-11","infected_cumul_EMS-11",
                    "critical_EMS-11", "crit_det_cumul_EMS-11","crit_cumul_EMS-11","crit_det_EMS-11",
                    "death_det_cumul_EMS-11","deaths_EMS-11",
                    "hospitalized_EMS-11","hosp_cumul_EMS-11", "hosp_det_cumul_EMS-11","hosp_det_EMS-11")]
                    
      dat <- dat %>% left_join(sampledf) %>% rename(sample_num=sample_num1)              
      datList[[length(datList)+1]] <- dat
    }
      dat <- do.call(rbind.data.frame, datList)
      fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trim.csv"))
      dat <- dat[dat$time>=244,]
      fwrite(dat, file.path(simoutdir,exp_name,"trajectoriesDat_region_11_trimfut.csv"))
    }
  }


for(exp_name in exp_names){
  print(exp_name)
  f_combine_csv(exp_name)
}