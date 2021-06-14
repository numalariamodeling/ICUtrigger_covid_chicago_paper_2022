# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Scenario completeness

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()


exp_names <- list.dirs(sim_dir, full.names = FALSE)
exp_names <- exp_names[grep("triggeredrollback_reopen",exp_names)]
exp_names <- exp_names[!(grepl("14daysdelay",exp_names))]
column_names <- c("n_trajectories", "n_trajectories_success", "n_trajectories_trigger", "n_trajectories_peakBeforeDec",
                  "n_traces", "n_traces_trigger", "n_traces_peakBeforeDec")
scen_table <- matrix(NA, length(exp_names), length(column_names)+1)
capacity_multipliers <- seq(0,1,0.1)

column_names1 <- c("n_trajectories_success", "n_trajectories_trigger", "n_traces", "n_traces_trigger")
scen_table1 <- matrix(NA, length(exp_names) * length(capacity_multipliers), length(column_names1)+2)

colnames(scen_table) <- c("exp_name",column_names)
colnames(scen_table1) <- c("exp_name", "threshold",column_names1)

traces <- fread(file.path(sim_dir,"sample_num_traces_all.csv"))

iter=0
for(exp_name in exp_names){
  print(exp_name)
  iter=iter+1
  dat <- fread(file.path(sim_dir,exp_name,'trajectoriesDat_region_11_trimfut.csv'),
               select = c("crit_det_EMS-11","capacity_multiplier","sample_num","scen_num","time")) %>%
    mutate(date = as.Date("2020-01-01") + time) %>%
    filter(date >= baseline_date & date <= sim_end_date) %>%
    rename(crit_det=`crit_det_EMS-11`)

  dat_trigger <- dat %>%
    filter(date > trigger_min_date) %>%
    group_by(scen_num, sample_num, capacity_multiplier) %>%
    filter(crit_det >= 516 * capacity_multiplier ) %>%
    filter(date==min(date))

  dat_peakDec <-  dat %>%
    filter(date > trigger_min_date) %>%
    group_by(scen_num, sample_num, capacity_multiplier) %>%
    filter(crit_det ==max(crit_det) ) %>%
    filter(date==min(date)) %>%
    filter(date <= as.Date("2020-12-31"))

  dat_traces <- dat %>% filter(sample_num %in% traces$traces)

  dat_traces_trigger <- dat_traces %>%
    filter(date > trigger_min_date) %>%
    group_by(scen_num, sample_num, capacity_multiplier) %>%
    filter(crit_det >= 516 * capacity_multiplier ) %>%
    filter(date==min(date))

  dat_traces_peakDec <-  dat_traces %>%
    filter(date > trigger_min_date) %>%
    group_by(scen_num, sample_num, capacity_multiplier) %>%
    filter(crit_det ==max(crit_det) ) %>%
    filter(date==min(date)) %>%
    filter(date <= as.Date("2020-12-31"))

  scen_table[iter,1] <- exp_name
  scen_table[iter,2] <- 400 * 11
  scen_table[iter,3] <- length(unique(dat$scen_num))
  scen_table[iter,4] <- length(unique(dat_trigger$scen_num))
  scen_table[iter,5] <- length(unique(dat_peakDec$scen_num))
  scen_table[iter,6] <- length(unique(dat_traces$scen_num))
  scen_table[iter,7] <- length(unique(dat_traces_trigger$scen_num))
  scen_table[iter,8] <- length(unique(dat_traces_peakDec$scen_num))


  # iter1 <- 0
  # for(cap in unique(dat$capacity_multiplier)){
  #   iter1 <-  iter1 + 1
  #   iter2 <- iter + iter1
  #
  #   scen_table1[iter2, 1] <- exp_name
  #   scen_table1[iter2, 2] <- cap
  #   scen_table1[iter2, 3] <- length(unique(dat$scen_num[dat$capacity_multiplier == cap]))
  #   scen_table1[iter2, 4] <- length(unique(dat_trigger$scen_num[dat_trigger$capacity_multiplier == cap]))
  #   scen_table1[iter2, 5] <- length(unique(dat_traces$scen_num[dat_traces$capacity_multiplier == cap]))
  #   scen_table1[iter2, 6] <- length(unique(dat_traces_trigger$scen_num[dat_traces_trigger$capacity_multiplier == cap]))
  # }

}

scen_dat <- scen_table %>% as.data.frame() %>%
  mutate(exp_name = gsub("20210517_IL_localeEMS_11_","", exp_name),
         exp_name = gsub("20210518_IL_localeEMS_11_","", exp_name),
         exp_name = gsub("20210527_IL_localeEMS_11_","", exp_name),
         exp_name = gsub("20210528_IL_localeEMS_11_","", exp_name),
         exp_name = gsub("_triggeredrollback_reopen","", exp_name)) %>%
  separate(exp_name , into= c("reopen", "delay", "mitigation"), sep = "_") %>%
  mutate(n_trajectories_success=as.numeric(n_trajectories_success),
         n_trajectories=as.numeric(n_trajectories),
         n_trajectories_trigger=as.numeric(n_trajectories_trigger),
         n_trajectories_peakBeforeDec=as.numeric(n_trajectories_peakBeforeDec),
         n_traces=as.numeric(n_traces),
         n_traces_trigger=as.numeric(n_traces_trigger),
         n_traces_peakBeforeDec=as.numeric(n_traces_peakBeforeDec)) %>%
  mutate(success = round(n_trajectories_success/ n_trajectories,3),
         trigger = round( n_trajectories_trigger/ n_trajectories_success,3),
         peakBeforeDec = round(n_trajectories_peakBeforeDec/ n_trajectories_success,3),
         traces = round(n_traces/ n_trajectories_success,3),
         traces_trigger = round(n_traces_trigger/ n_traces,3),
         traces_peakBeforeDec = round(n_traces_peakBeforeDec/ n_traces,3))

scen_dat$mitigation <- factor(scen_dat$mitigation,
                              levels=c("pr2","pr4","pr6","pr8"),
                              labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))
scen_dat$delay <- factor(scen_dat$delay,
                              levels=c("1daysdelay","7daysdelay"),
                              labels=c("1 day","7 days"))
scen_dat$reopen <- factor(scen_dat$reopen,
                              levels=c("100perc","50perc"),
                              labels=c("High","Low"))

fwrite(scen_dat, file.path(git_dir,"out", "S1_table_5.csv"))



