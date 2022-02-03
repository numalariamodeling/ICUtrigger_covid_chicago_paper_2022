require(data.table)
require(dplyr)

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
simoutdir <- simulation_output
stem <- "reopen"
exp_names <- list.files(simulation_output, pattern = stem)
exp_names_counterfactual <- c("100perc_counterfactual_reopen", "50perc_counterfactual_reopen")

##### Get sample_num where trigger is activated
iter = 0
for (exp_name in exp_names) {
  print(exp_name)
  iter = iter + 1
  dat <- fread(file.path(simoutdir, exp_name, 'trajectoriesDat_region_11_trimfut.csv'))
  scens <- dat %>%
    filter(`crit_det_EMS-11` >= 516 * `capacity_multiplier`) %>%
    dplyr::select(scen_num, sample_num, capacity_multiplier) %>%
    unique()
  if (iter == 1)scensAll = scens
  if (iter > 1) {
    scensAll = merge(scensAll, scens, all = FALSE)
  }
  fwrite(scens, file.path(simoutdir, exp_name, "sample_num_trigger.csv"))
}
fwrite(scensAll, file.path(simoutdir, "sample_num_trigger_all.csv"))

#### Get same best 100 traces
##check if ranking the same
traces1 <- fread(file.path(simoutdir, exp_names_counterfactual[1], 'traces_ranked_region_11.csv'))
traces2 <- fread(file.path(simoutdir, exp_names_counterfactual[2], 'traces_ranked_region_11.csv'))
#traces1$sample_num == traces2$sample_num
traces1 <- traces1 %>% filter(sample_num %in% unique(scensAll$sample_num))
traces2 <- traces2 %>% filter(sample_num %in% unique(scensAll$sample_num))
traces_dat <- traces1[traces1$sample_num %in% traces2$sample_num][c(1:100),]

fwrite(as.data.frame(traces_dat$sample_num), file.path(simoutdir, "sample_num_traces_all.csv"))
fwrite(traces_dat, file.path(simoutdir, "traces_dat_all.csv"))


for (exp_name in exp_names_counterfactual) {
  print(exp_name)
  dat <- fread(file.path(simoutdir, exp_name, 'trajectoriesDat.csv'))
  ### Filter by trigger activated
  dat <- dat %>% filter(sample_num %in% unique(scensAll$sample_num))
  fwrite(dat, file.path(simoutdir, exp_name, "trajectoriesDat_region_11_trigger.csv"))
  ### Filter by traces
  dat <- dat %>% filter(sample_num %in% traces)
  fwrite(dat, file.path(simoutdir, exp_name, "trajectoriesDat_region_11_triggertraces.csv"))
}


for (exp_name in exp_names) {
  print(exp_name)
  dat <- fread(file.path(simoutdir, exp_name, 'trajectoriesDat_region_11_trimfut.csv'))
  ### Filter by trigger activated
  dat <- dat %>% filter(sample_num %in% unique(scensAll$sample_num))
  fwrite(dat, file.path(simoutdir, exp_name, "trajectoriesDat_region_11_trimfuttrigger.csv"))
  ### Select best (same) 100 traces
  dat <- dat %>% filter(sample_num %in% traces)
  fwrite(dat, file.path(simoutdir, exp_name, "trajectoriesDat_region_11_trimfuttriggertraces.csv"))
}



