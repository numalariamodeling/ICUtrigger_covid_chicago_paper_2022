# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Helper functions

f_save_plot <- function(pplot, plot_name, plot_dir, width = 14, height = 8, scale = 1) {
  ggsave(paste0(plot_name, ".png"), plot = pplot, path = plot_dir,
         width = width, height = height, scale = scale, device = "png")
  if (!dir.exists(file.path(plot_dir, "pdf"))) { dir.create(file.path(plot_dir, "pdf")) }
  ggsave(paste0(plot_name, ".pdf"), plot = pplot, path = file.path(plot_dir, "pdf"),
         width = width, height = height, scale = scale, device = "pdf", useDingbats = FALSE)
}

f_load_ref_df <- function(data_path, LL_file_date = NULL) {

      #' Load reference data and merge files
      #'
      #' Load reference data, EMResource and Line list data
      #' Per default adds the restore regions and selects specified variables.
      #' Output a merged dataframe in wide format.
      #' @param data_path directory path to uppr level that includes covid_IDPH , covid_chicago..
      #' @param LL_file_date date of the lates line list data
      #'

  emresource <- read.csv(file.path(data_path, "covid_IDPH/Corona virus reports/emresource_by_region.csv")) %>%
    dplyr::mutate(
      date_of_extract = as.Date(date_of_extract),
      suspected_and_confirmed_covid_icu = suspected_covid_icu + confirmed_covid_icu
    ) %>%
    dplyr::rename(
      Date = date_of_extract,
      region = covid_region,
    ) %>%
    dplyr::select(
      Date, region, suspected_and_confirmed_covid_icu,
      confirmed_covid_deaths_prev_24h, confirmed_covid_icu, covid_non_icu
    )

  LLdir <- file.path(data_path, "covid_IDPH", "Cleaned Data")
  if (is.null(LL_file_date)) {
    LLfiles <- list.files(LLdir)[grep("aggregated_covidregion", list.files(LLdir))]
    LL_file_dates <- as.numeric(gsub("_jg_aggregated_covidregion.csv", "", LLfiles))
    LL_file_date <- max(LL_file_dates)
  }


  ref_df <- read.csv(file.path(LLdir, paste0(LL_file_date, "_jg_aggregated_covidregion.csv")))

  ref_df <- ref_df %>%
    dplyr::rename(
      Date = date,
      region = covid_region,
      LL_deaths = deaths,
      LL_cases = cases,
      LL_admissions = admissions
    )

  ref_df$Date <- as.Date(ref_df$Date)
  emresource$Date <- as.Date(emresource$Date)
  out <- left_join(emresource, ref_df, by = c("Date", "region"))

  return(out)
}

load_new_capacity <- function(selected_ems = NULL, filedate = NULL) {
      #' Load ICU and non ICU capacity estimates
      #'
      #' The csv file is weekly updated and provided by CIVIS
      #' @param selected_ems if specified, the dataframe is filtered for that region
      #' @param filedate date of csv file to use, if not specified latest file is selected
      #'
  library(dplyr)
  capacity_dir <- file.path(data_path, "covid_IDPH/Corona virus reports/hospital_capacity_thresholds")

  if (is.null(filedate)) {
    files <- list.files(capacity_dir)[grep("capacity_weekday_average", list.files(capacity_dir))]
    files <- files[(nchar(files) == 37)]
    dates <- as.numeric(gsub(".csv", "", gsub("capacity_weekday_average_", "", files)))
    filedate <- max(dates)
  }

  fname <- paste0("capacity_weekday_average_", filedate, ".csv")
  df <- read.csv(file.path(capacity_dir, fname))
  colnames(df)[colnames(df) == 'avg_resource_available_prev2weeks'] <- 'avg_resource_available'

  df <- df %>%
    dplyr::filter(overflow_threshold_percent == 1) %>%
    dplyr::select(geography_modeled, resource_type, avg_resource_available) %>%
    unique() %>%
    pivot_wider(names_from = "resource_type", values_from = "avg_resource_available") %>%
    dplyr::mutate(geography_name = gsub("covidregion_", "", geography_modeled)) %>%
    dplyr::select(geography_name, icu_availforcovid, hb_availforcovid)

  dfIL <- df %>%
    dplyr::summarize(
      icu_availforcovid = sum(icu_availforcovid),
      hb_availforcovid = sum(hb_availforcovid)
    ) %>%
    dplyr::mutate(geography_name = "illinois") %>%
    dplyr::select(geography_name, icu_availforcovid, hb_availforcovid)


  df <- rbind(df, dfIL) %>%
    as.data.frame() %>%
    dplyr::rename(
      icu_available = icu_availforcovid,
      medsurg_available = hb_availforcovid
    )

  if (!(is.null(selected_ems))) df <- df %>% filter(geography_name %in% selected_ems)

  return(df)
}


f_getCustomTheme <- function(fontscl = 1) {

  customTheme <- theme(
    strip.text.x = element_text(size = 16 * fontscl, face = "bold"),
    strip.text.y = element_text(size = 16 * fontscl, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(size = 20 * fontscl, vjust = -1, hjust = 0),
    plot.subtitle = element_text(size = 18 * fontscl),
    plot.caption = element_text(size = 14 * fontscl),
    legend.title = element_text(size = 16 * fontscl),
    legend.text = element_text(size = 16 * fontscl),
    axis.title.x = element_text(size = 18 * fontscl),
    axis.text.x = element_text(size = 16 * fontscl),
    axis.title.y = element_text(size = 18 * fontscl),
    axis.text.y = element_text(size = 16 * +fontscl),
    axis.ticks = element_line(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

  return(customTheme)
}

f_get_scenVars <- function(dat) {
  dat$scen_name <- dat$exp_name
  dat$scen_name <- gsub("_triggeredrollback_reopen", "", dat$scen_name)
  dat$scen_name <- gsub("_reopen", "", dat$scen_name)
  dat <- dat %>% separate(scen_name, into = c("reopen", "delay", "rollback"), sep = "_")
  dat$rollback[is.na(dat$rollback)] <- "counterfactual"

  dat$reopen_fct <- factor(dat$reopen,
                           levels = c("100perc", "50perc"),
                           labels = c("High\ntransmission\nincrease",
                                      "Low\ntransmission\nincrese"))

  dat$reopen_fct2 <- factor(dat$reopen,
                            levels = c("100perc", "50perc"),
                            labels = c("High", "Low"))

  dat$rollback_fct <- factor(dat$rollback,
                             levels = c("pr8", "pr6", "pr4", "pr2"),
                             labels = rev(seq(20, 80, 20)))

  dat$capacity_multiplier_fct <- round(dat$capacity_multiplier * 100, 0)
  fct_labels <- sort(unique(dat$capacity_multiplier_fct))
  dat$capacity_multiplier_fct[dat$rollback == "counterfactual"] <- "counterfactual"
  dat$capacity_multiplier_fct <- factor(dat$capacity_multiplier_fct,
                                        levels = c(fct_labels, "counterfactual"),
                                        labels = c(fct_labels, "counterfactual")
  )
  dat$capacity_multiplier_fct2 <- factor(dat$capacity_multiplier_fct,
                                         levels = c(fct_labels, "counterfactual"),
                                         labels = c(fct_labels, "counter\nfactual")
  )

  return(dat)
}


f_load_trajectories <- function(exp_name, sim_dir, fname, region_nr = 11, trace_selection = FALSE) {
  dat <- fread(file.path(sim_dir, exp_name, fname))
  colnames(dat) <- gsub(paste0('_EMS-', region_nr), '', colnames(dat))
  if (!("capacity_multiplier" %in% colnames(dat)) & length(grep("counterfactual", exp_name)) > 0) dat$capacity_multiplier <- 0

  dat <- dat %>%
    dplyr::select(time, startdate, scen_num, sample_num, capacity_multiplier,
                  infected_cumul, crit_det_cumul, hosp_det_cumul, death_det_cumul, hosp_det, Ki_t, crit_det) %>%
    dplyr::mutate(exp_name = exp_name,
                  startdate = as.Date("2020-01-01"),
                  date = startdate + time) %>%
    dplyr::select(-time, -startdate) %>%
    unique() %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::group_by(exp_name, capacity_multiplier, scen_num, sample_num) %>%
    arrange(date) %>%
    dplyr::mutate(new_infected = infected_cumul - lag(infected_cumul),
                  new_crit_det = crit_det_cumul - lag(crit_det_cumul),
                  new_hosp_det = hosp_det_cumul - lag(hosp_det_cumul),
                  new_death_det = death_det_cumul - lag(death_det_cumul)) %>%
    dplyr::group_by(scen_num, sample_num, exp_name) %>%
    dplyr::mutate(group_id = cur_group_id()) %>%
    f_get_scenVars() %>%
    unique()

  if (trace_selection) {
    traces <- fread(file.path(sim_dir, "sample_num_traces_all.csv"))
    dat <- dat %>% filter(sample_num %in% traces$traces)
  }
  return(dat)
}

f_get_triggerDat <- function(dat) {
  dat_trigger <- dat %>%
    ungroup() %>%
    filter(date >= as.Date("2020-09-30") & date <= as.Date(last_plot_date)) %>%
    dplyr::group_by(exp_name, group_id) %>%
    dplyr::mutate(Ki_t_min = min(Ki_t), Ki_t_max = max(Ki_t),
                  delay_days = as.numeric(gsub("daysdelay", "", delay))) %>%
    filter(Ki_t_min != Ki_t_max) %>%
    filter(Ki_t == Ki_t_min) %>%
    filter(date == min(date)) %>%
    dplyr::mutate(triggerDate = as.Date(date) - delay_days) %>%
    dplyr::select(exp_name, group_id, triggerDate)

  return(dat_trigger)
}


f_N_triggered_scen <- function(dat) {

  dat <- dat %>%
    mutate(trigger_activated = ifelse(!is.na(triggerDate), 1, 0)) %>%
    ungroup() %>%
    dplyr::select(exp_name, group_id, capacity_multiplier, trigger_activated) %>%
    unique() %>%
    group_by(trigger_activated, capacity_multiplier) %>%
    tally() %>%
    pivot_wider(names_from = trigger_activated, values_from = n) %>%
    arrange(capacity_multiplier)

  if ('0' %in% colnames(dat) & !('1' %in% colnames(dat))) {
    dat <- dat %>%
      rename(N_no_trigger = `0`) %>%
      mutate(N_trigger = 0, perc_trigger = 0)
  }
  if ('1' %in% colnames(dat) & !('0' %in% colnames(dat))) {
    dat <- dat %>%
      rename(N_trigger = `1`) %>%
      mutate(N_no_trigger = 0, perc_trigger = 1)
  }
  if ('1' %in% colnames(dat) & '1' %in% colnames(dat)) {
    dat <- dat %>%
      rename(N_no_trigger = `0`, N_trigger = `1`) %>%
      mutate(perc_trigger = N_trigger / (N_trigger + N_no_trigger))
  }

  return(dat)
}

f_get_peakDat <- function(dat,
                          groupVARS = c("group_id"),
                          lower_date = as.Date("2020-10-01"),
                          keepVARS = c()) {

  if (sum(length(keepVARS)) == 0) {
    keepVARS <- c(groupVARS, "date_peak", "crit_det_peak")
  }
  dat <- dat %>%
    filter(date >= lower_date) %>%
    dplyr::group_by_at(.vars = groupVARS) %>%
    dplyr::filter(crit_det == max(crit_det)) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::mutate(crit_det_peak = crit_det,
                  date_peak = date) %>%
    dplyr::select_at(all_of(keepVARS))

  return(dat)
}


f_n_scenarios <- function(dat) {
  if (!("trigger_activated" %in% colnames(dat))) {
    dat$trigger_activated = NA
  }
  scens <- dat %>%
    dplyr::group_by(group_id, exp_name, capacity_multiplier, reopen, rollback, delay) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(group_id, scen_num, exp_name, capacity_multiplier, reopen, trigger_activated, rollback, delay) %>%
    dplyr::group_by(capacity_multiplier, reopen, rollback, delay) %>%
    dplyr::add_tally() %>%
    dplyr::group_by(capacity_multiplier, n, reopen, rollback, delay) %>%
    dplyr::summarize(n_trigger_activated = sum(trigger_activated)) %>%
    dplyr::rename(n_scenarios = n) %>%
    as.data.frame() %>%
    arrange(capacity_multiplier, rollback)

  return(scens)
}

f_prob_threshold <- function(dat, keep_reopen = FALSE) {

  if (keep_reopen) {
    scenarioVARS <- c('rollback', 'delay', 'reopen')
  }else {
    scenarioVARS <- c('rollback', 'delay')
  }

  groupVARS = c('group_id', 'exp_name', 'capacity_multiplier', scenarioVARS)
  groupVARS_tally = c('capacity_multiplier', scenarioVARS)
  groupVARS_sum = c('n', groupVARS_tally)
  keepVARS <- c(groupVARS, 'scen_num', 'trigger_activated')

  probs <- dat %>%
    dplyr::group_by_at(.vars = groupVARS) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select_at(all_of(keepVARS)) %>%
    dplyr::group_by_at(.vars = groupVARS_tally) %>%
    dplyr::add_tally() %>%
    dplyr::group_by_at(.vars = groupVARS_sum) %>%
    dplyr::summarize(n_trigger_activated = sum(trigger_activated)) %>%
    dplyr::rename(n_scenarios = n) %>%
    dplyr::mutate(n_trigger_not_activated = n_scenarios - n_trigger_activated,
                  perc_trigger = n_trigger_activated / n_scenarios) %>%
    as.data.frame() %>%
    arrange(capacity_multiplier, rollback)

  return(probs)
}


f_prob_capacity <- function(dat, keep_reopen = FALSE) {

  if (keep_reopen) {
    scenarioVARS <- c('rollback', 'delay', 'reopen')
  }else {
    scenarioVARS <- c('rollback', 'delay')
  }

  groupVARS = c('group_id', 'exp_name', 'capacity_multiplier', scenarioVARS)
  groupVARS_tally = c('capacity_multiplier', scenarioVARS)
  groupVARS_sum = c('n', groupVARS_tally)
  keepVARS <- c(groupVARS, 'scen_num', 'crit_det_peak')

  probs <- dat %>%
    dplyr::group_by_at(.vars = groupVARS) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select_at(all_of(keepVARS)) %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = groupVARS_tally) %>%
    dplyr::add_tally() %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = groupVARS_sum) %>%
    mutate(above_yn = ifelse(crit_det_peak >= 516, 1, 0)) %>%
    dplyr::summarize(above_y = sum(above_yn)) %>%
    dplyr::rename(n_scenarios = n) %>%
    dplyr::mutate(n_not_above = n_scenarios - above_y,
                  perc_above = above_y / n_scenarios) %>%
    as.data.frame() %>%
    arrange(capacity_multiplier, rollback)

  return(probs)
}


f_prob_capacity_wt <- function(dat, keep_reopen = FALSE) {

  if (keep_reopen) {
    scenarioVARS <- c('rollback', 'delay', 'reopen')
  }else {
    scenarioVARS <- c('rollback', 'delay')
  }

  groupVARS = c('group_id', 'exp_name', 'capacity_multiplier', scenarioVARS)
  groupVARS_tally = c('capacity_multiplier', scenarioVARS)
  groupVARS_sum = c('n', groupVARS_tally)
  keepVARS <- c(groupVARS, 'scen_num', 'crit_det_peak', 'll_wt')

  probs <- dat %>%
    dplyr::group_by_at(.vars = groupVARS) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select_at(all_of(keepVARS)) %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = groupVARS_tally) %>%
    dplyr::add_tally() %>%
    ungroup() %>%
    dplyr::group_by_at(.vars = groupVARS_sum) %>%
    mutate(above_yn = ifelse(crit_det_peak >= 516, 1, 0),
           above_yn_wt = above_yn * ll_wt) %>%
    dplyr::summarize(above_y = sum(above_yn),
                     above_y_wt = sum(above_yn_wt)) %>%
    dplyr::rename(n_scenarios = n) %>%
    dplyr::mutate(
      perc_above = above_y / n_scenarios,
      perc_above_wt = above_y_wt / n_scenarios) %>%
    as.data.frame() %>%
    arrange(capacity_multiplier, rollback)

  return(probs)
}




f_add_popvars <- function(dat) {
  ### add seconday variables
  dat$pop = 2456274
  dat$capacity = 516
  dat$rel_occupancy = dat$crit_det / dat$capacity
  dat$capacity_per10000 = (dat$capacity / dat$pop) * 10000
  dat$crit_det_per10000 = (dat$crit_det / dat$pop) * 10000
  dat$new_infected_per10000 = (dat$new_infected / dat$pop) * 10000
  return(dat)
}

#""" Wrapper function to load trajectories """
f_load_sim_data <- function(exp_name, fname, sim_dir, add_peak_cols = TRUE, add_trigger_cols = TRUE, addRt = TRUE, trace_selection = FALSE) {

  #"""Load trajectories """
  dat <- f_load_trajectories(exp_name, sim_dir, fname, trace_selection = trace_selection)

  #"""Add peak dates """
  if (add_peak_cols) {
    dat <- dat %>%
      left_join(f_get_peakDat(dat))
  }

  if (add_trigger_cols) {
    #"""Add trigger dates """
    dat <- dat %>%
      left_join(f_get_triggerDat(dat))

    #"""Calculate N triggered per capacity_multiplier """
    dat <- dat %>%
      left_join(f_N_triggered_scen(dat)) %>%
      mutate(trigger_activated = ifelse(!is.na(triggerDate), 1, 0))
  }

  #"""Add Rt """
  if (addRt) {
    #rtdat = fread(file.path(sim_dir,'Rt_chicago_combined.csv'))
    rtdat = fread(file.path(sim_dir, exp_name, 'rt_trajectoriescovidregion_11.csv')) %>%
      select(date, scen_num, rt_lower, rt_median, rt_upper)
    rtdat$date <- as.Date(rtdat$date)
    ### add 7 day lag
    #rtdat$date = rtdat$date + 7

    rtdat$date = as.character(rtdat$date)
    dat$date = as.character(dat$date)
    dat <- dat %>%
      left_join(rtdat) %>%
      mutate(date = as.Date(date))
  }


  return(dat)
}


f_sample_trajectories <- function(dat, groupVars = c('rollback', 'delay', 'capacity_multiplier'),weighted=FALSE) {

  dat_sub_50_list <- list()
  for (i in c(1:100)) {

    nsubsample_50 <- dat %>%
      ungroup() %>%
      dplyr::group_by_at(groupVars) %>%
      sample_n(50, replace = TRUE) %>%
      dplyr::mutate(
        nsamples_sub = n_distinct(sample_num),
        scen_num_sel = scen_num
      ) %>%
      dplyr::select(all_of(groupVars), 'scen_num', 'scen_num_sel', 'nsamples_sub') %>%
      unique()

    dat_subsample_50 <- dat %>%
      ungroup() %>%
      dplyr::filter(crit_det >= 516 * capacity_multiplier) %>%
      dplyr::select(-sample_num) %>%
      dplyr::left_join(nsubsample_50, by = c(groupVars, "scen_num")) %>%
      dplyr::filter(scen_num == scen_num_sel)

    if(weighted){
      dat_sub_50_list[[length(dat_sub_50_list) + 1]] <- f_prob_capacity_wt(dat_subsample_50, keep_reopen = TRUE) %>% mutate(resample_n = i)
    }else{
      dat_sub_50_list[[length(dat_sub_50_list) + 1]] <- f_prob_capacity(dat_subsample_50, keep_reopen = TRUE) %>% mutate(resample_n = i)
    }
    rm(dat_subsample_50)
  }
  dat_prob <- dat_sub_50_list %>%
    bind_rows()

  return(dat_prob)

}


f_prob_plot_base <- function(dat) {

  pplot <- ggplot(data = dat) +
    geom_vline(xintercept = 50, col = 'grey', alpha = 0.8) +
    geom_hline(yintercept = 0.5, col = 'grey', alpha = 0.8) +
    geom_ribbon(aes(
      x = capacity_multiplier * 100,
      y = prob,
      ymin = prob_lower,
      ymax = prob_upper,
      fill = rollback,
      group = interaction(rollback)
    ), alpha = 0.3) +
    geom_line(aes(
      x = capacity_multiplier * 100, y = prob,
      col = rollback,
      group = interaction(rollback)
    ), size = 1.3) +
    scale_y_continuous(
      lim = c(0, 1.01),
      breaks = seq(0, 1, 0.2),
      labels = seq(0, 1, 0.2) * 100,
      minor_breaks = seq(0, 1, 0.1)
    ) +
    scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), lim = c(0, 100)) +
    scale_color_manual(values = mitigation_cols) +
    scale_fill_manual(values = mitigation_cols) +
    customTheme +
    theme(
      panel.spacing = unit(1, "lines"),
      legend.position = "right",
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(size = 0.5)
    ) +
    labs(
      y = "Probability of ICU overflow (%)",
      x = "\nTrigger threshold\n(% of available ICU beds)",
      caption = '\nby any date',
      color = "Mitigation\nstrengths", fill = "Mitigation\nstrengths"
    )

  return(pplot)
}