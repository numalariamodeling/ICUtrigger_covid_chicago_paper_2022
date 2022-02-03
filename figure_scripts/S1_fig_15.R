# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : S1 Figure 15

source(file.path('setup/settings.R'))
source(file.path('setup/helper_functions.R'))
customTheme <- f_getCustomTheme()
trace_selection <- TRUE
if (trace_selection) fig_dir = fig_dir_traces

pal = colorRampPalette(brewer.pal(8, "Spectral"))(100)

### Load saved probability dataframe from Fig 5B
p5Bdat <- fread(file.path(fig_dir, "csv", "p5Bdat.csv"))

### Define regression model per group
dfGLM <- p5Bdat %>%
  dplyr::rename(
    x = capacity_multiplier,
    y = prob,
  ) %>%
  dplyr::group_by(reopen, delay, rollback) %>%
  do(fitglm = glm(y ~ x, family = binomial(link = logit), data = .))

### Make predictions
pred_list <- list()
for (i in c(1:nrow(dfGLM))) {
  pred_dat <- data.frame('x' = seq(0, 1, 0.01), 'y_pred' = NA)
  glmmodel <- dfGLM[i, 'fitglm'][[1]]
  pred_dat$y_pred <- predict(glmmodel[[1]], newdata = pred_dat, type = "response")
  pred_dat$reopen <- dfGLM[i, 'reopen'][[1]]
  pred_dat$delay <- dfGLM[i, 'delay'][[1]]
  pred_dat$rollback <- dfGLM[i, 'rollback'][[1]]
  pred_list[[length(pred_list) + 1]] <- pred_dat
}

### Combine data-list
pred_dat <- pred_list %>%
  bind_rows() %>%
  dplyr::rename(capacity_multiplier = x, prob = y_pred) %>%
  dplyr::mutate(rollback_num = readr::parse_number(rollback))

### Re-define factor labels
pred_dat$rollback <- factor(pred_dat$rollback,
                            levels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"),
                            labels = c("weak\n(20%)", "moderate\n(40%)", "strong\n(60%)", "very strong\n(80%)"))

pred_dat$delay <- factor(pred_dat$delay,
                         levels = c("immediate mitigation:\n1 day after threshold reached",
                                    "delayed mitigation:\n7 days after threshold reached"),
                         labels = c("immediate mitigation:\n1 day after threshold reached",
                                    "delayed mitigation:\n7 days after threshold reached"))

pred_dat$reopen <- factor(pred_dat$reopen,
                          levels = c("High increase in transmission:\nRt approx. 1.28",
                                     "Low increase in transmission:\nRt approx. 1.16"),
                          labels = c("High increase in transmission:\nRt approx. 1.28",
                                     "Low increase in transmission:\nRt approx. 1.16"))

p5Bdat$rollback <- factor(p5Bdat$rollback,
                          levels = c("weak (20%)", "moderate (40%)", "strong (60%)", "very strong (80%)"),
                          labels = c("weak\n(20%)", "moderate\n(40%)", "strong\n(60%)", "very strong\n(80%)"))

p5Bdat$delay <- factor(p5Bdat$delay,
                       levels = c("immediate mitigation:\n1 day after threshold reached",
                                  "delayed mitigation:\n7 days after threshold reached"),
                       labels = c("immediate mitigation:\n1 day after threshold reached",
                                  "delayed mitigation:\n7 days after threshold reached"))

p5Bdat$reopen <- factor(p5Bdat$reopen,
                        levels = c("High increase in transmission:\nRt approx. 1.28",
                                   "Low increase in transmission:\nRt approx. 1.16"),
                        labels = c("High increase in transmission:\nRt approx. 1.28",
                                   "Low increase in transmission:\nRt approx. 1.16"))

pplot <- ggplot(data = pred_dat) +
  geom_tile(aes(x = capacity_multiplier, y = rollback, fill = prob), col = NA) +
  facet_grid(reopen ~ delay) +
  geom_vline(xintercept = c(0.25, 0.5, 0.75), col = 'black', size = 0.2) +
  geom_hline(yintercept = c(1.5, 2.5, 3.5), col = 'white', size = 0.2) +
  scale_fill_gradientn(colours = rev(pal), limits = c(0, 1),
                       breaks = seq(0, 1, 0.25),
                       labels = seq(0, 1, 0.25) * 100) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels = seq(0, 1, 0.1) * 100,
                     expand = c(0, 0)) +
  labs(y = "Mitigation strength",
       x = "ICU occupancy relative to capacity",
       fill = "Probability of\nICU overflow (%)") +
  theme(panel.spacing = unit(1.2, "lines")) +
  customTheme

f_save_plot(
  plot_name = paste0("S1_fig_15"), pplot = pplot,
  plot_dir = file.path(fig_dir), width = 12, height = 8
)

fwrite(pred_dat, file.path(fig_dir, "csv", "S1_fig_15.csv"))
if (cleanEnv)rm(list = ls())