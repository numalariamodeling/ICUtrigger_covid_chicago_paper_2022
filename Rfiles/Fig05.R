# Title     : COVID-19 Chicago: ICU thresholds for action to prevent overflow
# Objective : Figure 5 combined

source(file.path('settings.R'))
source(file.path('helper_functions.R'))
customTheme <- f_getCustomTheme()
delay = "1daysdelay"

p5Adat <- fread(file.path(fig_dir,"csv","p5Adat.csv"))
p5Bdat <- fread(file.path(fig_dir,"csv","p5Bdat.csv"))
p5C_dat <- fread(file.path(fig_dir,"csv",paste0("p5Cdat_",delay,".csv")))

p5C_dat$rollback <- factor(p5C_dat$rollback,
                           levels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"),
                           labels=c("weak (20%)","moderate (40%)","strong (60%)","very strong (80%)"))

p5A <- p5Adat %>%
  ggplot() +
  geom_line(aes(x=time_since_trigger, y=rel_occupancy,
                group=interaction( scen_num,group_id, exp_name, capacity_multiplier),
                col=as.factor(reopen)),
            alpha=0.5,size=0.5) +
  scale_color_manual(values=c('deepskyblue4','deepskyblue')) +
  scale_fill_manual(values=c('deepskyblue4','deepskyblue')) +
  geom_hline(yintercept=1,col=capacitycolor,linetype='dashed')+
  geom_vline(xintercept=0)+
  theme_minimal()+customTheme+
  labs(x="Time since trigger (weeks)",y="Fraction ICU beds occupied",color="")+
  scale_x_continuous(breaks=seq(-28,90,14),labels=round(seq(-28,90,14)/7,0),lim=c(-28,90))+
  facet_wrap(~capacity_multiplier, nrow=1)

p5B <- f_prob_plot_base(dat = subset(p5Bdat, rollback!="counterfactual")) +
  facet_grid(delay ~ reopen)

p5C <- ggplot(data=subset(p5C_dat, delay=="1daysdelay"))+
  geom_ribbon( aes(x=capacity_multiplier,ymin=q5,ymax=q95,fill=rollback),alpha=0.3) +
  geom_line( aes(x=capacity_multiplier,y=median_val,col=rollback)) +
  facet_wrap(~reopen,scales='free_y', ncol=1) +
  scale_y_continuous(lim=c(0,4), breaks=seq(0,4,0.5), labels=seq(0,4,0.5)) +
  scale_x_continuous(breaks=seq(0,1,0.2), labels=seq(0,100,20)) +
  geom_hline(yintercept=1,col=capacitycolor,linetype='dashed')+
  scale_fill_manual(values=mitigation_colors)+
  scale_color_manual(values=mitigation_colors)+
  labs(title="",
       subtitle="immediate mitigation:\n1 day after threshold reached",
       x='ICU occupancy threshold\nto trigger mitigation',
       colos='Mitigation\nstrengths',
       y='peak ICU occupancy as fraction of ICU capacity')+
  customTheme

p5BC <- plot_grid(p5B, p5C, nrow=1, labels=c("B","C"))
pplot <- plot_grid(p5A, p5BC, nrow=2, rel_heights=c(0.4,1),labels=c("A","",""))

f_save_plot(
  plot_name = paste0("Fig5"), pplot = pplot,
  plot_dir = file.path(fig_dir), width =14, height = 8
)

### For text
p5Bdat %>%
  filter(delay=="immediate mitigation:\n1 day after threshold reached" &
           rollback=="moderate (40%)") %>%
  group_by(reopen,delay, rollback) %>%
  summarize(mean=mean(prob),
            prob_lower=mean(prob_lower),
            prob_upper=mean(prob_upper))


p5Bdat %>%
  filter(reopen=="High increase in transmission:\nRt approx. 1.25" &
           rollback=="moderate (40%)") %>%
  group_by(reopen,delay, rollback) %>%
  summarize(mean=mean(prob),
            prob_lower=mean(prob_lower),
            prob_upper=mean(prob_upper))

p5Bdat %>%
  filter(reopen!="High increase in transmission:\nRt approx. 1.25" &
           rollback=="very strong (80%)" &  capacity_multiplier==0.8)

p5Bdat %>%
  filter(reopen=="High level of\ntransmission increase (Rt 1.25)" &
           rollback=="very strong (80%)" & capacity_multiplier==0.6)
