## +++++++++++++++++++++++++++++++
# This scripts for developmental project
# color code slategray4 for p7-9 #F8766D for p14-16 and #00BFC4 for p21-23) (ggplot_build()$data)
# Created on 031215
## +++++++++++++++++++++++++++++++

## load library and function-------
library(IgorR)
library("openxlsx")
library("plyr")
library("dplyr")
library("ggplot2")
library("cowplot")
library("reshape2")
library("pracma")
library(CCfun)

## 1. analyze and plot of each data with 10mM EGTA---------------
dat <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/10mM\ EGTA\ detail.xlsx",sheet= 1,colNames = TRUE)
dat$Amplitude<- abs(dat$Amplitude)
dat$ID <- as.factor(dat$ID)
dat$Time <- dat$Trace*5

# plot the amplitude and Rs for each trace


# plot each data in group p7-9
ID_name<- levels(subset(dat, dat$Group=="P7-9")$ID)
# for ( n in ID_name) print(trace_plot(n))
# For data 1503161AB
p_amp_1503161AB <- trace_plot("1503161AB")
# for data 1503163AA
p_amp_1503163AA <- trace_plot("1503163AA")
# for data 1503166AA
p_amp_1503166AA <- trace_plot("1503166AA")
# for data 1712151AF
p_amp_1712151AF<- trace_plot("1712151AF")
# for data 0704165AA
p_amp_0704165AA<- trace_plot("0704165AA")
# for data 0704166AA
p_amp_0704166AA<- trace_plot("0704166AA")
# for data 0704167AA
p_amp_0704167AA<- trace_plot("0704167AA")

## 1503161AB For showing the effect of 10mEGTA
dat1<- subset(dat, dat$ID=="1503161AB")

p7_amp <- ggplot(dat1, aes(Time, Amplitude))+
  geom_point(colour="slategray4",shape=1)+
  labs(x="", y="Peak amplitude (pA)")+
  theme(axis.line = element_line(colour = "black"),
        # axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())

p7_rs<- ggplot(dat1, aes(x=Time,y=Rs)) +
  geom_line(colour="black")+
  labs(x ="Time (s)", y="Rs (MΩ)")+
  theme(axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())+
  scale_y_continuous(limits=c(0,8), expand = c(0,0),breaks=c(0,4,8))

p_trace_p7<- plot_grid(p7_amp,p7_rs, ncol=1, nrow=2,align = "v",rel_heights = c(3,1.2))

## for data from P21-23 group
# for data 1804161AA
p_amp_1804161AA<- trace_plot("1804161AA")
# for data 2204161AB
p_amp_2204161AB<- trace_plot("2204161AB")
# for data 2204162AA
p_amp_2204162AA<- trace_plot("2204162AA")
# for data 2204163AB
p_amp_2204163AB <- trace_plot("2204163AB")
dat2<- subset(dat, dat$ID=="2204162AA")
p21_amp <- ggplot(dat2, aes(Time, Amplitude))+
  geom_point(shape=1, colour="cyan3")+
  labs(x="", y="Peak amplitude (pA)")+
  theme(axis.line = element_line(colour = "black"),
        # axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())

p21_rs<- ggplot(dat2, aes(x=Time,y=Rs)) +
  geom_line()+
  labs(x ="Time (s)", y="Rs (MΩ)")+
  theme(axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())+
  scale_y_continuous(limits=c(0,8), expand = c(0,0),breaks=c(0,4,8))

p_trace_p21<- plot_grid(p21_amp,p21_rs, ncol=1, nrow=2,align = "v",rel_heights = c(3,1.2))

## for P14-16
dat_14 <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/10mM\ EGTA\ detail.xlsx",sheet= 2,colNames = TRUE)
p14_amp <- ggplot(dat_14, aes(Time, -Amp))+
  geom_point(shape=1, colour="salmon")+
  labs(x="", y="Peak amplitude (pA)")+
  theme(axis.line = element_line(colour = "black"),
        # axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())

p14_rs<- ggplot(dat_14, aes(x=Time,y=Rs)) +
  geom_line()+
  labs(x ="Time (s)", y="Rs (MΩ)")+
  theme(axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())+
  scale_y_continuous(limits=c(0,8), expand = c(0,0),breaks=c(0,4,8))

p_trace_p14<- plot_grid(p14_amp,p14_rs, nrow=2,align = "v",rel_heights = c(3,1.2))

## combine three groups
EGTA_trace <- plot_grid(p_trace_p7, p_trace_p14, p_trace_p21, nrow = 3)
## plot 10 mM EGTA trace
EGTA_v<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/Untitled.ibw_In1V.ibw",Verbose = TRUE,ReturnTimeSeries = TRUE)

EGTA_i<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/Untitled.ibw_In2I.ibw",Verbose = TRUE, ReturnTimeSeries = TRUE);

EGTA_first<- rowMeans(EGTA_i[,1:10])
EGTA_last<- rowMeans(EGTA_i[,190:200])
par(mfrow=c(1,2))
plot(EGTA_first[10000:10500], type = "l",ylim = c(-4700, -3600),xaxt='n',yaxt="n",bty="n", box=FALSE)
plot(EGTA_last[10000:10500], type = "l",col="blue", ylim = c(-4600,-3700),xaxt='n',yaxt="n",bty="n", box=FALSE)
par(mfrow=c(1,1))


## 2. analysis IPSC properties of mice -----------------
dat_compare <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data 060717.xlsx",sheet= "IPSC properties",colNames = TRUE)
# dat_compare<- subset(dat_compare, dat_compare$Intra.sol=="2C")
dat_compare<- dat_compare[,1:12]
dat_compare$Amplitude<- dat_compare$Amplitude/1000
dat_compare$Age <- factor(dat_compare$Age, c("P7-9","P14-16","P21-23"))

# reshape the data for analysis
dat_compare_re <- melt(dat_compare, id=c("ID", "Age"))
dat_compare_sta <- ddply(dat_compare_re,.(variable,Age),summarise,n=length(value),mean=mean(value),sd=sd(value),se=sd(value)/sqrt(length(value)))

# bar plot risetime,peak and so on
p_amplitude<- ggplot(subset(dat_compare_sta,variable=="Amplitude"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="Peak amplitude (nA)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,10), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Amplitude"),aes(Age, value),color="black",shape=1,width = 0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")

p_risetime <- ggplot(subset(dat_compare_sta,variable=="risetime"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="20-80% risetime (ms)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,1.5), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="risetime"),aes(Age, value),colour="black",shape=1,width=0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")+
  annotate(x=c(1,1,3,3), y=c(1.4,1.42,1.42,1.4),"path")+
  annotate(x=c(1,1,2,2), y=c(1.3,1.32,1.32,1.3),"path")+
  annotate("text",x=1.5,y=1.32, label="***", size=6)+
  annotate("text", x= 2, y=1.42, label="***", size=6)

p_halfduration <- ggplot(subset(dat_compare_sta,variable=="Half.duration"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="Half duration (ms)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,15), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Half.duration"),aes(Age, value),colour="black",shape=1,width = 0.25)+
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")
  

p_latency <- ggplot(subset(dat_compare_sta,variable=="Latency"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity",colour='black')+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="Latency (ms)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,1.5), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Latency"),aes(Age, value),colour="black",shape=1,width = 0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")+
  annotate(x=c(1,1,3,3), y=c(1.38,1.40,1.40,1.38),"path")+
  annotate(x=c(1,1,2,2), y=c(1.22,1.24,1.24,1.22),"path")+
  # annotate(x=c(2,2,3,3), y=c(0.6,0.62,0.62,0.6),"path")+
  annotate("text",x=1.5,y=1.26, label="***", size=6)+
  # annotate("text",x=2.5,y=0.65, label="n.s")+
  annotate("text", x= 2, y=1.42, label="***", size=6)

p_tau <- ggplot(subset(dat_compare_sta,variable=="Tau.decay"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="Tau decay (ms)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,15), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Tau.decay"),aes(Age, value),colour="black",shape=1,width = 0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")

p_latency_sd <- ggplot(subset(dat_compare_sta,variable=="Latency.sd"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="SD latency (ms)")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,0.8), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Latency.sd"),aes(Age, value,colour=Age),colour="black",shape=1,width = 0.25) +
  theme(legend.position="none")




p_amp_cv<- ggplot(subset(dat_compare_sta,variable=="Amp.cv"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="CV of amplitude")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,0.8), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Amp.cv"),aes(Age, value),color="black",shape=1,width = 0.25) +
  theme(legend.position="none")

p_latency_cv<- ggplot(subset(dat_compare_sta,variable=="Latency.cv"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="CV of latency")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,0.8), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Latency.cv"),aes(Age, value),color="black",shape=1,width = 0.25) +
  theme(legend.position="none")

p_risetime_cv<- ggplot(subset(dat_compare_sta,variable=="Risetime.cv"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="CV of risetime")+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5 ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,0.8), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Risetime.cv"),aes(Age, value),color="black",shape=1,width = 0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")

p_failure<- ggplot(subset(dat_compare_sta,variable=="Failure"), aes(x=Age, y=mean, fill=Age)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x="", y="Proportion of failure (%)")+
  theme(axis.line = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  # scale_y_continuous(limits=c(0,0.3), expand=c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=subset(dat_compare_re, variable=="Failure"),aes(Age, value),color="black",shape=1,width = 0.25) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")

p_prop <- plot_grid(p_amplitude, p_risetime, p_halfduration, p_latency, p_amp_cv, p_risetime_cv, ncol = 3)
plot_grid(p_amp_cv, p_risetime_cv)

## save the figure for plot
setwd("~cchen2/Documents/neuroscience/project/Figures/Figures/development/PDF")

cairo_pdf("IPSC_prop.pdf", width = 150/25.6, height = 170/25.6, family = "Arial")
p_prop
dev.off()

## statistical test
t_peak<- aov(Amplitude~Age, data = dat_compare)
summary(t_peak)
TukeyHSD(t_peak)
kruskal.test(Amplitude~Age, data = dat_compare)
pairwise.wilcox.test(dat_compare$Amplitude, dat_compare$Age)

t_risetime<- aov(risetime~Age, data = dat_compare)
summary(t_risetime)
TukeyHSD(t_risetime)
kruskal.test(risetime~Age, data = dat_compare)
pairwise.t.test(dat_compare$risetime, dat_compare$Age)

pairwise.t.test(dat_compare$Half.duration, dat_compare$Age)
pairwise.wilcox.test(dat_compare$Half.duration, dat_compare$Age)

t_latency <- aov(Latency~Age, data=dat_compare)
summary(t_latency)
TukeyHSD(t_latency)
kruskal.test(Latency~Age, dat_compare)
pairwise.t.test(dat_compare$Latency, dat_compare$Age)
pairwise.wilcox.test(dat_compare$Latency, dat_compare$Age)


t_sd_latency <- aov(Latency.sd~Age, dat_compare)
summary(t_sd_latency)
TukeyHSD(t_sd_latency)
kruskal.test(Latency.sd~Age, dat_compare)
pairwise.t.test(dat_compare$Latency.sd, dat_compare$Age)
pairwise.wilcox.test(dat_compare$Latency.sd, dat_compare$Age)

pairwise.t.test(dat_compare$Latency.cv, dat_compare$Age)
pairwise.wilcox.test(dat_compare$Latency.cv, dat_compare$Age)

pairwise.wilcox.test(dat_compare$Amp.cv, dat_compare$Age)
pairwise.t.test(dat_compare$Amp.cv, dat_compare$Age)

pairwise.wilcox.test(dat_compare$Risetime.cv, dat_compare$Age)
pairwise.t.test(dat_compare$Risetime.cv, dat_compare$Age)


pairwise.wilcox.test(dat_compare$Failure, dat_compare$Age)
pairwise.t.test(dat_compare$Failure, dat_compare$Age)

## 3. plot to show single AP trace--------------

# import data for p7-9
p7_voltage<- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1011162AD.ibw_In1V.ibw",Verbose = TRUE,ReturnTimeSeries = F)
p7_current <- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1011162AD.ibw_In2I.ibw",Verbose = TRUE,ReturnTimeSeries = F)


q <- 5
p7_current1<- cc_decimate(p7_current,q)
p7_current_mean <- rowMeans(p7_current[,1:20])
# plot data

p7_vol_plot<- qplot(seq_along(p7_voltage[9900:11000,1]), p7_voltage[9900:11000,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-50,0) )

p7_current_plot<- as.data.frame(p7_current1[(9900:11000)/q,1:20])
p7_current_plot$ID <- 1:nrow(p7_current_plot)
p7_current_plot1<- melt(p7_current_plot, id.vars = "ID")
p7_mean_current<- data.frame(ID=p7_current_plot$ID,mean=p7_current_mean[9900:11000])
p7_cur_plot<- ggplot()+ 
  geom_line(data = p7_current_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p7_mean_current, aes(x=ID, y=mean), colour="slategray4")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(900,1000,1000,1000),y=c(-1500,-1500,-1500,-1000))

p7_trace<- plot_grid(p7_vol_plot, p7_cur_plot,nrow = 2, rel_heights = c(0.3,1))      

# import data for p14-16
p14_voltage<- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/2805152AC.ibw_In1V.ibw",Verbose = TRUE,ReturnTimeSeries = TRUE)
p14_current <- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/2805152AC.ibw_In2I.ibw",Verbose = TRUE,ReturnTimeSeries = F)
p14_current1<- cc_decimate(p14_current,q)
p14_current_mean <- rowMeans(p14_current[,c(1:22)[-c(4,9)]])
# plot data

p14_vol_plot<- qplot(seq_along(p14_voltage[9900:11000,1]), p14_voltage[9900:11000,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-70,-20) )

p14_current_plot<- as.data.frame(p14_current1[(9900:11000)/q,1:20])
p14_current_plot$ID <- 1:nrow(p14_current_plot)
p14_current_plot1<- melt(p14_current_plot, id.vars = "ID")
p14_mean_current<- data.frame(ID=p14_current_plot$ID,mean=p14_current_mean[9900:11000])
p14_cur_plot<- ggplot()+ 
  geom_line(data = p14_current_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p14_mean_current, aes(x=ID, y=mean), colour="#F8766D")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(900,1000,1000,1000),y=c(-2000,-2000,-2000,-1500))

p14_trace<- plot_grid(p14_vol_plot, p14_cur_plot,nrow = 2, rel_heights = c(0.3,1))      


# import data for p21-23
p21_voltage<- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1804161AA.ibw_In1V.ibw",Verbose = TRUE,ReturnTimeSeries = TRUE)
p21_current <- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1804161AA.ibw_In2I.ibw",Verbose = TRUE,ReturnTimeSeries = F)

p21_current1<- cc_decimate(p21_current, q)
p21_current_mean <- rowMeans(p21_current[,3:23])
# plot data

p21_vol_plot<- qplot(seq_along(p21_voltage[9900:11000,1]), p21_voltage[9900:11000,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-60,-10) )

p21_current_plot<- as.data.frame(p21_current1[(9900:11000)/q,3:23])
p21_current_plot$ID <- 1:nrow(p21_current_plot)
p21_current_plot1<- melt(p21_current_plot, id.vars = "ID")
p21_mean_current<- data.frame(ID=p21_current_plot$ID,mean=p21_current_mean[9900:11000])
p21_cur_plot<- ggplot()+ 
  geom_line(data = p21_current_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p21_mean_current, aes(x=ID, y=mean), colour="#00BFC4")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(900,1000,1000,1000),y=c(-1100,-1100,-1100,-600))

p21_trace<- plot_grid(p21_vol_plot, p21_cur_plot,nrow = 2, rel_heights = c(0.3,1))      

x<- plot_grid(p7_trace, p14_trace, p21_trace, nrow = 1)
y<- plot_grid(p_amplitude, p_risetime, p_halfduration, p_latency, p_latency_sd, p_tau, nrow = 1, labels = c("B","C","D","E","F","G"))
plot_grid(x,y, nrow = 2,labels = c("A",""),label_size = 16)+
  theme(plot.margin = unit(c(1.5,1,1,1), "cm")) 
## analysis of the releation between kinetic properties
p_risetime_amp <- ggplot(dat_compare, aes(risetime, Amplitude, colour=Age))+
  geom_point()

p_risetime_duration <- ggplot(dat_compare, aes(risetime,Half.duration))+
  geom_point()+
  facet_grid(Age~.)+ 
  geom_smooth(method = "lm",se=F, aes(colour=Age))



## 4. plot normalized data with 10mM EGTA------------------
dat <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= 1,colNames = TRUE)

dat<- dat[,c(2,3,5)]

dat<- subset(dat, dat$Trace<21)
dat$Group<- factor(dat$Group, c("P7-9", "P14-16","P21-23"))

sum_dat_amp<- ddply(dat,.(Group, Trace),summarise,n=length(Nor.amp),mean=mean(Nor.amp),sd=sd(Nor.amp),se=sd(Nor.amp)/sqrt(length(Nor.amp)))


dat_norm <- ggplot(sum_dat_amp, aes(Trace, mean, colour=Group))+
  geom_errorbar(aes(ymin=mean-se-se, ymax=mean+se), width=.2) +
  geom_point( aes(shape=Group),size=2)+
  geom_line()+
  scale_color_manual(values = c("slategray4","salmon","cyan3"))+
  # geom_smooth(method = "lm", formula= y~ log(x), se=F)+
  xlab("Time (s)")+ylab("Normalized amplitude") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  theme(legend.key = element_blank())+
  scale_y_continuous(limits=c(0,2),expand = c(0,0)) +
  scale_x_continuous(labels=c(seq(0,1000,250)))+
  geom_hline(yintercept = 1, linetype=2)+
  theme(legend.title=element_blank())
dat_norm

## calculate the depression
dat_depression<- subset(dat, dat$Trace=="20")
dat_depression_sta <- subset(sum_dat_amp, sum_dat_amp$Trace=="20")

p_10EGTA_depression<- ggplot(dat_depression_sta, aes(Group, mean, fill=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(position=position_dodge(), stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon","cyan3"))+
  labs(x="", y="Normalized reduction")+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits = c(0,2),expand = c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23"))+
  geom_jitter(data=dat_depression,aes(Group,Nor.amp),width=0.25, colour="black", shape=1) +
  geom_hline(yintercept = 1, linetype=2)+
  theme(legend.position="none")



## 5. plot the firing pattern of neuron in p7-9 P21-23 and p14-16 mice---------
ap_14<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1304152AA.ibw_In1V.ibw",Verbose = F,ReturnTimeSeries = TRUE)
ap_7<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1503164AB.ibw_In1V.ibw",Verbose = F,ReturnTimeSeries = TRUE)
ap_7_1<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/1503162AA.ibw_In1V.ibw",Verbose = TRUE,ReturnTimeSeries = TRUE)
ap_21<-read.ibw("~cchen2/Documents/neuroscience/project/data/develop/2204161AE.ibw_In1V.ibw",Verbose = F,ReturnTimeSeries = TRUE)


ap_21<- cc_decimate(ap_21, 5)
ap_14<- cc_decimate(ap_14, 5)
ap_7_1<- cc_decimate(ap_7_1,5)

# for P7
matplot(ap_7_1[1:5000,1:4], type="l", lty=1, col="slategray4", axes=F,ylab = "")
segments(1050*4,-130, 1250*4,-130) # 200ms
segments(1250*4, -130,1250*4,-110) # 20 mV

# for P14
matplot(ap_14[1:5000,1:4],type="l", lty=1,col = "salmon", axes=F, ylab = "")
segments(1050*4,-100, 1250*4,-100) # 200ms
segments(1250*4, -100,1250*4,-80)
# for P21
matplot(ap_21[1:5000,1:5], type="l", lty=1, col="cyan3", axes=F,ylab = "")
segments(4200,-88, 1250*4,-88) # 200ms
segments(1250*4, -88,1250*4,-68) # 20mV



## plot individual Ap
peak_14 <- which(ap_14[2050:2400,4]==max(ap_14[2050:2400,4])) + 2050
peak_7_1 <- which(ap_7_1[2300:3000,4]==max(ap_7_1[2300:3000,4])) + 2300
peak_21 <- which(ap_21[2000:2200,5]==max(ap_21[2000:2200,5])) + 2000

par(mar = c(0,0,0,0) + 0.1)
plot(ap_21[(peak_21-100):(peak_21+100),5]+10, type = "l",col="cyan3", axes=F,ylab = "",xlab = "")
lines(ap_7_1[(peak_7_1-100):(peak_7_1+100),4], type = "l", col="slategray4")
lines(ap_14[(peak_14-100):(peak_14+100),4], type = "l", col="salmon")
segments(150,-20, 190,-20) # 2ms
segments(190, -20,190,-10) # 10 mV
par(mar = c(5,4,4,2) + 0.1)


## phase plot of the Action potential
plot(ap_14[1:5000,4],type="l", lty=1,col = "salmon", axes=F, ylab = "")


trace <- ap_14[1:5000, 4]
d_trace <- diff(trace)/0.05
plot(trace[-1], d_trace, type = "p", pch=".")


trace_21 <- ap_21[1:5000,5]
d_trace_21 <- diff(trace_21)/0.05

points(trace_21[-1], d_trace_21, type = "p", pch=".", col="cyan3")


plot(trace_21[-1], d_trace_21, type = "p", pch=".", cex=1.5,col="cyan3",xlab="", ylab="", axes = F)
axis(1, pos = 0, at=seq(-80, 20, 10))
axis(4, pos=0)


points(trace[-1], d_trace, type = "p", pch=".")

## save figures for showing
setwd("~cchen2/Documents/neuroscience/project/Figures/Figures/development/PDF/")


# 1. IPSC kinetic properties ## unit in
# amplitude
cairo_pdf("dynamic_plot.pdf", width = 6, height =4, family = "Arial")
plot(trace_21[-1], d_trace_21, type = "p", pch=".", cex=1.5,col="cyan3",xlab="", ylab="", axes = F)
axis(1, pos = 0, at=seq(-80, 20, 10))
axis(4, pos=0)
dev.off()


## 6. data with 0.1 mM EGTA ----------------
dat <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= 3,colNames = TRUE)
dat$Amplitude<- abs(dat$Amplitude)
dat$ID<- as.factor(dat$ID)
dat$Group<- factor(dat$Group, c("P7-9", "P14-16"))

# plot the amplitude VS RS of each data
# For data 3003162AA
p_3003162AA_amp <- trace_plot("3003162AA")

# For data 0404163AA (800*500 for copyplot)
p_0404163AA_amp <- trace_plot("0404163AA")


# caculate the coefficient of variance of Amplitude in each group
dat_re <- melt(dat[,c(1,2,4)], id=c("ID", "Group"))
dat_re_sta <- ddply(dat_re,.(variable,Group,ID),summarise,n=length(value),mean=mean(value),cv=sd(value)/mean(value),sk=skewness(value))

ddply(dat_re_sta,.(Group), summarise, mean=mean(cv), sk=mean(sk))

# plot C.V and Skewness
dat_re_sta_re <- melt(dat_re_sta[,c(2,5,6,7)],id="Group")
dat_re_sta_sum <- ddply(dat_re_sta_re,.(variable,Group),summarise,n=length(value),mean=mean(value),sd=sd(value),se=sd(value)/sqrt(length(value)))

p_cv <- ggplot(subset(dat_re_sta_sum, dat_re_sta_sum$variable=="cv"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill="white",colour="black")+
  labs(x="", y="Coefficient of variance")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,0.8),expand = c(0,0)) +
  geom_point(data=dat_re_sta,aes(Group, cv,colour=Group),position = position_dodge(.9)) +
  scale_color_manual(values = c("black","red")) +
  theme(legend.position="none")

p_skew <- ggplot(subset(dat_re_sta_sum, dat_re_sta_sum$variable=="sk"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill="white",colour="black")+
  labs(x="", y="Skewness")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,0.8),expand = c(0,0)) +
  geom_point(data=dat_re_sta,aes(Group, cv,colour=Group),position = position_dodge(.9)) +
  scale_color_manual(values = c("black","red")) +
  theme(legend.position="none")
plot_grid(p_cv, p_skew)

## 7. data analysis and plot for Ca blocker usage ---------
dat <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= 4,colNames = TRUE)
dat$Amplitude <- abs(dat$Amplitude)
dat$Blocker<- as.factor(dat$Blocker)

# for agatoxin
p_0404163AB_amp<- trace_plot("0404163AB")

# for conotoxin
p_0704162AB_amp<-trace_plot("0704162AB")

## 8. calculate the jitter of each data, hist plot -------------
dat <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= 6,colNames = TRUE)
dat$ID <- as.factor(dat$ID)
dat$Group <- as.factor(dat$Group)

# bar plot of latency
dat_p7 <- subset(dat, dat$ID=="0704165AA"|dat$ID=="0704166AA")
n_7<- length(dat_p7$Latency)
mean_7<- mean(dat_p7$Latency)
sd_7<- sd(dat_p7$Latency)
FWHM_7<- 2.358*sd_7
binwidth_7<- 0.05
p7_hist <- ggplot(dat_p7, aes(Latency)) +
  #geom_histogram(aes(y=..density..), binwidth = 0.05,colour="black", fill="white")+
  geom_histogram(color="black", fill="white",binwidth = 0.05)+
  labs(x="Latency (ms)", y="Number")+
  theme(axis.line.x = element_line(),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_x_continuous(limits=c(0.2,2)) +
  scale_y_continuous(limits = c(0,8),expand=c(0,0))+
  stat_function(fun= function(x, mean, sd, n, bw){ 
    dnorm(x = x, mean = mean, sd = sd) * n * bw
  }, 
  args=list(mean=mean_7, sd=sd_7,n=n_7,bw=binwidth_7))

dat_p14<- subset(dat, dat$ID=="2805152AC"|dat$ID=="2805151AB")
n_14<- length(dat_p14$Latency)
mean_14<- mean(dat_p14$Latency)
sd_14<- sd(dat_p14$Latency)
FWHM_14<- 2.358*sd_14
binwidth_14<- 0.05
p14_hist <- ggplot(dat_p14, aes(Latency)) +
  #geom_histogram(aes(y=..density..), binwidth = 0.05,colour="black", fill="white")+
  geom_histogram(color="red", fill="white",binwidth = 0.05)+
  labs(x="Latency (ms)", y="Number")+
  theme(axis.line.x = element_line(),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_x_continuous(limits=c(0.2,2)) +
  scale_y_continuous(limits = c(0,8),expand=c(0,0))+
  stat_function(fun= function(x, mean, sd, n, bw){ 
    dnorm(x = x, mean = mean, sd = sd) * n * bw
  }, 
  args=list(mean=mean_14, sd=sd_14,n=n_14,bw=binwidth_14),colour="red")

dat_p21 <- subset(dat, dat$ID=="1804161AA")
n_21<- length(dat_p21$Latency)
mean_21<- mean(dat_p21$Latency)
sd_21<- sd(dat_p21$Latency)
FWHM_21<- 2.358*sd_21
binwidth_21<- 0.05
p21_hist <- ggplot(dat_p21, aes(Latency)) +
  geom_histogram(color="blue", fill="white",binwidth = 0.05)+
  labs(x="Latency (ms)", y="Number")+
  theme(axis.line.x = element_line(),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_x_continuous(limits=c(0.2,2)) +
  scale_y_continuous(limits = c(0,8),expand=c(0,0))+
  stat_function(fun= function(x, mean, sd, n, bw){ 
    dnorm(x = x, mean = mean, sd = sd) * n * bw
  }, 
  args=list(mean=mean_21, sd=sd_21,n=n_21,bw=binwidth_21),colour="blue")

plot_grid(p7_hist, p14_hist, p21_hist)

## 9. analyse the AP properties -----------
dat_ap <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= 5,colNames = TRUE)
dat_ap$Group <- factor(dat_ap$Group, c("P7-9", "P14-16", "P21-23"))

dat_ap_re <- melt(dat_ap, id=c("ID", "Group"))
dat_ap_sta <- ddply(dat_ap_re,.(variable,Group),summarise,n=length(value),mean=mean(value),sd=sd(value),se=sd(value)/sqrt(length(value)))

p_ap_Rin <- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Rin"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon", "cyan3")) +
  labs(x="", y=expression(paste(" Input resistance (M", Omega,")")))+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,1600),expand = c(0,0)) +
  geom_jitter(data=dat_ap,aes(Group, Rin),shape=1,width = 0.25) +
  theme(legend.position="none")+
  annotate(x=c(1,1,2,2), y=c(1300, 1320, 1320, 1300),"path" )+
  annotate(x=c(1,1,3,3),y=c(1400, 1420, 1420, 1400),"path")+
  annotate("text", x=1.5, y=1320,label="**", size=6)+
  annotate("text",x=2,y=1420, label="***", size=6)

p_Ap_amp<- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Amplitude.1st"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon", "cyan3")) +
  labs(x="", y="Peak amplitude of 1st AP (mV)")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,60),expand = c(0,0)) +
  geom_jitter(data=dat_ap,aes(Group, Amplitude.1st), shape=1,width = 0.25) +
  theme(legend.position="none")

p_ap_duration<- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Half.duration"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon", "cyan3")) +
  labs(x="", y="Half duration of 1st AP (ms)")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,3),expand = c(0,0)) +
  geom_jitter(data=dat_ap,aes(Group, Half.duration), shape=1,width=0.25) +
  
  theme(legend.position="none")+
  annotate("path", x=c(1,1,2,2),y=c(2.5,2.55,2.55,2.5))+
  annotate("path", x=c(1,1,3,3), y=c(2.8, 2.85, 2.85, 2.8))+
  annotate("path", x=c(2,2,3,3), y=c(2.3,2.35,2.35,2.3))+
  annotate("text", x=1.5, y=2.55, label="*", size=6)+
  annotate("text", x=2, y=2.85, label="***", size=6)+
  annotate("text", x= 2.5, y=2.35, label="***", size=6)

p_ap_freq<- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Max.freq"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon", "cyan3")) +
  labs(x="", y="Number of APs")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,250),expand = c(0,0)) +
  geom_jitter(data=dat_ap,aes(Group, Max.freq), shape=1,width = 0.25) +
  theme(legend.position="none")+
  annotate("path", x=c(1,1,2,2), y=c(200,205, 205, 200))+
  annotate("path", x=c(1,1,3,3), y=c(220, 225, 225, 220))+
  annotate("text", x=1.5, y=205, label="**", size=6)+
  annotate("text", x=2, y=225, label="***", size=6)

# p_ap_vr <- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Vrest"), aes(Group, mean, fill=Group)) +
#   geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
#   geom_bar(stat="identity", fill="white",colour="black")+
#   labs(x="", y="Number of APs")+
#   theme(axis.line.x = element_line(),
#         axis.line.y =element_line(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
#   scale_y_continuous(limits=c(0,200),expand = c(0,0)) +
#   geom_point(data=dat_ap,aes(Group, Vrest,colour=Group),position = position_dodge(.9)) +
#   scale_color_manual(values = c("black","red","blue")) +
#   theme(legend.position="none")


p_ap_riseslope<- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Max.slope.rise"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", colour="black")+
  scale_fill_manual(values = c("slategray4","salmon", "cyan3")) +
  labs(x="", y="??")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0)) +
  geom_point(data=dat_ap,aes(Group, Max.slope.rise,colour=Group),position = position_dodge(.9)) +
  theme(legend.position="none")

p_ap_decayslope <- ggplot(subset(dat_ap_sta, dat_ap_sta$variable=="Max.slope.decay"), aes(Group, mean, fill=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill="white",colour="black")+
  labs(x="", y="??")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0)) +
 geom_point(data=dat_ap,aes(Group, Max.slope.decay,colour=Group),position = position_dodge(.9)) +
  scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")

p_ap_property<- plot_grid(p_ap_Rin, p_Ap_amp, p_ap_duration,p_ap_freq, ncol=4,rel_widths = 0.6)

## statistical results
# 1. input resistance
t_input<- aov(Rin~Group, data = dat_ap)
summary(t_input)
TukeyHSD(t_input)
kruskal.test(Rin~Group, data = dat_ap)
pairwise.wilcox.test(dat_ap$Rin, dat_ap$Group)

# 2. half duration of the 1st AP
t_duration<- aov(Half.duration~Group, data = dat_ap)
summary(t_duration)
TukeyHSD(t_duration)
kruskal.test(Half.duration~Group, data = dat_ap)
pairwise.wilcox.test(dat_ap$Half.duration, dat_ap$Group)

# 3. AP firing frequency
t_firing<- aov(Max.freq~Group, data = dat_ap)
summary(t_firing)
TukeyHSD(t_firing)
kruskal.test(Max.freq~Group, data = dat_ap)
pairwise.wilcox.test(dat_ap$Max.freq, dat_ap$Group)


## 10. AP firing patten with current injection----
dat_current <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= "Current injection",colNames = TRUE)
dat_current$ID<- c()
dat_current$Group <- factor(dat_current$Group, c("P7-9", "P14-16","P21-23"))
dat_current_sta <- ddply(dat_current, .(Group, Current), summarize,n=length(Freq),mean=mean(Freq),sd=sd(Freq),se=sd(Freq)/sqrt(length(Freq)) )

## plot the data
p_current <- ggplot(dat_current_sta, aes(Current, mean, group=Group, colour=Group))+ 
  geom_point(aes(colour=Group))+ 
  geom_line(aes(colour=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se, colour=Group),width=.2)+
  scale_color_manual(values = c("slategray4","salmon","cyan3"))+
  labs(x= "Input Current (pA)", y= "APs Frequency (Hz)")+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_x_continuous(breaks = seq(-100,350,50)) 
p_current

## for jonas email 071016

dat_current_max<- ddply(dat_current,.(ID, Group), summarise,Freq=max(Freq))
dat_current__max_sta <- ddply(dat_current_max, .(Group),summarize,n=length(Freq),mean=mean(Freq),sd=sd(Freq),se=sd(Freq)/sqrt(length(Freq)) )

p_max <- ggplot(subset(dat_current__max_sta, dat_current__max_sta$Group=="P14-16"), aes(Group, mean))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill="white",colour="black")+
  labs(x="", y="Max AP freq (Hz)")+
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(),
        axis.line.y =element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"))+
  scale_y_continuous(limits=c(0,150),expand = c(0,0)) +
  scale_x_discrete(labels="")+
  geom_point(data=subset(dat_current_max, dat_current_max$Group=="P14-16"),aes(Group, Freq),col="black",position = position_dodge(.9)) +
  # scale_color_manual(values = c("black","red","blue")) +
  theme(legend.position="none")
p_current <- ggplot(subset(dat_current_sta, dat_current_sta$Group=="P14-16"), aes(Current, mean, group=Group, colour=Group))+ 
  geom_point(aes(colour=Group))+ 
  geom_line(aes(colour=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se, colour=Group),width=.2)+
  scale_color_manual(values = c("black"))+
  labs(x= "Input Current (pA)", y= "APs Frequency (Hz)")+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_x_continuous(breaks = seq(-100,350,50))+
  theme(legend.position="none")

## 11. Ca2+ sensitivity-----
dat_sens <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data 060717.xlsx",sheet= "Sens_failure",colNames = TRUE)
dat_sens <- dat_sens[, 1:5]
dat_sens$Group<- factor(dat_sens$Group, c("P7-9", "P14-16","P21-23"))
# dat_sens$Amplitude<- dat_sens$Amplitude/1000
# dat_sens<- subset(dat_sens, dat_sens$Concentration!="1")
# dat_sens<- subset(dat_sens, dat_sens$Concentration!="10")
dat_sens_sta<- ddply(dat_sens, .(Group, Concentration), summarise,n=length(Nor.amp),mean=mean(Nor.amp),sd=sd(Nor.amp),se=sd(Nor.amp)/sqrt(length(Nor.amp)))
# dat_sens_sta<- ddply(dat_sens, .(Group, Concentration), summarise,n=length(Amplitude),mean=mean(Amplitude),sd=sd(Amplitude),se=sd(Amplitude)/sqrt(length(Amplitude)))

Group<- levels(dat_sens_sta$Group)

## Hill equation fit (need more data points)
a <- rep(0, length(Group))
b <- rep(0, length(Group))
c <- rep(0, length(Group))


for (i in seq_along(Group)){
  dat_fit <- dat_sens_sta[which(dat_sens_sta$Group==Group[i]),]
  sens_fit <- nls(mean~a*(1+(b/Concentration)^c)^(-1), data=dat_fit, start=list(a=3, b= 3, c=2))
  sens_fit <- nls(mean~a*(1+(b/Concentration)^c)^(-1), data=dat_fit, start=list(a=3, b= 3, c=2))
    
  a_fit<- round(summary(sens_fit)$coefficient[1,1], digits=4)
  b_fit<- round(summary(sens_fit)$coefficient[2,1], digits=4)
  c_fit<- round(summary(sens_fit)$coefficient[3,1], digits=4)
  a[i]<- a_fit
  b[i]<- b_fit
  c[i]<- c_fit
}

fun_7 <- function(x) a[1]*(1+(b[1]/x)^c[1])^(-1)
fun_14 <- function(x) a[2]*(1+(b[2]/x)^c[2])^(-1)
fun_21 <- function(x) a[3]*(1+(b[3]/x)^c[3])^(-1)

ggplot(data = dat_sens_sta, aes(Concentration, mean, colour=Group))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se, colour=Group), width=.05) +
  scale_colour_manual(values = c("slategray4","salmon","cyan3"))+
  labs(x=expression(paste("Ca"^"2+ ", "concentration (mM)", sep="")),y="Normalized IPSC")+
  stat_function(fun=fun_7, colour="slategray4")+
  stat_function(fun=fun_14, colour="#F8766D")+
  stat_function(fun=fun_21, colour="#00BFC4")+
  scale_x_continuous(limits = c(0.45,20), trans = "log10", breaks = c(0.5,0.7,1,2,10,20))+
  scale_y_continuous(limits = c(0, 8))+
  theme(legend.position=c(0.2,0.8))+
  theme(legend.title=element_blank())

## plot every points
p_cop_point <- ggplot(data = dat_sens, aes(Concentration, Nor.amp, colour=Group, shape=Group))+
  geom_jitter()+
  scale_colour_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  labs(x= expression(paste("Ca"^"2+ ", "concentration (mM)", sep="")),y="Normalized IPSC")+
  stat_function(fun=fun_7, colour="slategray4")+
  stat_function(fun=fun_14, colour="#F8766D")+
  stat_function(fun=fun_21, colour="#00BFC4")+
  scale_x_continuous(limits = c(0.3,20), trans = "log10", breaks = c(0.5,0.7,1,2,10,20))+
  # scale_y_continuous(limits = c(0, 8))+
  theme(legend.position=c(0.2,0.8))+
  theme(legend.title=element_blank())

setwd("/Users/cchen2/Documents/neuroscience/project/Graphs/Figures/development/PDF/")
cairo_pdf("copperativity.pdf", width = 5.5, height =3.6, family = "Arial")
p_cop_point
dev.off()

## points plot Amp change during recording (0811162AA)
dat_sens_ex <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= "Sens_example",colNames = TRUE)
dat_sens_ex$Amp[dat_sens_ex$Amp>0]=0

p_amp<- ggplot(dat_sens_ex, aes(Time, -Amp/1000))+
  geom_point()+
  labs(x="", y="Amplitude (nA)")+
  theme(axis.line = element_line(colour = "black"),
        # axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0))+
  annotate("path", x=c(0, 150), y=c(6,6))+
  annotate("path", x=c(250,450), y=c(6,6))+
  annotate("path", x=c(600,800), y=c(6,6))+
  annotate("path", x=c(900,1125), y=c(6,6))+
  annotate("path", x=c(1300,1500), y=c(6,6))

p_rs <- ggplot(data = dat_sens_ex, aes(Time, Rs))+
  geom_line()+
  labs(x="Time (s)")+
  theme(axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  theme(legend.key = element_blank())+
  scale_y_continuous(limits=c(0,8), expand = c(0,0),breaks=c(0,4,8))

P_sens_example <- plot_grid(p_amp,p_rs, ncol=1, nrow=2,align = "v",rel_heights = c(3,1.2))

## save the image
setwd("/Users/cchen2/Documents/neuroscience/project/Graphs/Figures/development/PDF/")
cairo_pdf("sens_example.pdf", width = 5.6, height =3.8, family = "Arial")
P_sens_example
dev.off()

### trace plot for showing the sensitivity (0811162AA)
vol <- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/0811162AA.ibw_In1V.ibw", Verbose = F, ReturnTimeSeries = T)
cur <- read.ibw("~cchen2/Documents/neuroscience/project/data/develop/0811162AA.ibw_In2I.ibw", Verbose = F, ReturnTimeSeries = T)
q <- 5

cur1 <- cc_decimate(cur,q)

## for 0.7 mM Ca2+
vol_0.7_plot<- qplot(seq_along(vol[9900:11000,90]), vol[9900:11000,90], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-50,0) )


current_0.7_mean <- rowMeans(cur[,80:90])
current_0.7_plot<- as.data.frame(cur1[(9900:11000)/q,80:90])
current_0.7_plot$ID <- 1:nrow(current_0.7_plot)
current_0.7_plot1<- melt(current_0.7_plot, id.vars = "ID")
mean_0.7_current<- data.frame(ID=current_0.7_plot$ID,mean=current_0.7_mean[9900:11000])
cur_0.7_plot<- ggplot()+ 
  geom_line(data = current_0.7_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=mean_0.7_current, aes(x=ID, y=mean))+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  scale_y_continuous(limits = c(-6000,0))

trace_0.7<- plot_grid(vol_0.7_plot, cur_0.7_plot,nrow = 2, rel_heights = c(0.3,1))      


## for 2 mM Ca2+
vol_2_plot<- qplot(seq_along(vol[9900:11000,1]), vol[9900:11000,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-50,0) )


current_2_mean <- rowMeans(cur[,1:11])
current_2_plot<- as.data.frame(cur1[(9900:11000)/q,1:11])
current_2_plot$ID <- 1:nrow(current_2_plot)
current_2_plot1<- melt(current_2_plot, id.vars = "ID")
mean_2_current<- data.frame(ID=current_2_plot$ID,mean=current_2_mean[9900:11000])
cur_2_plot<- ggplot()+ 
  geom_line(data = current_2_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=mean_2_current, aes(x=ID, y=mean))+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  scale_y_continuous(limits = c(-6000,0))

trace_2<- plot_grid(vol_2_plot, cur_2_plot,nrow = 2, rel_heights = c(0.3,1))      

## for 4 mM Ca2+
vol_4_plot<- qplot(seq_along(vol[9900:11000,160]), vol[9900:11000,160], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-50,0) )


current_4_mean <- rowMeans(cur[,150:160])
current_4_plot<- as.data.frame(cur1[(9900:11000)/q,150:160])
current_4_plot$ID <- 1:nrow(current_4_plot)
current_4_plot1<- melt(current_4_plot, id.vars = "ID")
mean_4_current<- data.frame(ID=current_4_plot$ID,mean=current_4_mean[9900:11000])
cur_4_plot<- ggplot()+ 
  geom_line(data = current_4_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=mean_4_current, aes(x=ID, y=mean))+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  scale_y_continuous(limits = c(-6000,0))

trace_4<- plot_grid(vol_4_plot, cur_4_plot,nrow = 2, rel_heights = c(0.3,1))      

## for 10 mM Ca2+
vol_10_plot<- qplot(seq_along(vol[9900:11000,220]), vol[9900:11000,220], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(1000,1000),y=c(-50,0) )


current_10_mean <- rowMeans(cur[,215:225])
current_10_plot<- as.data.frame(cur1[(9900:11000)/q,215:225])
current_10_plot$ID <- 1:nrow(current_10_plot)
current_10_plot1<- melt(current_10_plot, id.vars = "ID")
mean_10_current<- data.frame(ID=current_10_plot$ID,mean=current_10_mean[9900:11000])
cur_10_plot<- ggplot()+ 
  geom_line(data = current_10_plot1, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=mean_10_current, aes(x=ID, y=mean))+
  theme(line = element_blank(),
        text = element_blank(),
        axis.line = element_blank(),
        title = element_blank())+
  scale_y_continuous(limits = c(-6000,0))+
  annotate("path", x=c(800,1000,1000,1000),y=c(-4000,-4000,-4000,-2000))

trace_10<- plot_grid(vol_10_plot, cur_10_plot,nrow = 2, rel_heights = c(0.3,1))      

trace_Ca_plot<- plot_grid(trace_0.7, trace_2, trace_4, trace_10, ncol = 1)

# ## vertical plot
# trace_ca_ver <- plot_grid(trace_0.7,cur_2_plot,cur_4_plot,cur_10_plot, ncol = 1)
# # save PNG for PPT
# setwd("~cchen2/Documents/neuroscience/Syt7\ project/Figures/PDF/")
# png("Ca_dependent.png", width = 5, height = 26, units = "cm", res = 300,bg="transparent")
# trace_ca_ver
# dev.off()
## save the image
setwd("/Users/cchen2/Documents/neuroscience/project/Graphs/Figures/development/PDF/")
cairo_pdf("sens_trace_example.pdf", width = 8.6, height =2, family = "Arial")
trace_Ca_plot
dev.off()


## Exponential growth fit (by rate)
a <- rep(0, length(Group))
b <- rep(0, length(Group))
c <- rep(0, length(Group))


for (i in seq_along(Group)){
  dat_fit <- dat_sens_sta[which(dat_sens_sta$Group==Group[2]),]
  sens_fit <- nls(mean~a*exp(b*Concentration)+c, data=dat_fit, start=list(a=0.1, b=0.7,c=0.5),algorithm="port")
  a_fit<- round(summary(sens_fit)$coefficient[1,1], digits=4)
  b_fit<- round(summary(sens_fit)$coefficient[2,1], digits=4)
  c_fit<- round(summary(sens_fit)$coefficient[3,1], digits=4)
  a[i]<- a_fit
  b[i]<- b_fit
  c[i]<- c_fit
}
# sense_fit1<- nls(Norm.amp~ a*exp(1)^(Concentration/b), data = dat_sens, start = list(a=1, b=2))
f_wt<- function(x) a*(x^b)/(c^b+x^b)
ggplot(data=dat_sens_sta, aes(Concentration, mean, Group=Group))+
  geom_point(aes(colour=Group))+
  # stat_function(fun=f_wt, colour="blue")+
  # scale_y_continuous(limits = c(0, 2.2), expand = c(0,0))+
  scale_x_continuous(limits = c(0, 10),breaks = c(0.5, 0.6,0.7,0.8,0.9, 1, 2,3, 4,10))


## Hill equationf fit of the P14-16 data(081116)
dat_mean <- subset(dat_sens_sta, dat_sens_sta$Group=="P14-16")



sens_fit <- nls(mean~a*(1+(b/Concentration)^c)^(-1), data=dat_mean, start=list(a=3, b= 3, c=2))
a<- round(summary(sens_fit)$coefficient[1,1], digits=4)
b<- round(summary(sens_fit)$coefficient[2,1], digits=4)
c<- round(summary(sens_fit)$coefficient[3,1], digits=4)

fun_wt <- function(x) a*(1+(b/x)^c)^(-1)

ggplot(data = dat_mean, aes(Concentration, mean))+
  geom_point()+
  coord_trans(x="log10")+
  stat_function(fun=fun_wt, colour="blue")+
  scale_x_continuous(breaks = c(seq(0.4, 1, 0.1),2,4,10))

## linear fit for the low concentration
dat_low <- subset(dat_mean, dat_mean$Concentration<10)
fit_low <- lm(log10(mean)~log10(Concentration), dat_low)  
plot(log10(dat_low$Concentration), log10(dat_low$mean))

abline(fit_low, col="red")
summary(fit_low)
## point and line plot (091116)
ggplot(data=dat_sens_sta, aes(Concentration, mean, colour=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2)+
  geom_point()+
  geom_line()+
  labs(y="Nor.ampliitude")+
  theme(legend.position=c(0.8,0.2))+
  theme(legend.title=element_blank())

## log-log plot
ggplot(data = dat_sens_sta, aes(log10(Concentration), log10(mean), colour=Group))+
  geom_point(data=subset(dat_sens_sta, Concentration < 4)) + 
  geom_point(data=subset(dat_sens_sta, Concentration > 2), alpha=.8) +
  geom_smooth(data = subset(dat_sens_sta, Concentration< 4), method = "lm", se=F)+
  

## get the coefficient of the lm fit
dat_sens_sta1<- dat_sens_sta[dat_sens_sta$Concentration<4,]
Group_name <- levels(dat_sens_sta$Group)

coff <- rep(0, length(Group_name))
Group <- rep(0, length(Group_name))
for (i in seq_along(Group_name)){
  dat_fit <- subset(dat_sens_sta1, dat_sens_sta1$Group==Group_name[i])
  fit <- lm(log10(mean)~log10(Concentration), data = dat_fit)
  Group[i]<- Group_name[i]
  coff[i]<- as.numeric(fit$coefficients[2])
}
dat_cof<- data.frame(Group, coff)


## do the normalize with IPSC in 10 mM Ca
dat_sens <- dat_sens[complete.cases(dat_sens$Nor.amp1),]
dat_sens$Group<- as.factor(dat_sens$Group)
dat_sens_sta1<- ddply(dat_sens, .(Group, Concentration), summarise,n=length(Nor.amp1),mean=mean(Nor.amp1),sd=sd(Nor.amp1),se=sd(Nor.amp1)/sqrt(length(Nor.amp1)))


ggplot(data = dat_sens_sta1, aes(Concentration, mean, colour=Group))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se, colour=Group), width=.2)+
  labs(y="Normalized IPSC")+
  stat_function(fun=fun_14, colour="#F8766D")+
  stat_function(fun=fun_21, colour="#00BFC4")+
  scale_x_continuous(limits = c(0, 10),breaks=c(0.7,1, 2, 4,10))+
  theme(legend.position=c(0.2,0.8))+
  theme(legend.title=element_blank())


Group<- levels(dat_sens_sta1$Group)

## Hill equation fit (need more data points)
a <- rep(0, length(Group))
b <- rep(0, length(Group))
c <- rep(0, length(Group))


for (i in seq_along(Group)){
  dat_fit <- dat_sens_sta1[which(dat_sens_sta1$Group==Group[i]),]
  if (i<3){
    sens_fit <- nls(mean~a*(1+(b/Concentration)^c)^(-1), data=dat_fit, start=list(a=1.5, b= 5, c=1))
  } else {
    sens_fit <- nls(mean~a*(1+(b/Concentration)^c)^(-1), data=dat_fit, start=list(a=22, b= 30, c=1))
  }
  a_fit<- round(summary(sens_fit)$coefficient[1,1], digits=4)
  b_fit<- round(summary(sens_fit)$coefficient[2,1], digits=4)
  c_fit<- round(summary(sens_fit)$coefficient[3,1], digits=4)
  a[i]<- a_fit
  b[i]<- b_fit
  c[i]<- c_fit
}

fun_14 <- function(x) a[1]*(1+(b[1]/x)^c[1])^(-1)
fun_21 <- function(x) a[2]*(1+(b[2]/x)^c[2])^(-1)

## 12.analysis the 10APs @ 50Hz data------
setwd("~cchen2/Documents/neuroscience/project/data/develop/")
# for p7-9 group
ID_p7 <- c("2006162AA","0704162AA","0911161AA","1011162AB","1011164AA","1411161AB","1611161AC","1611162AD")
num_p7<- c(10,10,20,10,10,12,20,20)
ratio_p7 <- rep(0, length(ID_p7)*10)
amp_p7 <- rep(0, length(ID_p7)*10)
for (i in seq_along(ID_p7)){
  amp<- c_Amp10(ID_p7[i], num_p7[i],9950,14300,300)[[1]]
  ratio <- c_Amp10(ID_p7[i], num_p7[i],9950,14300,300)[[2]]
  amp_p7[(10*i-9): (10*i)]<- amp
  ratio_p7[(10*i-9): (10*i)]<- ratio
}
# amp_2006162AA <- c_Amp10("2006162AA", 10,9950,14300, 300)
## for p14-16 group
ID_p14 <- c("2805153AA","2805152AB","2805151AA","1305152AA","1106151AB","1305151AA","1105154AB","0306151AB")
num_p14<- rep(50, 8)
ratio_p14 <- rep(0, length(ID_p14)*10)
amp_p14 <- rep(0, length(ID_p14)*10)
 for (i in seq_along(ID_p14)){
   amp<- c_Amp10(ID_p14[i], num_p14[i],9950,14300,300)[[1]]
   ratio <- c_Amp10(ID_p14[i], num_p14[i],9950,14300,300)[[2]]
   amp_p14[(10*i-9): (10*i)]<- amp
   ratio_p14[(10*i-9): (10*i)]<- ratio
 }


# for p21-23 group
ID_p21 <- c("2305162AC","2204163AC","2204162AB","1804161AC","1804161AB","1210160AB","1210163AF","2211160AG")
num_p21<- c(10,18,20,10,20,20,20,20)
ratio_p21 <- rep(0, length(ID_p21)*10)
amp_p21 <- rep(0, length(ID_p21)*10)
for (i in seq_along(ID_p21)){
  amp<- c_Amp10(ID_p21[i], num_p21[i],9950,14300,300)[[1]]
  ratio <- c_Amp10(ID_p21[i], num_p21[i],9950,14300,300)[[2]]
  amp_p21[(10*i-9): (10*i)]<- amp
  ratio_p21[(10*i-9): (10*i)]<- ratio
}

Group<- c(rep("P7-9", length(ID_p7)*10), rep("P14-16", length(ID_p14)*10), rep("P21-23",length(ID_p21)*10))
Stim <- rep(1:10, length(Group)/10)
Peak<- c(amp_p7, amp_p14, amp_p21)
PeakR<-c(ratio_p7, ratio_p14, ratio_p21)

dat_devp_50Hz<- data.frame(Group, Stim, Peak, PeakR)
write.xlsx(dat_devp_50Hz, "~cchen2/Documents/neuroscience/project/data\ analysis/development/50Hz.xlsx")
## 1. show the traces of 50Hz data in each group
# 1). for p7 group (2006162AA)
p7_v <- read.ibw("1011162AB.ibw_In1V.ibw",Verbose = FALSE, ReturnTimeSeries = F )
p7_i <- read.ibw("1011162AB.ibw_In2i.ibw",Verbose = FALSE, ReturnTimeSeries = F)
p7_v <- cc_decimate(p7_v, 20)
p7_i <- cc_decimate(p7_i, 20)
p7_i_mean <- rowMeans(p7_i)



p7_vol<- qplot(seq_along(p7_v[250:1250,1]), p7_v[250:1250,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(950,950),y=c(-60,-10) )
  # annotate("text", x=19000, y=-35,label="50 mV" , hjust=0)

p7_current_plot<- as.data.frame(p7_i[250:1250,1:10])
p7_current_plot$ID <- 1:nrow(p7_current_plot)
p7_current_plot<- melt(p7_current_plot, id.vars = "ID")
p7_mean_current<- data.frame(ID=p7_current_plot$ID,mean=p7_i_mean[250:1250])

p7_cur_plot<- ggplot()+ 
  geom_line(data = p7_current_plot, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p7_mean_current, aes(x=ID, y=mean), colour="slategray4")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(850,950,950,950),y=c(-2500,-2500,-2500,-2000))
  #annotate("text", x=18000, y=-2500, label="100 ms", vjust=0)+
  #annotate("text", x=19000, y=-2250,label="500 pA", hjust=0)

p7_50_trace <- plot_grid(p7_vol, p7_cur_plot, nrow = 2, rel_heights = c(0.5,1))
  

# 2). for p14 group (2805153AA)
p14_v <- read.ibw("2805151AA.ibw_In1V.ibw",Verbose = FALSE, ReturnTimeSeries = TRUE )
p14_i <- read.ibw("2805151AA.ibw_In2i.ibw",Verbose = FALSE, ReturnTimeSeries = TRUE)
p14_v <- cc_decimate(p14_v, 20)
p14_i <- cc_decimate(p14_i, 20)
p14_i_mean <- rowMeans(p14_i[,33:43])

p14_vol<- qplot(seq_along(p14_v[250:1250,1]), p14_v[250:1250,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(950,950),y=c(-60,-10) )
  #annotate("text", x=19000, y=-35,label="50 mV" , hjust=0)

p14_current_plot<- as.data.frame(p14_i[250:1250,33:43])
p14_current_plot$ID <- 1:nrow(p14_current_plot)
p14_current_plot<- melt(p14_current_plot, id.vars = "ID")
p14_mean_current<- data.frame(ID=p14_current_plot$ID,mean=p14_i_mean[250:1250])

p14_cur_plot<- ggplot()+ 
  geom_line(data = p14_current_plot, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p14_mean_current, aes(x=ID, y=mean), colour="#F8766D")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(850,950,950,950),y=c(-2500,-2500,-2500,-2000))
  #annotate("text", x=18000, y=-2500, label="100 ms", vjust=0)+
  #annotate("text", x=19000, y=-2250,label="500 pA", hjust=0)

p14_50_trace <- plot_grid(p14_vol, p14_cur_plot, nrow = 2, rel_heights = c(0.5,1))



# 3). for p21 group (1804161AB)
p21_v <- read.ibw("1804161AB.ibw_In1V.ibw",Verbose = FALSE, ReturnTimeSeries = F )
p21_i <- read.ibw("1804161AB.ibw_In2i.ibw",Verbose = FALSE, ReturnTimeSeries = F)
p21_v <- cc_decimate(p21_v, 20)
p21_i <- cc_decimate(p21_i, 20)
p21_i_mean <- rowMeans(p21_i[,10:20])

p21_vol<- qplot(seq_along(p21_v[250:1250,1]), p21_v[250:1250,1], geom = "line")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(950,950),y=c(-60,-10) )
  # annotate("text", x=19000, y=-35,label="50 mV" , hjust=0)

p21_current_plot<- as.data.frame(p21_i[250:1250,10:20])
p21_current_plot$ID <- 1:nrow(p21_current_plot)
p21_current_plot<- melt(p21_current_plot, id.vars = "ID")
p21_mean_current<- data.frame(ID=p21_current_plot$ID,mean=p21_i_mean[250:1250])

p21_cur_plot<- ggplot()+ 
  geom_line(data = p21_current_plot, aes(ID, value, group=variable), colour="gray90")+
  geom_line(data=p21_mean_current, aes(x=ID, y=mean), colour="#00BFC4")+
  theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank())+
  annotate("path", x=c(850,950,950,950),y=c(-2500,-2500,-2500,-2000))
  #annotate("text", x=18000, y=-2500, label="100 ms", vjust=0)+
  #annotate("text", x=19000, y=-2250,label="500 pA", hjust=0)

p21_50_trace <- plot_grid(p21_vol, p21_cur_plot, nrow = 2, rel_heights = c(0.5,1))

trace_plot<- plot_grid(p7_50_trace, p14_50_trace, p21_50_trace, nrow=1)

## 2. plot the 50Hz data
dat_devp_50Hz <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/50Hz.xlsx",sheet= 1,colNames = TRUE)
dat_devp_50Hz$Stim<-factor(dat_devp_50Hz$Stim,c(1:10))
dat_devp_50Hz$Group<- factor(dat_devp_50Hz$Group, c("P7-9","P14-16","P21-23"))

dat_50Hz_sum<-ddply(dat_devp_50Hz,.(Stim,Group),summarise,n=length(PeakR),mean=mean(PeakR),sd=sd(PeakR),se=sd(PeakR)/sqrt(length(PeakR)))

p_50Hz_devp<- ggplot(dat_50Hz_sum, aes(x=Stim, y=mean, group=Group)) + 
  geom_point(aes(colour=Group), size=2)+
  geom_line(aes(colour=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se, colour=Group), width=.2) +
  scale_color_manual(name="",values = c("slategray4","#F8766D","#00BFC4")) +
  xlab("Stimulus number")+ylab("Normalized amplitude") +
  theme(axis.line = element_line(colour = "black"),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0.2,1.5),expand = c(0,0)) +
  scale_x_discrete(labels=c(1:10))+
  theme(legend.position=c(0.8,0.85))+
  geom_hline(yintercept = 1,linetype=2)

p_50Hz_devp

## 3. IPSC2 and IPSC10 plot
dat_psc2_sum<-subset(dat_50Hz_sum, dat_50Hz_sum$Stim=="2")
dat_psc2<- subset(dat_devp_50Hz, dat_devp_50Hz$Stim=="2")

# For peak 10
dat_psc10_sum<-subset(dat_50Hz_sum, dat_50Hz_sum$Stim=="10")
dat_psc10<- subset(dat_devp_50Hz, dat_devp_50Hz$Stim=="10")


b_psc2<- ggplot(dat_psc2_sum, aes(x=Group, y=mean,fill=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill=c("slategray4","#F8766D","#00BFC4"), colour="black") +
  labs(x="", y=expression("IPSC"[2]/"IPSC"[1]))+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face ="plain"))+
  scale_y_continuous(limits=c(0,1.5),expand = c(0,0)) +
  scale_x_discrete(labels=c("P7-9","P14-16","P21-23")) +
  geom_jitter(data=dat_psc2,aes(Group, PeakR),colour="black",shape=1,width = 0.25) +
  theme(legend.position="none")+
  geom_hline(yintercept = 1, linetype=2)+
  annotate(x=c(1,1,2,2), y=c(1.32,1.34,1.34,1.32),"path")+
  annotate("text",x=1.5,y=1.34, label="***", size=6)+
  annotate(x=c(1,1,3,3), y=c(1.36,1.38,1.38,1.36),"path")+
  annotate("text",x=2,y=1.38, label="***", size=6)
  

b_psc10<- ggplot(dat_psc10_sum, aes(x=Group, y=mean,fill=Group))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se), width=.2, position = position_dodge(.9))+
  geom_bar(stat="identity", fill=c("slategray4","#F8766D","#00BFC4"), colour="black") +
  labs(x="", y=expression("IPSC"[10]/"IPSC"[1]))+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain")) +
  scale_y_continuous(limits=c(0,1),expand = c(0,0)) +
  geom_jitter(data=dat_psc10,aes(Group, PeakR),colour="black", shape=1,width = 0.25) +
  theme(legend.position="none")+
  annotate(x=c(1,1,2,2), y=c(0.88,0.9,0.9,0.88),"path")+
  annotate("text",x=1.5,y=0.9, label="***", size=6)+
  annotate(x=c(1,1,3,3), y=c(0.93,0.95,0.95,0.93),"path")+
  annotate("text",x=2,y=0.95, label="***", size=6)

p_ppr<- plot_grid(b_psc2,b_psc10, ncol = 2)

## cobine 50Hz trace plot and analysis data
p_analysis <- plot_grid(p_50Hz_devp, p_ppr, nrow = 2 )

p_50hz <- plot_grid(trace_plot, p_analysis, nrow = 2, labels = c("A",""))+
  theme(plot.margin = unit(c(1.5,1,1,1), "cm")) 

## save the figure
setwd("~cchen2/Documents/neuroscience/project/Figures/Figures/development/PDF")

cairo_pdf("dev_50Hz.pdf", width = 140/25.6, height = 90/25.6, family = "Arial")
p_50Hz_devp
dev.off()

cairo_pdf("dev_50Hz_ppr.pdf", width = 100/25.6, height = 80/25.6, family = "Arial")
p_ppr
dev.off()

## statistic test for 2 and 10th response
pairwise.wilcox.test(dat_psc2$PeakR, dat_psc2$Group)
pairwise.wilcox.test(dat_psc10$PeakR, dat_psc10$Group)


## 13.variance and mean analysis------
dat_var<- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data 060717.xlsx", sheet = "variance_mean", colNames = T)
dat_var$Amplitude[dat_var$Amplitude >= -50]=0
dat_var$Amplitude<- dat_var$Amplitude/-1000
dat_var$ID<- as.factor(dat_var$ID)
dat_var$Group<- factor(dat_var$Group, c("P7-9","P14-16","P21-23"))
dat_var_sta<- ddply(dat_var,.(Group,ID,Ca.con), summarize,  mean=mean(Amplitude), var=var(Amplitude))

dat_var_sta_14<- subset(dat_var_sta, dat_var_sta$Group=="P14-16")
dat_var_sta_7<- subset(dat_var_sta, dat_var_sta$Group=="P7-9")
dat_var_sta_21<- subset(dat_var_sta, dat_var_sta$Group=="P21-23")

## plot v-m and get the numbers for each data
# for the p14 group
names_14<- as.character(unique(dat_var_sta_14$ID))[c(1:8)]
N_14 <- rep(0, length(names_14))
pr_14 <- rep(0, length(names_14))
quantal_14 <- rep(0, length(names_14))
for (i in seq_along(names_14)) {
  dat<- subset(dat_var_sta_14, dat_var_sta_14$ID==names_14[i])
  fit_14<- nls(var~ a*mean- mean^2/b, data=dat,start = list(a= 0.5, b= 10) )
  a_14<- round(summary(fit_14)$coefficients[1, 1], 4)
  b_14<- round(summary(fit_14)$coefficients[2, 1], 4)
  fun_14 <- function(x) a_14*x - x^2/b_14
  N_14[i]<- b_14
  mean_2 <- dat$mean[which(dat$Ca.con==2)]
  pr_14[i]<- round(mean_2/(a_14*b_14), 4)
  quantal_14[i]<- a_14
  
  x<- ggplot(dat, aes(x=mean, y=var))+geom_point()+
    labs(title=names_14[i])+
    stat_function(fun=fun_14, colour="#F8766D")+
    labs(x="Mean Current (nA)", y="Variance (nA^2)")
    
  show(x)
}

# for p21 group
names_21<- as.character(unique(dat_var_sta_21$ID))[c(2,5)]
N_21 <- rep(0, length(names_21))
pr_21 <- rep(0, length(names_21))
quantal_21<- rep(0, length(names_21))
for (i in seq_along(names_21)) {
  dat<- subset(dat_var_sta_21, dat_var_sta_21$ID==names_21[i])
  fit_21<- nls(var~ a*mean- mean^2/b, data=dat,start = list(a= 0.3, b= 80) )
  a_21<- round(summary(fit_21)$coefficients[1, 1], 4)
  b_21<- round(summary(fit_21)$coefficients[2, 1], 4)
  
  fun_21 <- function(x) a_21*x - x^2/b_21
  
  x<- ggplot(dat, aes(x=mean, y=var))+geom_point()+
    labs(title=names_21[i])+
    stat_function(fun=fun_21, colour="#F8766D")+
    labs(x="Mean Current (nA)", y="Variance (nA^2)")
  show(x)
  N_21[i]<- b_21
  mean_2 <- dat$mean[which(dat$Ca.con==2)]
  quantal_21[i]<- a_21
  pr_21[i]<- round(mean_2/(a_21*b_21), 4)
}

# for P7 group
names_7<- as.character(unique(dat_var_sta_7$ID))
N_7 <- rep(0, length(names_7))
pr_7 <- rep(0, length(names_7))
quantal_7<- rep(0, length(names_7))
for (i in seq_along(names_7)) {
  dat<- subset(dat_var_sta_7, dat_var_sta_7$ID==names_7[i])
  fit_7<- nls(var~ a*mean- mean^2/b, data=dat,start = list(a= 0.5, b= 10) )
  a_7<- round(summary(fit_7)$coefficients[1, 1], 4)
  b_7<- round(summary(fit_7)$coefficients[2, 1], 4)
  fun_7 <- function(x) a_7*x - x^2/b_7
  
  x<- ggplot(dat, aes(x=mean, y=var))+geom_point()+
    labs(title=names_7[i])+
    stat_function(fun=fun_7, colour="#F8766D")+
    labs(x="Mean Current (nA)", y="Variance (nA^2)")
  show(x)
  N_7[i]<- b_7
  mean_2 <- dat$mean[which(dat$Ca.con==2)]
  pr_7[i]<- round(mean_2/(a_7*b_7), 4)
  quantal_7[i]<- a_7
}


## combine data and plot
Pr<- c(pr_7,pr_14,pr_21)
N<- c(N_7, N_14, N_21)
Quantal <- c(quantal_7, quantal_14, quantal_21)
ID <- c(names_7, names_14, names_21)
Group <- c(rep("P7-9", length(names_7)), rep("P14-16", length(names_14)), rep("P21-23", length(names_21)))


dat_var_ana <- data.frame(ID, Group, N, Pr,Quantal)
dat_var_ana$Group<- factor(dat_var_ana$Group, c("P7-9","P14-16","P21-23"))

dat_var_ana_melt <- melt(dat_var_ana, id=c("Group","ID"))

dat_var_ana_melt_sta <- ddply(dat_var_ana_melt, .(variable,Group),summarise,n=length(value),mean=mean(value),sd=sd(value),se=sd(value)/sqrt(length(value)))

p_var_N <- ggplot(subset(dat_var_ana_melt_sta,dat_var_ana_melt_sta$variable=="N"), aes(Group, mean))+
  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity",colour='black')+
  geom_bar(stat="identity", fill=c("slategray4","#F8766D","#00BFC4"), colour="black") +
  labs(x="", y="Number of release sites")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain")) +
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  geom_jitter(data = dat_var_ana,aes(Group, N),colour="black", shape=1,width = 0.25) +
  theme(legend.position="none")

p_var_pr <- ggplot(subset(dat_var_ana_melt_sta,dat_var_ana_melt_sta$variable=="Pr"), aes(Group, mean))+
  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity",colour='black')+
  geom_bar(stat="identity", fill=c("slategray4","#F8766D","#00BFC4"), colour="black") +
  labs(x="", y="Pr")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain")) +
  scale_y_continuous(limits=c(0,0.8),expand = c(0,0)) +
  geom_jitter(data = dat_var_ana,aes(Group, Pr),colour="black", shape=1,width = 0.25) +
  theme(legend.position="none")

p_var_quantal <- ggplot(subset(dat_var_ana_melt_sta,dat_var_ana_melt_sta$variable=="Quantal"), aes(Group, mean))+
  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  geom_bar(position=position_dodge(), stat="identity",colour='black')+
  geom_bar(stat="identity", fill=c("slategray4","#F8766D","#00BFC4"), colour="black") +
  labs(x="", y="Quantal size (nA)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain")) +
  scale_y_continuous(limits=c(0,0.6),expand = c(0,0)) +
  geom_jitter(data = dat_var_ana,aes(Group, Quantal),colour="black", shape=1,width = 0.25) +
  theme(legend.position="none")

p_var<- plot_grid(p_var_quantal, p_var_N, p_var_pr, nrow = 1)
## save the image
setwd("~cchen2/Documents/neuroscience/project/Figures/Figures/development/PDF/")

cairo_pdf("var_mean.pdf", width = 140/25.6, height = 75/25.6, family = "Arial")
p_var
dev.off()

cairo_pdf("var_mean_N.pdf", width = 2.3, height = 3.5, family = "Arial")
p_var_N
dev.off()

cairo_pdf("var_mean_quantal.pdf", width = 2.3, height = 3.5, family = "Arial")
p_var_quantal
dev.off()

cairo_pdf("var_mean_pr.pdf", width = 2.3, height = 3.5, family = "Arial")
p_var_pr
dev.off()
  
## plot the variance and mean for three groups
# split data to mean and var
dat_var_mean <- dat_var_sta[,1:4]
dat_var_var <- dat_var_sta[, c(1:3, 5)]
dat_var_mean_sta <- ddply(dat_var_mean,.(Group,Ca.con), summarize,  mean=mean(mean), sd=sd(mean),se=sd(mean)/sqrt(length(mean)) )
dat_var_var_sta <- ddply(dat_var_var,.(Group,Ca.con), summarize,  var=mean(var), sd=sd(var),se=sd(var)/sqrt(length(var)) )
dat_var_mean_sta$var <- dat_var_var_sta$var

## plot combined data
Group <- levels(dat_var_mean_sta$Group)
a <- rep(0, 3)
b <- rep(0, 3)


for (i in seq_along(Group)) {
  dat<- dat_var_mean_sta[which(dat_var_mean_sta$Group==Group[i]),]
  fit<- nls(var~ a*mean- mean^2/b, data=dat,start = list(a= 0.5, b= 10) )
  a[i]<- round(summary(fit)$coefficients[1, 1], 4)
  b[i]<- round(summary(fit)$coefficients[2, 1], 4)
  
}

fun_7 <- function(x) a[1]*x - x^2/b[1]
fun_14 <- function(x) a[2]*x - x^2/b[2]
fun_21 <- function(x) a[3]*x - x^2/b[3]

## quantal, N and Pr (var = q*mean-mean^2/N, mean = N*P*q)
dat_mean_2mM <- subset(dat_var_mean_sta, dat_var_mean_sta$Ca.con=="2")
p_vm<- ggplot(dat_var_mean_sta, aes(x=mean, y=var, Group=Group))+geom_point(aes(colour=Group))+
  scale_colour_manual(values = c("slategray4","#F8766D","#00BFC4"))+
  stat_function(fun=fun_7, colour="slategray4")+
  stat_function(fun=fun_14, colour="#F8766D")+
  stat_function(fun=fun_21, colour="#00BFC4")+
  labs(x="Mean Current (nA)", y=expression(paste("Variance (", nA^{2}, ")", sep = "")))+
  scale_y_continuous(limits = c(0, 0.6),expand = c(0,0))+
  scale_x_continuous(limits = c(0,8))+
  theme(legend.position=c(0.1,0.8), legend.title=element_blank())
  
  # annotate("text",x=dat_var_sta_7$mean-0.2, y=dat_var_sta_7$var, label= dat_var_sta_7$Ca.con )+
  # annotate("text", x=2, y=0.02, label= paste("N=",round(N_7,0),""), col="gray")+
  # annotate("text", x=2, y=0, label= paste("Q=",a_7*1000,"pA", sep=""), col="gray")+
  # annotate("text", x=2, y=-0.02, label= paste("Pr=",round(p_7, 2),""), col="gray")
  # scale_y_continuous(limits = c(0, 0.3))
## save the image
setwd("/Users/cchen2/Documents/neuroscience/project/Graphs/Figures/development/PDF/")
cairo_pdf("variance_mean.pdf", width = 5.5, height =3.6, family = "Arial")
p_vm
dev.off()
## 14. calculate the sd of IPSC prameters--------
dat_IPSC_sd <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= "IPSC SD",colNames = TRUE)
dat_IPSC_sd$ID <- as.factor(dat_IPSC_sd$ID)
dat_IPSC_sd$Group <- as.factor(dat_IPSC_sd$Group)
dat_IPSC_sd_re <- melt(dat_IPSC_sd, id=c("ID", "Group"))
dat_IPSC_sd_sta <- ddply(dat_IPSC_sd_re, .(Group, ID,variable), summarise, n=length(value),mean=mean(value),sd=sd(value),cv=sd(value)/mean(value))

#

## 15. EM data analysis-------
dat_vesicle <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/development\ data.xlsx",sheet= "radius_vesicle",colNames = TRUE)
dat_vesicle$Group<- factor(dat_vesicle$Group, c("P7-9", "P14-16", "P21-23"))
dat_vesicle$Radius<- dat_vesicle$Radius*2.366 #(the size of pixel)
dat_vesicle_mean <- ddply(dat_vesicle, .(ID,Group), summarise, Radius=mean(Radius))
dat_vesicle_sta <- ddply(dat_vesicle_mean, .(Group), summarise, n = length(Radius),mean=mean(Radius),sd=sd(Radius), se=sd(Radius)/sqrt(length(Radius)))


ggplot(dat_vesicle, aes(Radius, Group=Group,colour=Group)) +
  stat_ecdf(data = dat_vesicle, aes(Radius, Group=ID, colour=Group), alpha=0.5)+
  stat_ecdf(size=1)+
  labs(x="Radius of vesicles", y= "Cummulative probability")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(family = "Arial",size = 12, face = "plain"),
        panel.background = element_blank()) +
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position=c(0.8,0.9))+
  theme(legend.title=element_blank())


## 16. Analyze the buttons per avtive zone------
dat_activezone <- read.xlsx("~cchen2/Documents/neuroscience/project/data\ analysis/development/EM_results.xlsx",sheet = 5, colNames = T)
dat_activezone_sta <- ddply(dat_activezone, .(Group), summarise, n = length(release_site),mean=mean(release_site),sd=sd(release_site), se=sd(release_site)/sqrt(length(release_site)))

##  Save plots for figures----
setwd("/Users/cchen2/Documents/neuroscience/project/Graphs/Figures/development/PDF/")


# 1. IPSC kinetic properties ## unit in
# amplitude
cairo_pdf("IPSC_amplitude.pdf", width = 2.7, height = 3.5, family = "Arial")
p_amplitude
dev.off()

# risetime 
cairo_pdf("IPSC_risetime.pdf", width = 2.5, height = 3.5, family = "Arial")
p_risetime
dev.off()

cairo_pdf("IPSC_duration.pdf", width = 2.5, height = 3.5, family = "Arial")
p_halfduration
dev.off()

cairo_pdf("IPSC_latency.pdf", width = 2.5, height = 3.5, family = "Arial")
p_latency
dev.off()

cairo_pdf("IPSC_sd_latency.pdf", width = 2.5, height = 3.5, family = "Arial")
p_latency.sd
dev.off()

cairo_pdf("IPSC_tau.pdf", width = 2.5, height = 3.5, family = "Arial")
p_tau
dev.off()

# 2. histgrame of the latency
cairo_pdf("hist_P7_latency.pdf", width = 3.3, height =2.4, family = "Arial")
p7_hist
dev.off()

cairo_pdf("hist_P14_latency.pdf", width = 3.3, height =2.4, family = "Arial")
p14_hist
dev.off()

cairo_pdf("hist_P21_latency.pdf", width = 3.3, height =2.4, family = "Arial")
p21_hist
dev.off()

# 3. normalized 10mM EGTA data
cairo_pdf("Norm_10EGTA.pdf", width = 6.3, height =3.7, family = "Arial")
dat_norm
dev.off()

cairo_pdf("EGTA_depression.pdf", width = 2.5, height =3.5, family = "Arial")
p_10EGTA_depression
dev.off()


# 4. Trace for showing the effect of 10mM EGTA
cairo_pdf("10EGTA_trace_p7.pdf", width = 5.5, height =3.7, family = "Arial")
p_trace_p7
dev.off()

cairo_pdf("10EGTA_trace_p21.pdf", width = 5.5, height =3.7, family = "Arial")
p_trace_p21
dev.off()

cairo_pdf("10EGTA_trace_p14.pdf", width = 5.5, height =3.7, family = "Arial")
p_trace_p14
dev.off()


# 5. The Ap firing property between groups
cairo_pdf("ap_rin.pdf", width = 2.2, height =3.2, family = "Arial")
p_ap_Rin
dev.off()

cairo_pdf("ap_amp.pdf", width = 2.1, height =3.2, family = "Arial")
p_Ap_amp
dev.off()

cairo_pdf("ap_duration.pdf", width = 2, height =3.2, family = "Arial")
p_ap_duration
dev.off()

cairo_pdf("ap_freq.pdf", width = 2.1, height =3.2, family = "Arial")
p_ap_freq
dev.off()

## 6. for 50Hz data
cairo_pdf("50hz_stimulation.pdf", width = 10.5, height =4, family = "Arial")
p_analysis
dev.off()

tiff("50Hz_trace_plot.tif",width = 10.5, height = 3, units = "in", res=300 )
trace_plot
dev.off()
# 10. Ap firing pattern
cairo_pdf("firing_pattern.pdf", width = 5.5, height =3.4, family = "Arial")
p_current
dev.off()

cairo_pdf("var.pdf", width = 5.5, height =3.4, family = "Arial")
p_vm
dev.off()
