###################################################################################################################################################
# For questions, contact: 

# Abhishek Gaur
# National Research Council Canada
# Abhishek.Gaur@nrc-cnrc.gc.ca; +1-613-998-9799
###################################################################################################################################################





# A demonstration of bias correction for the city of Montreal is performed.


# set working directory to "demonstration package" folder
# setwd("/Users/xuanluo/Documents/cordex/bias-correction")

# Data required are kept in "./input data" folder:
# 1. Observational data
# 2. RCM data for: 
    # a) observational time-period
    # b) RCM: 2001-2020, 2041-2060, 2081-2100

install.packages("lubridate")
library(lubridate) # for date time operations

obs_cal=read.csv("./input data/obs_LA_Airport_2001-2020_clean.csv")
rcm_cal=read.csv("./input data/2020-10-15_WDTF_Los_Angeles_Airport_hourly-weather-data-CORDEX_2001-2020-rcp85.csv")
rcm_2010s=read.csv("./input data/2020-10-15_WDTF_Los_Angeles_Airport_hourly-weather-data-CORDEX_2001-2020-rcp85.csv")
rcm_2050s=read.csv("./input data/2020-10-15_WDTF_Los_Angeles_Airport_hourly-weather-data-CORDEX_2041-2060-rcp85.csv")
rcm_2090s=read.csv("./input data/2020-10-15_WDTF_Los_Angeles_Airport_hourly-weather-data-CORDEX_2081-2100-rcp85.csv")

# ** NEW: prepare monthly dataframes of observations and RCM data
obs_cal_mth=lapply(1:12, function(x) subset(obs_cal,month %in% x))
rcm_cal_mth=lapply(1:12, function(x) subset(rcm_cal,month %in% x))

rcm_2010s_mth=lapply(1:12, function(x) subset(rcm_2010s,month %in% x))
rcm_2050s_mth=lapply(1:12, function(x) subset(rcm_2050s,month %in% x))
rcm_2090s_mth=lapply(1:12, function(x) subset(rcm_2090s,month %in% x))



# reframing data for bias correction
obs_cal_MBCn=lapply(1:12, function(x) cbind(obs_cal_mth[[x]]$tas,
                                            obs_cal_mth[[x]]$ps,
                                            obs_cal_mth[[x]]$hurs,
                                            obs_cal_mth[[x]]$sfcWind))
obs_cal_QDM=lapply(1:12, function(x) obs_cal_mth[[x]]$rsds)


rcm_cal_MBCn=lapply(1:12, function(x) cbind(rcm_cal_mth[[x]]$tas,
                                            rcm_cal_mth[[x]]$ps,
                                            rcm_cal_mth[[x]]$hurs,
                                            rcm_cal_mth[[x]]$sfcWind))
rcm_cal_QDM=lapply(1:12, function(x) rcm_cal_mth[[x]]$rsds)


rcm_2010s_MBCn=lapply(1:12, function(x) cbind(rcm_2010s_mth[[x]]$tas,
                                              rcm_2010s_mth[[x]]$ps,
                                              rcm_2010s_mth[[x]]$hurs,
                                              rcm_2010s_mth[[x]]$sfcWind))
rcm_2010s_QDM=lapply(1:12, function(x) rcm_2010s_mth[[x]]$rsds)


rcm_2050s_MBCn=lapply(1:12, function(x) cbind(rcm_2050s_mth[[x]]$tas,
                                              rcm_2050s_mth[[x]]$ps,
                                              rcm_2050s_mth[[x]]$hurs,
                                              rcm_2050s_mth[[x]]$sfcWind))
rcm_2050s_QDM=lapply(1:12, function(x) rcm_2050s_mth[[x]]$rsds)


rcm_2090s_MBCn=lapply(1:12, function(x) cbind(rcm_2090s_mth[[x]]$tas,
                                              rcm_2090s_mth[[x]]$ps,
                                              rcm_2090s_mth[[x]]$hurs,
                                              rcm_2090s_mth[[x]]$sfcWind))
rcm_2090s_QDM=lapply(1:12, function(x) rcm_2090s_mth[[x]]$rsds)



ratio.seq=c("FALSE","TRUE","TRUE","TRUE")
trace=c(Inf,10,1,0.1)
varnames=c("tas","ps","hurs","sfcWind")


# MBCn model calibration
install.packages("MBC")
library(MBC)


# Apply the MBCn methods

set.seed(1)
MBCout_2010s=lapply(1:12, function(x) MBCn(o.c=obs_cal_MBCn[[x]],
                                           m.c=rcm_cal_MBCn[[x]],
                                           m.p=rcm_2010s_MBCn[[x]],
                                           ratio.seq=ratio.seq,
                                           trace=trace))

QDMout_2010s=lapply(1:12, function(x) QDM(o.c=obs_cal_QDM[[x]],
                                          m.c=rcm_cal_QDM[[x]],
                                          m.p=rcm_2010s_QDM[[x]],
                                          ratio=TRUE,
                                          trace=0.1))





set.seed(1)
MBCout_2050s=lapply(1:12, function(x) MBCn(o.c=obs_cal_MBCn[[x]],
                                           m.c=rcm_cal_MBCn[[x]],
                                           m.p=rcm_2050s_MBCn[[x]],
                                           ratio.seq=ratio.seq,
                                           trace=trace))

QDMout_2050s=lapply(1:12, function(x) QDM(o.c=obs_cal_QDM[[x]],
                                          m.c=rcm_cal_QDM[[x]],
                                          m.p=rcm_2050s_QDM[[x]],
                                          ratio=TRUE,
                                          trace=0.1))





set.seed(1)
MBCout_2090s=lapply(1:12, function(x) MBCn(o.c=obs_cal_MBCn[[x]],
                                           m.c=rcm_cal_MBCn[[x]],
                                           m.p=rcm_2090s_MBCn[[x]],
                                           ratio.seq=ratio.seq,
                                           trace=trace))

QDMout_2090s=lapply(1:12, function(x) QDM(o.c=obs_cal_QDM[[x]],
                                          m.c=rcm_cal_QDM[[x]],
                                          m.p=rcm_2090s_QDM[[x]],
                                          ratio=TRUE,
                                          trace=0.1))









# combine monthly bc values in one df

rcm_bc_cal=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_cal_mth[[x]]$time_lst)),
                      do.call(rbind,lapply(1:12, function(x) MBCout_2010s[[x]][[1]])))
rcm_bc_cal=rcm_bc_cal[order(rcm_bc_cal$time_lst),]
colnames(rcm_bc_cal)=c("time_lst",varnames)



rcm_bc_2010s=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2010s_mth[[x]]$time_lst)),
                        do.call(rbind,lapply(1:12, function(x) MBCout_2010s[[x]][[2]])))
rcm_bc_2010s=rcm_bc_2010s[order(rcm_bc_2010s$time_lst),]
colnames(rcm_bc_2010s)=c("time_lst",varnames)



rcm_bc_2050s=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2050s_mth[[x]]$time_lst)),
                        do.call(rbind,lapply(1:12, function(x) MBCout_2050s[[x]][[2]])))
rcm_bc_2050s=rcm_bc_2050s[order(rcm_bc_2050s$time_lst),]
colnames(rcm_bc_2050s)=c("time_lst",varnames)



rcm_bc_2090s=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2090s_mth[[x]]$time_lst)),
                        do.call(rbind,lapply(1:12, function(x) MBCout_2090s[[x]][[2]])))
rcm_bc_2090s=rcm_bc_2090s[order(rcm_bc_2090s$time_lst),]
colnames(rcm_bc_2090s)=c("time_lst",varnames)






rcm_bc_cal_rsds=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_cal_mth[[x]]$time_lst)),
                           do.call(c,lapply(1:12, function(x) QDMout_2010s[[x]][[1]])))
rcm_bc_cal_rsds=rcm_bc_cal_rsds[order(rcm_bc_cal_rsds$time_lst),]
colnames(rcm_bc_cal_rsds)=c("time_lst","rsds")





rcm_bc_2010s_rsds=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2010s_mth[[x]]$time_lst)),
                             do.call(c,lapply(1:12, function(x) QDMout_2010s[[x]][[2]])))
rcm_bc_2010s_rsds=rcm_bc_2010s_rsds[order(rcm_bc_2010s_rsds$time_lst),]
colnames(rcm_bc_2010s_rsds)=c("time_lst","rsds")



rcm_bc_2050s_rsds=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2050s_mth[[x]]$time_lst)),
                             do.call(c,lapply(1:12, function(x) QDMout_2050s[[x]][[2]])))
rcm_bc_2050s_rsds=rcm_bc_2050s_rsds[order(rcm_bc_2050s_rsds$time_lst),]
colnames(rcm_bc_2050s_rsds)=c("time_lst","rsds")




rcm_bc_2090s_rsds=data.frame(time_lst=do.call(c,lapply(1:12, function(x) rcm_2090s_mth[[x]]$time_lst)),
                             do.call(c,lapply(1:12, function(x) QDMout_2090s[[x]][[2]])))
rcm_bc_2090s_rsds=rcm_bc_2090s_rsds[order(rcm_bc_2090s_rsds$time_lst),]
colnames(rcm_bc_2090s_rsds)=c("time_lst","rsds")







# ensure all variables are within the expected range

# rain should be >=0.1
#if(any(rcm_bc_cal$rain_mm<0.1))
#  rcm_bc_cal$rain_mm[which(rcm_bc_cal$rain_mm<0.1)]=0


# wdr should be 0-360
#if(any(rcm_bc_cal$wdr_deg<0))
#  rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg<0)]=360+rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg<0)]

#if(any(rcm_bc_cal$wdr_deg>=360))
#  rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg>=360)]=rcm_bc_cal$wdr_deg[which(rcm_bc_cal$wdr_deg>=360)]-360


# wsp should be >=0
if(any(rcm_bc_cal$sfcWind<0))
  rcm_bc_cal$sfcWind[which(rcm_bc_cal$sfcWind<0)]=0


# no checks for tas


# rsds should be >=0
if(any(rcm_bc_cal$rsds<0))
  rcm_bc_cal$rsds[which(rcm_bc_cal$rsds<0)]=0


# ps should be >=0
if(any(rcm_bc_cal$ps<0))
  rcm_bc_cal$ps[which(rcm_bc_cal$ps<0)]=0



# hurs should be 0-100
if(any(rcm_bc_cal$hurs<0))
  rcm_bc_cal$hurs[which(rcm_bc_cal$hursr<0)]=0

if(any(rcm_bc_cal$hurs>100))
  rcm_bc_cal$hurs[which(rcm_bc_cal$hurs>100)]=100



# clt should be 0-100
#if(any(rcm_bc_cal$clt_per<0))
#  rcm_bc_cal$clt_per[which(rcm_bc_cal$clt_per<0)]=0

#if(any(rcm_bc_cal$clt_per>100))
#  rcm_bc_cal$clt_per[which(rcm_bc_cal$clt_per>100)]=100



# snd should be >=1
#if(any(rcm_bc_cal$snd_cm<1))
#  rcm_bc_cal$snd_cm[which(rcm_bc_cal$snd_cm<1)]=0












# rain should be >=0.1
#if(any(rcm_bc_2010s$rain_mm<0.1))
#  rcm_bc_2010s$rain_mm[which(rcm_bc_2010s$rain_mm<0.1)]=0


# wdr should be 0-360
#if(any(rcm_bc_2010s$wdr_deg<0))
#  rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg<0)]=360+rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg<0)]

#if(any(rcm_bc_2010s$wdr_deg>=360))
#  rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg>=360)]=rcm_bc_2010s$wdr_deg[which(rcm_bc_2010s$wdr_deg>=360)]-360


# wsp should be >=0
if(any(rcm_bc_2010s$sfcWind<0))
  rcm_bc_2010s$sfcWind[which(rcm_bc_2010s$sfcWind<0)]=0


# no checks for tas


# rsds should be >=0
if(any(rcm_bc_2010s$rsds<0))
  rcm_bc_2010s$rsds[which(rcm_bc_2010s$rsds<0)]=0


# ps should be >=0
if(any(rcm_bc_2010s$ps<0))
  rcm_bc_2010s$ps[which(rcm_bc_2010s$ps<0)]=0



# hurs should be 0-100
if(any(rcm_bc_2010s$hurs<0))
  rcm_bc_2010s$hurs[which(rcm_bc_2010s$hurs<0)]=0

if(any(rcm_bc_2010s$hurs>100))
  rcm_bc_2010s$hurs[which(rcm_bc_2010s$hurs>100)]=100



# clt should be 0-100
#if(any(rcm_bc_2010s$clt_per<0))
#  rcm_bc_2010s$clt_per[which(rcm_bc_2010s$clt_per<0)]=0

#if(any(rcm_bc_2010s$clt_per>100))
#  rcm_bc_2010s$clt_per[which(rcm_bc_2010s$clt_per>100)]=100



# snd should be >=1
#if(any(rcm_bc_2010s$snd_cm<1))
#  rcm_bc_2010s$snd_cm[which(rcm_bc_2010s$snd_cm<1)]=0










# rain should be >=0.1
#if(any(rcm_bc_2050s$rain_mm<0.1))
#  rcm_bc_2050s$rain_mm[which(rcm_bc_2050s$rain_mm<0.1)]=0


# wdr should be 0-360
#if(any(rcm_bc_2050s$wdr_deg<0))
#  rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg<0)]=360+rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg<0)]

#if(any(rcm_bc_2050s$wdr_deg>=360))
#  rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg>=360)]=rcm_bc_2050s$wdr_deg[which(rcm_bc_2050s$wdr_deg>=360)]-360


# wsp should be >=0
if(any(rcm_bc_2050s$sfcWind<0))
  rcm_bc_2050s$sfcWind[which(rcm_bc_2050s$sfcWind<0)]=0


# no checks for tas


# rsds should be >=0
if(any(rcm_bc_2050s$rsds<0))
  rcm_bc_2050s$rsds[which(rcm_bc_2050s$rsds<0)]=0


# ps should be >=0
if(any(rcm_bc_2050s$ps<0))
  rcm_bc_2050s$ps[which(rcm_bc_2050s$ps<0)]=0



# hurs should be 0-100
if(any(rcm_bc_2050s$hurs<0))
  rcm_bc_2050s$hurs[which(rcm_bc_2050s$hurs<0)]=0

if(any(rcm_bc_2050s$hurs>100))
  rcm_bc_2050s$hurs[which(rcm_bc_2050s$hurs>100)]=100



# clt should be 0-100
#if(any(rcm_bc_2050s$clt_per<0))
#  rcm_bc_2050s$clt_per[which(rcm_bc_2050s$clt_per<0)]=0

#if(any(rcm_bc_2050s$clt_per>100))
#  rcm_bc_2050s$clt_per[which(rcm_bc_2050s$clt_per>100)]=100



# snd should be >=1
#if(any(rcm_bc_2050s$snd_cm<1))
#  rcm_bc_2050s$snd_cm[which(rcm_bc_2050s$snd_cm<1)]=0








# rain should be >=0.1
#if(any(rcm_bc_2090s$rain_mm<0.1))
#  rcm_bc_2090s$rain_mm[which(rcm_bc_2090s$rain_mm<0.1)]=0


# wdr should be 0-360
#if(any(rcm_bc_2090s$wdr_deg<0))
#  rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg<0)]=360+rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg<0)]

#if(any(rcm_bc_2090s$wdr_deg>=360))
#  rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg>=360)]=rcm_bc_2090s$wdr_deg[which(rcm_bc_2090s$wdr_deg>=360)]-360


# wsp should be >=0
if(any(rcm_bc_2090s$sfcWind<0))
  rcm_bc_2090s$sfcWind[which(rcm_bc_2090s$sfcWind<0)]=0


# no checks for tas


# rsds should be >=0
if(any(rcm_bc_2090s$rsds<0))
  rcm_bc_2090s$rsds[which(rcm_bc_2090s$rsds<0)]=0


# ps should be >=0
if(any(rcm_bc_2090s$ps<0))
  rcm_bc_2090s$ps[which(rcm_bc_2090s$ps<0)]=0



# hurs should be 0-100
if(any(rcm_bc_2090s$hurs<0))
  rcm_bc_2090s$hurs[which(rcm_bc_2090s$hurs<0)]=0

if(any(rcm_bc_2090s$hurs>100))
  rcm_bc_2090s$hurs[which(rcm_bc_2090s$hurs>100)]=100



# clt should be 0-100
#if(any(rcm_bc_2090s$clt_per<0))
#  rcm_bc_2090s$clt_per[which(rcm_bc_2090s$clt_per<0)]=0

#if(any(rcm_bc_2090s$clt_per>100))
#  rcm_bc_2090s$clt_per[which(rcm_bc_2090s$clt_per>100)]=100



# snd should be >=1
#if(any(rcm_bc_2090s$snd_cm<1))
#  rcm_bc_2090s$snd_cm[which(rcm_bc_2090s$snd_cm<1)]=0



# attach rsds to the bias-corrected data
rcm_bc_cal$rsds=rcm_bc_cal_rsds$rsds
rcm_bc_2010s$rsds=rcm_bc_2010s_rsds$rsds
rcm_bc_2050s$rsds=rcm_bc_2050s_rsds$rsds
rcm_bc_2090s$rsds=rcm_bc_2090s_rsds$rsds



# consider all rsds and wsp less than 0 as runif between 0 and 0.1
set.seed(1)
rcm_bc_cal$rsds[which(rcm_bc_cal$rsds<0.1)]=runif(n=length(which(rcm_bc_cal$rsds<0.1)),min=0,max=0.1)

set.seed(1)
rcm_bc_2010s$rsds[which(rcm_bc_2010s$rsds<0.1)]=runif(n=length(which(rcm_bc_2010s$rsds<0.1)),min=0,max=0.1)

set.seed(1)
rcm_bc_2050s$rsds[which(rcm_bc_2050s$rsds<0.1)]=runif(n=length(which(rcm_bc_2050s$rsds<0.1)),min=0,max=0.1)

set.seed(1)
rcm_bc_2090s$rsds[which(rcm_bc_2090s$rsds<0.1)]=runif(n=length(which(rcm_bc_2090s$rsds<0.1)),min=0,max=0.1)




write.csv(rcm_bc_cal,"./outputs/jg_monthlyMBCn_rcm_bc_cal.csv")
write.csv(rcm_bc_2010s,"./outputs/jg_monthlyMBCn_rcm_bc_2010s.csv")
write.csv(rcm_bc_2050s,"./outputs/jg_monthlyMBCn_rcm_bc_2050s.csv")
write.csv(rcm_bc_2090s,"./outputs/jg_monthlyMBCn_rcm_bc_2090s.csv")










# has bias correction worked?
summary(obs_cal)
summary(rcm_cal)
summary(rcm_bc_cal)


# comparison of PDFs
library(ggplot2)

ggplot()+
  geom_density(data=obs_cal,aes(x=tas),color="grey50",size=6)+
  geom_density(data=rcm_cal,aes(x=tas),color="blue",size=2)+
  geom_density(data=rcm_bc_cal,aes(x=tas),color="red",size=2)+
  xlab("Temperature (?C)")+ylab("Density")+theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=21,face = "bold",vjust=1),
        axis.title.y = element_text(size=21,face = "bold"),
        axis.text.x = element_text(size=21),
        axis.text.y = element_text(size=21),
        legend.title = element_text(size=21,face = "bold"),
        legend.text=element_text(size=21),text = element_text(size=21))
ggsave("./outputs/PDFs_tas_degc_obs_rcm_bcrcm.png",width=12,height=10)


ggplot()+
  geom_density(data=obs_cal,aes(x=sfcWind),color="grey50",size=6)+
  geom_density(data=rcm_cal,aes(x=sfcWind),color="blue",size=2)+
  geom_density(data=rcm_bc_cal,aes(x=sfcWind),color="red",size=2)+
  xlab("Wind speed (m/s)")+ylab("Density")+theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=21,face = "bold",vjust=1),
        axis.title.y = element_text(size=21,face = "bold"),
        axis.text.x = element_text(size=21),
        axis.text.y = element_text(size=21),
        legend.title = element_text(size=21,face = "bold"),
        legend.text=element_text(size=21),text = element_text(size=21))
ggsave("./outputs/PDFs_wsp_mpers_obs_rcm_bcrcm.png",width=12,height=10)








# impact on projected changes

summary(rcm_bc_2010s)
summary(rcm_bc_2050s)
summary(rcm_bc_2090s)


ggplot()+
  geom_density(data=rcm_bc_2010s,aes(x=tas),color="black",size=2)+
  geom_density(data=rcm_bc_2050s,aes(x=tas),color="blue",size=2)+
  geom_density(data=rcm_bc_2090s,aes(x=tas),color="red",size=2)+
  xlab("Temperature (?C)")+ylab("Density")+theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=21,face = "bold",vjust=1),
        axis.title.y = element_text(size=21,face = "bold"),
        axis.text.x = element_text(size=21),
        axis.text.y = element_text(size=21),
        legend.title = element_text(size=21,face = "bold"),
        legend.text=element_text(size=21),text = element_text(size=21))
ggsave("./outputs/PDFs_tas_degc_2010s_2050s_2090s.png",width=12,height=10)



ggplot()+
  geom_density(data=rcm_bc_2010s,aes(x=sfcWind),color="black",size=2)+
  geom_density(data=rcm_bc_2050s,aes(x=sfcWind),color="blue",size=2)+
  geom_density(data=rcm_bc_2090s,aes(x=sfcWind),color="red",size=2)+
  xlab("Temperature (?C)")+ylab("Density")+theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size=21,face = "bold",vjust=1),
        axis.title.y = element_text(size=21,face = "bold"),
        axis.text.x = element_text(size=21),
        axis.text.y = element_text(size=21),
        legend.title = element_text(size=21,face = "bold"),
        legend.text=element_text(size=21),text = element_text(size=21))
ggsave("./outputs/PDFs_wsp_mpers_2010s_2050s_2090s.png",width=12,height=10)


