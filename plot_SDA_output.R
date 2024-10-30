rm(list = ls())
library(Metrics)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(corrplot)
library(Metrics)
library(stringr)
library(grDevices)
library(reshape)

pft<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/site_pft.csv")
colpft<-c("#AA4499","#888888","#D55E00")
names(colpft)<-c("boreal.coniferous","temperate.deciduous.HPDA","semiarid.grassland_HPDA")

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/soilc_neon.Rdata") #neon data,initial value
ic_soc <- apply(soildata,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/site_loc_id_name.Rdata")
site_locs<-site.locs
soilgrids<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/soilgrids_soilC_data_neon39sites.csv")

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/ISCN/iscn_soc_from_map_depth.Rdata")
loca_iscn$soc[12] <- NA 

change_point_f<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/site_change_point_all.csv",header=F)
#####observation#####
sd_one_yr<-soilgrids$Std_soilC_0.200cm
sd_obs <- sd_one_yr*sqrt(12)
mean_obs <- soilgrids$Total_soilC_0.200cm
sd_obs_12yr  <- matrix(rep(sd_obs,12),ncol=12)
mean_obs_12yr <- matrix(rep(mean_obs,12),ncol=12)
time <- seq(2010,2021,1)
######################
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_500km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_soc_500km <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })

##############
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_0km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_soc_0km <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })

#####################
#read results with all obs
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_allobs_500km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_all_obs <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })

#################
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_allobs_0km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_all_obs_0km<- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })


#################

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_nosoc_500km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
sda_no_soc <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })
################


load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_nosoc_0km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
sda_no_soc_0km <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })

##################
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_freerun.Rdata") #Free run without any obs
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
freerun <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
    
  })
###############################
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_2012_500km.Rdata")
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
sda_soc_2012 <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    All.my.data[[listFA]]%>%
      purrr::map_df(function(state.vars){
        
        #finding the mean and Ci for all the state variables
        site.ids %>% unique() %>%
          map_df(function(site){
            (state.vars)[,which(site.ids  %in% site)] %>% 
              as.data.frame %>% 
              mutate(Site=site)
          }) %>%
          tidyr::gather(Variable, Value, -c(Site)) %>%
          group_by(Site,Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper = quantile(Value, 0.975,  na.rm = TRUE))
      }) %>% mutate(Type = paste0("SDA_", listFA),
                    Date = rep(as.Date(names(FORECAST)), each = colnames((All.my.data[[listFA]])[[1]]) %>% length() / length(unique(site.ids))) %>% as.POSIXct()
      )
  })

id_fc_uq<-unique(site.ids)
##################### initial condition
ic_soc_std<-(ic_soc[3,]-ic_soc[1,])/(1.96*2)
ic_soc_std_order<-ic_soc_std[match(pft$site,names(ic_soc_std))]
pft_a<-gsub("semiarid.grassland_HPDA","1",pft$pft)
pft_b<-gsub("temperate.deciduous.HPDA","2",pft_a)
pft_c<-gsub("boreal.coniferous","3",pft_b)
###################
sda_all_obs$std<-(sda_all_obs$Upper-sda_all_obs$Lower)/(1.96*2)
freerun$std<-(freerun$Upper-freerun$Lower)/(1.96*2)
sda_no_soc$std<-(sda_no_soc$Upper-sda_no_soc$Lower)/(1.96*2)
sda_soc_500km$std<-(sda_soc_500km$Upper-sda_soc_500km$Lower)/(1.96*2)
#sda_soc$std<-(sda_soc$Upper-sda_soc$Lower)/4
soc_all_obs_2021<-sda_all_obs %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15")) #Choose the last year
soc_freerun_2021<-freerun %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
soc_no_soc_2021<-sda_no_soc %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
soc_soc_2021<-sda_soc_500km %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
#soc_soc_2021<-sda_soc %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
#Sort the data to make sure the pft types correspond to site correctly.Or to use merge differnt data together by site id 
soilgrids_order <- soilgrids[match(pft$site,soilgrids$Site_ID),]
# for testing with a extreme case: soc_all_obs_order <- soc_all_obs[order(soc_all_obs$Site,decreasing = T),]
soc_all_obs_order <- soc_all_obs_2021[match(pft$site,soc_all_obs_2021$Site),]
soc_freerun_order <- soc_freerun_2021[match(pft$site,soc_freerun_2021$Site),]
soc_nosoc_order <- soc_no_soc_2021[match(pft$site,soc_no_soc_2021$Site),]
soc_soc_order <- soc_soc_2021[match(pft$site,soc_soc_2021$Site),]
#soc_soc_order <- soc_soc_2021[match(pft$site,soc_soc_2021$Site),]

uncertainty_ratio <- sd_obs/soc_freerun_order$std
uncertianty_sda_all <-soc_all_obs_order$std/soc_freerun_order$std
uncertianty_sda_no_soc <- soc_nosoc_order$std/soc_freerun_order$std
uncertianty_sda_soc <- soc_soc_order$std/soc_freerun_order$std

change_point_f$siteid<-site_locs$siteId
change_point_order <-change_point_f[match(pft$site,change_point_f$siteid),]

############## across-site variation of convergence rate, sd_SDA, and sd_freerun
rate_conv <- 1/change_point_order[,2]
par(mar = c(5, 5, 1, 1),mfrow = c(1, 3))
plot(log(uncertainty_ratio),change_point_order[,2],pch = c(16, 17, 5)[as.numeric(pft_c)], lwd=2,col = c("#D55E00","#888888","#AA4499")[as.numeric(pft_c)],xlab=bquote("log("~italic(sd)[obs]~") - log("~italic(sd)[freerun]~")"),cex=2,ylab=bquote(italic(T)[conv]~" (year)"),cex.lab=1.5, xlim=c(-2,4),ylim=c(0,13),yaxt="n",xaxt="n")
abline(lm(change_point_order[,2]~log(uncertainty_ratio)),col="red")
text(-2,12.9,"(a)",cex=1.5)
axis(2,cex.axis=1.5) 
axis(1,cex.axis=1.5)
legend("bottomright",legend=c("Grassland","Deciduous Forest","Coniferous Forest"),pch = c(16, 17, 5), lty=c(0,0,0),lwd=2,cex=1.3,pt.cex=2,col = c("#D55E00","#888888","#AA4499"))
abline(v=0,col="gray",lty=2,lwd=2)
text(-1.2,7,"y=1.43x+5.68",cex=1.5,col="red")
text(-1.2,6,bquote(R^2~"=0.25"),cex=1.5,col="red")
text(-1.2,5,"p<0.001",cex=1.5,col="red")
summary(lm(change_point_order[,2]~log(uncertainty_ratio)))
##plot convergence time~ratio
#plot(log(uncertainty_ratio),rate_conv)
#abline(lm(rate_conv~log(uncertainty_ratio)),col="red")
#summary(lm(rate_conv~log(uncertainty_ratio)))

plot(log(uncertainty_ratio),log(uncertianty_sda_soc),pch = c(16, 17, 5)[as.numeric(pft_c)], lwd=2,col = c("#D55E00","#888888","#AA4499")[as.numeric(pft_c)],xlab=bquote("log("~italic(sd)[obs]~") - log("~italic(sd)[freerun]~")"),cex=2,ylab=bquote("log("~italic(sd)[SDA]~") - log("~italic(sd)[freerun]~")"),cex.lab=1.5, yaxt="n",xaxt="n",ylim=c(-2.2,2),xlim=c(-1.5,4))
axis(2,cex.axis=1.5) 
axis(1,cex.axis=1.5)
lines(seq(-8,10,by=1), seq(-8,10,by=1),type = "l", lwd=2,col="black",lty = 2)
abline(lm(log(uncertianty_sda_soc)~log(uncertainty_ratio)),col="red")
abline(h=0,col="gray",lty=2,lwd=2)
abline(v=0,col="gray",lty=2,lwd=2)
text(2,-1,"y=0.86x-0.94",cex=1.5,col="red")
text(2,-1.2,bquote(R^2~"=0.93, p<0.001"),cex=1.5,col="red")
text(1.4,2,"1:1 line",cex=1.5)
text(-1.5,2,"(b)",cex=1.5)
summary(lm(log(uncertianty_sda_soc)~log(uncertainty_ratio)))
########bootstrapping
library(boot)
regression_func <- function(data, indices) {
  # Resample the data based on the provided indices
  resampled_data <- data[indices, ]
  
  # Fit a linear regression model to the resampled data
  model <- lm(Y ~ X, data = resampled_data)
  
  # Return the regression coefficients (Intercept and slope)
  return(coef(model))
}

data_varlog<-data.frame(X=log(uncertainty_ratio),Y=log(uncertianty_sda_soc))
boot_result <- boot(data = data_varlog, statistic = regression_func, R = 1000)
boot_ci <- boot.ci(boot_result, index=1,type = "perc") #for intercept
boot_ci
boot_ci <- boot.ci(boot_result, index=2,type = "perc") #for slope
boot_ci

plot(ic_soc_std_order,soc_freerun_order$std,pch = c(16, 17, 5)[as.numeric(pft_c)], lwd=2,col = c("#D55E00","#888888","#AA4499")[as.numeric(pft_c)],xlab=bquote(italic(sd)[initial]~"(kg/"~m^2~")"),cex=2,ylab=bquote(italic(sd)[freerun]~"(kg/"~m^2~")"),cex.lab=1.5, yaxt="n",xaxt="n")
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
lines(seq(0,100,by=10), seq(0,100,by=10),type = "l", lwd=2,col="black",lty = 2)
#legend(12,7,legend=c("Grassland","Deciduous Forest","Coniferous Forest"),pch = c(16, 17, 18), cex=1.5,col = c("red", "green","blue"))
text(30,25,"1:1 line",cex=1.5)
abline(lm(soc_freerun_order$std~ic_soc_std_order),col="red")
text(32.5,13,"y=0.53x+1.91",cex=1.5,col="red")
text(32.5,11,bquote(~R^2~"=0.72, p<0.001"),cex=1.5,col="red")
summary(lm(soc_freerun_order$std~ic_soc_std_order))
text(0,28,"(c)",cex=1.5)


####for illustration of convergence time/rate
soc_site <- sda_soc_500km %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS",Site=="1000004907")

fit <- loess(soc_site$Means~time)
soc_site_mean <- soc_site$Means
par(mar = c(4, 5,2, 1))
plot(time,predict(fit),type="l",col="black",ylim=c(10,35),xaxt = "n",yaxt = "n",xlab="",ylab="",lwd=2)
lines(time,mean_obs_12yr[soilgrids$Site_ID=="1000004907",],lwd=2,col="red")
axis(1, at = time,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 3, cex=1.5)
mtext("Year", side = 1, line = 3,cex=1.5)
legend("topright",legend=c("SDA analysis mean","SoilGrids mean"),lty=1,lwd=2,col=c("black","red"))


################only SOC site correlation or not
  
  label_id<-c(letters[1:26], paste0("a",letters[1:26]))
  par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
  k=0
  for (j in seq_along(unique(pft$pft))){
    for (i in 1:39){
      if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
        k=k+1
        ci_yr_site<-sda_soc_500km %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
       # ci_yr_site_0km<- sda_soc_0km %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
        ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
        rownames(ci_yr_site) <- time
        plot(time,ci_yr_site$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(1.2*min(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),1.5*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]))),col="#E69F00",lwd=2,xlab="",ylab="") #median
        polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA)  #green SDA_obs
        
       lines(time,ci_yr_site_free$Means,col=rgb(1,0.4,0.8),ylim=c(0,150),lwd=2) 
       polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
        
      #  lines(time,ci_yr_site_0km$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
      #  polygon(c(time ,rev(time)),c(ci_yr_site_0km$Lower, rev(ci_yr_site_0km$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
        
        # lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
        # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
        obs_lower<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
        obs_upper<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
        obs_lower[obs_lower<0] <- 0
        
        arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=1.5,length=0) #range of data
        # arrows(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]-sd_obs[soilgrids$Site_ID==id_fc_uq[i]], 2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+sd_obs[soilgrids$Site_ID==id_fc_uq[i]],col= "yellow",angle=90,code=3,lwd=1.5,length=0) #range of data
        points(time,mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],],pch=16,col="#009E73",cex=1) #mean of data
        # points(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]],pch=16,col="yellow",cex=1) #mean of data
        
        arrows(2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][1],2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
        points(2021,loca_iscn$soc[loca_iscn$site_ID==id_fc_uq[i]],pch=2,col="blue",cex=1.2,lwd=2)
        #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
        axis(1, at = time,cex.axis=1.5)
        axis(2,cex.axis=1.5)
        #text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),glue(site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],pft$pft[pft$site==id_fc_uq[i]],.sep = "\n"),cex=1)
        text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
        text(2011,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
      }
    }
  }
  mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 1, outer = TRUE,cex=1.5)
  mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)
  
  par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
  legend("bottom", legend=c("freerun","SOC_only_s","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,16,NA,2),lwd=c(0,0,2,2,2),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)

  ########## only SOC 6sites
  site_name <- c("ORNL","TREE","STER","ONAQ","TALL","WREF")
  site_id<-c()
  for (i in seq_along(site_name)){
    site_id[i]<-site_locs$siteId[site_locs$Site_name==site_name[i]]
  }
  #pft<-c("Boreal conifers","Semiarid grassland","Temperate deciduous forest","Boreal conifers")
  
  label_id<-c("(a)","(b)","(c)","(d)","(e)","(f)")
  
  par(mai=c(.5,.6,.2,.2), mfrow = c(3, 2), oma = c(10, 7, 0.2, 0.2),mar = c(3, 6, 1, 1))
  k=0
  for (j in seq_along(unique(pft$pft))){
    for (i in 1:6){
      if (pft$pft[pft$site==site_id[i]]==unique(pft$pft)[j]){
        k=k+1
        ci_yr_site<-sda_soc_500km %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
       # ci_yr_site_0km<- sda_soc_0km %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
 
        #ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
        ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
        rownames(ci_yr_site) <- time
        plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.4*max(max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr[soilgrids$Site_ID==site_id[i],]),na.rm =TRUE)),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
        polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
        
        
        lines(time,ci_yr_site$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
        polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 
        
       # lines(time,ci_yr_site_0km$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
       # polygon(c(time ,rev(time)),c(ci_yr_site_0km$Lower, rev(ci_yr_site_0km$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
        
        
        #lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
        # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
        
        obs_lower<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
        obs_upper<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
        obs_lower[obs_lower<0] <- 0
        
        arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
        points(time,mean_obs_12yr[soilgrids$Site_ID==site_id[i],],pch=16,col="#009E73",cex=1.5) #mean of data
        
        arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
        points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
        #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
        axis(1, at = time,cex.axis=1.5)
        axis(2,cex.axis=1.5)
        text(2015,1.3*max(max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr[soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
        text(2010.3,1.3*max(max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr[soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),label_id[k],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
        obs_error<-round(sd_obs_12yr [soilgrids$Site_ID==site_id[i],12]/((ci_yr_site_free$Upper-ci_yr_site_free$Lower)/(1.96*2))[12],digits=2)
        text(2019,1.3*max(max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr[soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),paste0("obs_error= ",obs_error),cex=2)
      }
    }
  }
  mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 2, outer = TRUE,cex=1.5)
  mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)
  
  par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
  legend('bottom', legend=c("freerun","SOC_only_s","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,16,NA,2),lwd=c(0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)
########Analysis of 39 sites for all obs SDA
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
  if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
    k=k+1
  ci_yr_site<-sda_all_obs %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  rownames(ci_yr_site) <- time
  plot(time,ci_yr_site$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(1.2*min(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),1.5*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]))),col="#E69F00",lwd=2,xlab="",ylab="") #median
 polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA)  #green SDA_obs

  lines(time,ci_yr_site_free$Means,col=rgb(1,0.4,0.8),ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
  
  lines(time,ci_yr_site_nosoil$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
  polygon(c(time ,rev(time)),c(ci_yr_site_nosoil$Lower, rev(ci_yr_site_nosoil$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
 # lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
 # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  obs_lower<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
  obs_upper<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
  obs_lower[obs_lower<0] <- 0
  
  arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=1.5,length=0) #range of data
 # arrows(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]-sd_obs[soilgrids$Site_ID==id_fc_uq[i]], 2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+sd_obs[soilgrids$Site_ID==id_fc_uq[i]],col= "yellow",angle=90,code=3,lwd=1.5,length=0) #range of data
  points(time,mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],],pch=16,col="#009E73",cex=1) #mean of data
 # points(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]],pch=16,col="yellow",cex=1) #mean of data
  
  arrows(2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][1],2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==id_fc_uq[i]],pch=2,col="blue",cex=1.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  #text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),glue(site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],pft$pft[pft$site==id_fc_uq[i]],.sep = "\n"),cex=1)
  text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
  text(2011,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
  }
  }
}
mtext(bquote("Total soil organic carbon (kg/"~m^2~")"), side = 2, line = 1, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend("bottom", legend=c("Free run","LAI+AGB+SWC_spatial","LAI+AGB+SWC+SOC_spatial","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2,2,2),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)

################Analysis of 39 sites for 2012 SOC SDA#################
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
      k=k+1
  ci_yr_site<-sda_soc_2012 %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
 # ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  #ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  rownames(ci_yr_site) <- time
  plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.5*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+1.96*sd_one_yr[soilgrids$Site_ID==id_fc_uq[i]]))),lwd=2,col=rgb(1,0.4,0.8),xlab="",ylab="") #median
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #green SDA_obs
  
  
  lines(time,ci_yr_site_allyr$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site_allyr$Lower, rev(ci_yr_site_allyr$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 
  
  lines(time,ci_yr_site$Means,col=rgb(0,0.8,1),ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #pink,freerun
 # lines(time,ci_yr_site_nosoil$Means,col=rgb(0,0.8,1)) #blue, LAI, AGB, SWC
  #polygon(c(time ,rev(time)),c(ci_yr_site_nosoil$Lower, rev(ci_yr_site_nosoil$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  # lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
  # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  arrows(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]-1.96*sd_one_yr[soilgrids$Site_ID==id_fc_uq[i]], 2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+1.96*sd_one_yr[soilgrids$Site_ID==id_fc_uq[i]],col= "#009E73",angle=90,code=3,lwd=2,length=0)  #for 1yrsp
  #points(time,mean_obs_8yr[soilgrids$Site_ID==id_plot[i],],pch=16,col="yellow",cex=1.5) #mean of obs
  points(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]],pch=16,col="#009E73",cex=1.5) #mean of data for 1yrsp
  
  arrows(2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][1],2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==id_fc_uq[i]],pch=2,col="blue",cex=1.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,colnames(soildata_spinup)==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+1.96*sd_one_yr[soilgrids$Site_ID==id_fc_uq[i]]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
  text(2011,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+1.96*sd_one_yr[soilgrids$Site_ID==id_fc_uq[i]]),na.rm =TRUE),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
    }
  }
}
mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 1, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("freerun","1yrSOC_s","all_obs_s","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)

################calculate mean relative error for 2012 assimilation and all obs assimilation
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(8, 4, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
   if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
      k=k+1
      ci_yr_site_2012<-sda_soc_2012 %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
      ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
      mre_2012 <- (ci_yr_site_2012$Means - mean_obs[soilgrids$Site_ID==id_fc_uq[i]])/mean_obs[soilgrids$Site_ID==id_fc_uq[i]]*100
      mre_all <- (ci_yr_site_allyr$Means- mean_obs[soilgrids$Site_ID==id_fc_uq[i]])/mean_obs[soilgrids$Site_ID==id_fc_uq[i]]*100
      plot(time, mre_2012,type="l",col=rgb(0,0.8,1),xaxt = "n",yaxt = "n",ylim=c(min(ifelse(min(mre_2012,mre_all)<0,2,0)*min(mre_2012,mre_all),-10),ifelse(max(mre_2012,mre_all)<0,-1.5,1.5)*max(mre_2012,mre_all))+5,lwd=2)
      lines(time,mre_all,col= "#E69F00",lwd=2)
      abline(h=0,col="gray",lty=2,lwd=2)
      axis(1, at = time,cex.axis=1.5)
      axis(2,cex.axis=1.5)
      mtext("Residual errors of analysis SOC (%)", side = 2, line = 1, outer = TRUE,cex=1.5)
      mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)
      text(2016,ifelse(max(mre_2012,mre_all)<0,-1.5,1.4)*max(mre_2012,mre_all),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
      text(2011.5,ifelse(max(mre_2012,mre_all)<0,-1.5,1.4)*max(mre_2012,mre_all),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
    }
  }
}
par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("1yrSOC_s","all_obs_s","RE=0"),lty=c(1,1,2),col=c(rgb(0,0.8,1),"#E69F00","gray"),lwd=2,cex=1.8,xpd = TRUE, ncol=3)      
############################site local for 39 sites
mre_all<-c()
mre_all_noc <-c()
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
      k=k+1
  ci_yr_site<-sda_all_obs %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_noc<- sda_all_obs_0km %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
  
  ci_yr_site_2021 <- sda_all_obs %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
  ci_yr_site_noc_2021 <- sda_all_obs_0km %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS",Date==as.Date("2021-07-15"))
  
  mre_all[i] <- (ci_yr_site_2021$Means - mean_obs[soilgrids$Site_ID==id_fc_uq[i]])/mean_obs[soilgrids$Site_ID==id_fc_uq[i]]*100
  mre_all_noc[i] <-(ci_yr_site_noc_2021$Means - mean_obs[soilgrids$Site_ID==id_fc_uq[i]])/mean_obs[soilgrids$Site_ID==id_fc_uq[i]]*100
  
  #rownames(ci_yr_site) <- time
  plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(1.2*min(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),1.5*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #green SDA_obs
  

  lines(time,ci_yr_site_noc$Means,col=rgb(0,0.8,1),ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site_noc$Lower, rev(ci_yr_site_noc$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #pink,freerun
  
  lines(time,ci_yr_site$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col=adjustcolor("#E69F00",alpha=0.5),border=NA) 
  
  # lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
  # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  obs_lower<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
  obs_upper<-mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]
  obs_lower[obs_lower<0] <- 0
  
  arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=1.5,length=0) #range of data
  # arrows(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]-sd_obs[soilgrids$Site_ID==id_fc_uq[i]], 2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]]+sd_obs[soilgrids$Site_ID==id_fc_uq[i]],col= "yellow",angle=90,code=3,lwd=1.5,length=0) #range of data
  points(time,mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],],pch=16,col="#009E73",cex=1) #mean of data
  # points(2012,mean_obs[soilgrids$Site_ID==id_fc_uq[i]],pch=16,col="yellow",cex=1) #mean of data
  
  arrows(2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][1],2010,ic_soc[,colnames(soildata)==id_fc_uq[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==id_fc_uq[i]],pch=2,col="blue",cex=1.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  #text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),glue(site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],pft$pft[pft$site==id_fc_uq[i]],.sep = "\n"),cex=1)
  text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
  text(2011,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==id_fc_uq[i]]),max(mean_obs_12yr[soilgrids$Site_ID==id_fc_uq[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==id_fc_uq[i],]),na.rm =TRUE),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
    }
  }
}
mtext(bquote("Total soil organic carbon (kg/"~m^2~")"), side = 2, line = 1, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("Free run","LAI+AGB+SWC+SOC","LAI+AGB+SWC+SOC_spatial","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)
###############barplot of RE for site correlation or not
data_mre <- data.frame(mre_all_noc,mre_all,id_fc_uq)
colnames(data_mre)<-c("MRE_no_spatial","MRE_spatial","id")
site_locs$siteId <- as.character(site_locs$siteId)
data_mre2 <- data_mre %>% inner_join( site_locs,by=c('id'='siteId'))  

data_mre3<-data_mre2 %>% mutate(relative_change = (data_mre$MRE_no_spatial - data_mre$MRE_spatial)/data_mre$MRE_spatial)
data_mre_order <- data_mre3[order(abs(data_mre3$relative_change)),]

data_mre_long <- data_mre2 %>% select("MRE_no_spatial","MRE_spatial","Site_name") %>% melt(id = "Site_name")

data_mre_long %>% 
  arrange(value) %>%
  mutate(variable = factor(variable, levels=c("MRE_spatial","MRE_no_spatial"))) %>%
  ggplot(aes(variable,value))+geom_col()+facet_wrap(~Site_name,scale='free_y')


##################
data_mre3<-data_mre2 %>% mutate(relative_change = (data_mre$MRE_no_spatial - data_mre$MRE_spatial)/data_mre$MRE_spatial)
data_mre_order <- data_mre3[order(abs(data_mre3$relative_change)),]

label_id<-c(letters[1:26], paste0("a",letters[1:26]))
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(8, 4, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
      k=k+1
    
      plot(time, mre_2012,type="l",col=rgb(0,0.8,1),xaxt = "n",yaxt = "n",ylim=c(min(ifelse(min(mre_2012,mre_all)<0,2,0)*min(mre_2012,mre_all),-10),ifelse(max(mre_2012,mre_all)<0,-1.5,1.5)*max(mre_2012,mre_all))+5,lwd=2)
      lines(time,mre_all,col= "#E69F00",lwd=2)
      abline(h=0,col="gray",lty=2,lwd=2)
      axis(1, at = time,cex.axis=1.5)
      axis(2,cex.axis=1.5)
      mtext("Residual errors of analysis SOC (%)", side = 2, line = 1, outer = TRUE,cex=1.5)
      mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)
      text(2016,ifelse(max(mre_2012,mre_all)<0,-1.5,1.4)*max(mre_2012,mre_all),site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
      text(2011.5,ifelse(max(mre_2012,mre_all)<0,-1.5,1.4)*max(mre_2012,mre_all),paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
    }
  }
}
par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("LAI+AGB+SWC+1yrSOC_spatial","LAI+AGB+SWC+SOC_spatial","RE=0"),lty=c(1,1,2),col=c(rgb(0,0.8,1),"#E69F00","gray"),lwd=2,cex=1.8,xpd = TRUE, ncol=2)      
######################### all obs chose 6 sites 
#id_plot<-c("1000004890","1000004875","1000004919","1000004928")
#site_name<-c("MLBS","ONAQ","STER","RMNP")
site_name <- c("ORNL","TREE","STER","ONAQ","TALL","SOAP")
site_id<-c()
for (i in seq_along(site_name)){
site_id[i]<-site_locs$siteId[site_locs$Site_name==site_name[i]]
}
#pft<-c("Boreal conifers","Semiarid grassland","Temperate deciduous forest","Boreal conifers")
label_id<-c("(a)","(b)","(c)","(d)","(e)","(f)")

par(mai=c(.5,.6,.2,.2), mfrow = c(3, 2), oma = c(10, 7, 0.2, 0.2),mar = c(3, 6, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:6){
    if (pft$pft[pft$site==site_id[i]]==unique(pft$pft)[j]){
      k=k+1
  ci_yr_site<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
 # ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  rownames(ci_yr_site) <- time
  plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(1.2*min(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]),1.5*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
  

  lines(time,ci_yr_site_nosoil$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
  polygon(c(time ,rev(time)),c(ci_yr_site_nosoil$Lower, rev(ci_yr_site_nosoil$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  lines(time,ci_yr_site$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 
  
  #lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
  # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  obs_lower<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
  obs_upper<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
  obs_lower[obs_lower<0] <- 0
  
  arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
  points(time,mean_obs_12yr[soilgrids$Site_ID==site_id[i],],pch=16,col="#009E73",cex=1.5) #mean of data
  
  arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  text(2015,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==site_id[i]]),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
  text(2010.3,1.4*max(max(ci_yr_site_free$Upper),max(ci_yr_site$Upper),max(soildata[,colnames(soildata)==site_id[i]]),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),label_id[k],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
    }
  }
}
mtext(bquote("Total soil organic carbon (kg/"~m^2~")"), side = 2, line = 4, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("Free run", "LAI+AGB+SWC_spatial","LAI+AGB+SWC+SOC_spatial","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)
###################SOC 2012 chose 6 sites 
#id_plot<-c("1000004890","1000004875","1000004919","1000004928")
#site_name<-c("MLBS","ONAQ","STER","RMNP")
site_name <- c("ORNL","TREE","STER","ONAQ","TALL","SOAP")
site_id<-c()
for (i in seq_along(site_name)){
  site_id[i]<-site_locs$siteId[site_locs$Site_name==site_name[i]]
}
#pft<-c("Boreal conifers","Semiarid grassland","Temperate deciduous forest","Boreal conifers")

label_id<-c("(a)","(b)","(c)","(d)","(e)","(f)")

par(mai=c(.5,.6,.2,.2), mfrow = c(3, 2), oma = c(10, 7, 0.2, 0.2),mar = c(3, 6, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:6){
    if (pft$pft[pft$site==site_id[i]]==unique(pft$pft)[j]){
      k=k+1
  ci_yr_site<-sda_soc_2012%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  #ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  rownames(ci_yr_site) <- time
  plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.5*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
  

  lines(time,ci_yr_site_allyr$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
  polygon(c(time ,rev(time)),c(ci_yr_site_allyr$Lower, rev(ci_yr_site_allyr$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 
  
  lines(time,ci_yr_site$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
  polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  
  #lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
  # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  obs_lower<-mean_obs[soilgrids$Site_ID==site_id[i]]-1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
  obs_upper<-mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
  obs_lower[obs_lower<0] <- 0
  
  arrows(2012,obs_lower, 2012,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
  points(2012,mean_obs[soilgrids$Site_ID==site_id[i]],pch=16,col="#009E73",cex=1.5) #mean of data
  
  arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  text(2015,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
  text(2010.3,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),label_id[k],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
    }
  }
}
mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 2, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("freerun","1yrSOC_s","all_obs_s","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)
####################################SOC 2012 2 sites

site_name <- c("ORNL","TREE","STER","ONAQ","TALL","WREF")
site_id<-c()
for (i in seq_along(site_name)){
  site_id[i]<-site_locs$siteId[site_locs$Site_name==site_name[i]]
}
#pft<-c("Boreal conifers","Semiarid grassland","Temperate deciduous forest","Boreal conifers")

par(mai=c(.5,.6,.2,.2), mfrow = c(2, 2), oma = c(5, 5, 0.2, 0.2),mar = c(0, 2, 0, 1))
i=1
ci_yr_site<-sda_soc_2012%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
#ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
rownames(ci_yr_site) <- time
plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.5*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun


lines(time,ci_yr_site_allyr$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
polygon(c(time ,rev(time)),c(ci_yr_site_allyr$Lower, rev(ci_yr_site_allyr$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 

lines(time,ci_yr_site$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)


#lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
# polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)

obs_lower<-mean_obs[soilgrids$Site_ID==site_id[i]]-1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
obs_upper<-mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
obs_lower[obs_lower<0] <- 0

arrows(2012,obs_lower, 2012,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
points(2012,mean_obs[soilgrids$Site_ID==site_id[i]],pch=16,col="#009E73",cex=1.5) #mean of data

arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
#lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
#axis(1, at = time,cex.axis=1.5)
axis(2,cex.axis=1.5)
text(2015,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
text(2010.3,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),"(a)",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 3, cex=1.5)
legend('bottom', legend=c("freerun","1yrSOC_s","all_obs_s","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1,pt.cex=2,xpd = TRUE, ncol=2)

####
i=6
ci_yr_site<-sda_soc_2012%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_nosoil<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
#ci_yr_site_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
rownames(ci_yr_site) <- time
plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.5*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun


lines(time,ci_yr_site_allyr$Means,col="#E69F00",ylim=c(0,150),lwd=2) 
polygon(c(time ,rev(time)),c(ci_yr_site_allyr$Lower, rev(ci_yr_site_allyr$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA) #green SDA_obs 

lines(time,ci_yr_site$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)


#lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
# polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)

obs_lower<-mean_obs[soilgrids$Site_ID==site_id[i]]-1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
obs_upper<-mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr [soilgrids$Site_ID==site_id[i]]
obs_lower[obs_lower<0] <- 0

arrows(2012,obs_lower, 2012,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
points(2012,mean_obs[soilgrids$Site_ID==site_id[i]],pch=16,col="#009E73",cex=1.5) #mean of data

arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
#lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
#axis(1, at = time,cex.axis=1.5)
axis(2,cex.axis=1.5)
text(2015,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
text(2010.3,1.4*max(max(ci_yr_site_allyr$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs[soilgrids$Site_ID==site_id[i]]+1.96*sd_one_yr[soilgrids$Site_ID==site_id[i]]),na.rm =TRUE),"(b)",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
###RE ORNL
i=1
ci_yr_site_2012<-sda_soc_2012 %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
mre_2012 <- (ci_yr_site_2012$Means - mean_obs[soilgrids$Site_ID==site_id[i]])/mean_obs[soilgrids$Site_ID==site_id[i]]*100
mre_all <- (ci_yr_site_allyr$Means- mean_obs[soilgrids$Site_ID==site_id[i]])/mean_obs[soilgrids$Site_ID==site_id[i]]*100
plot(time, mre_2012,type="l",col=rgb(0,0.8,1),xaxt = "n",yaxt = "n",ylim=c(min(ifelse(min(mre_2012,mre_all)<0,2,0)*min(mre_2012,mre_all),-10),ifelse(max(mre_2012,mre_all)<0,-1.5,1.5)*max(mre_2012,mre_all))+5,xlab="",ylab="",lwd=2)
lines(time,mre_all,col= "#E69F00",lwd=2)
abline(h=0,col="gray",lty=2,lwd=2)
axis(1, at = time,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext("Residual errors of SOC (%)", side = 2, line = 3, cex=1.5)
mtext("Year", side = 1, line = 3, outer = TRUE,cex=1.5)
#text(2015,155,"ORNL",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
text(2010.3,155,"(c)",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
legend(2014,150, legend=c("1yrSOC_s","all_obs_s","RE=0"),lty=c(1,1,2),col=c(rgb(0,0.8,1),"#E69F00","gray"),lwd=2,cex=1,xpd = TRUE, ncol=2) 
########RE TALL
i=6
ci_yr_site_2012<-sda_soc_2012 %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
ci_yr_site_allyr<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
mre_2012 <- (ci_yr_site_2012$Means - mean_obs[soilgrids$Site_ID==site_id[i]])/mean_obs[soilgrids$Site_ID==site_id[i]]*100
mre_all <- (ci_yr_site_allyr$Means- mean_obs[soilgrids$Site_ID==site_id[i]])/mean_obs[soilgrids$Site_ID==site_id[i]]*100
plot(time, mre_2012,type="l",col=rgb(0,0.8,1),xaxt = "n",yaxt = "n",ylim=c(min(ifelse(min(mre_2012,mre_all)<0,2,0)*min(mre_2012,mre_all),-10),ifelse(max(mre_2012,mre_all)<0,-1.5,1.5)*max(mre_2012,mre_all))+5,xlab="",ylab="",lwd=2)
lines(time,mre_all,col= "#E69F00",lwd=2)
abline(h=0,col="gray",lty=2,lwd=2)
axis(1, at = time,cex.axis=1.5)
axis(2,cex.axis=1.5)
#mtext("Relative error of analysis SOC (%)", side = 2, line = 1, outer = TRUE,cex=1.5)
#mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)
#text(2015,45,"SOAP",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
text(2010.3,165,"(d)",col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)


############## site correlation 6 sites
site_name <- c("ORNL","TREE","STER","ONAQ","TALL","WREF")
site_id<-c()
for (i in seq_along(site_name)){
  site_id[i]<-site_locs$siteId[site_locs$Site_name==site_name[i]]
}
#pft<-c("Boreal conifers","Semiarid grassland","Temperate deciduous forest","Boreal conifers")

label_id<-c("(a)","(b)","(c)","(d)","(e)","(f)")

par(mai=c(.5,.6,.2,.2), mfrow = c(3, 2), oma = c(10, 7, 0.2, 0.2),mar = c(3, 6, 1, 1))

k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:6){
    if (pft$pft[pft$site==site_id[i]]==unique(pft$pft)[j]){
      k=k+1
  ci_yr_site<-sda_all_obs%>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_noc<- sda_all_obs_0km %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  ci_yr_site_free<- freerun %>% filter(Variable=="TotSoilCarb", Site==site_id[i],Type=="SDA_ANALYSIS")
  rownames(ci_yr_site) <- time
  plot(time,ci_yr_site_free$Means,type="l",xaxt = "n",yaxt = "n",ylim=c(-10,1.5*max(max(ci_yr_site_noc$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]))),col=rgb(1,0.4,0.8),lwd=2,xlab="",ylab="") #median
  polygon(c(time ,rev(time)),c(ci_yr_site_free$Lower, rev(ci_yr_site_free$Upper)),col= rgb(1,0.4,0.8,alpha=0.5),border=NA)  #pink,freerun
  
  lines(time,ci_yr_site$Means,col="#E69F00",lwd=2) #blue, LAI, AGB, SWC
  polygon(c(time ,rev(time)),c(ci_yr_site$Lower, rev(ci_yr_site$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  lines(time,ci_yr_site_noc$Means,col=rgb(0,0.8,1),lwd=2) #blue, LAI, AGB, SWC
  polygon(c(time ,rev(time)),c(ci_yr_site_noc$Lower, rev(ci_yr_site_noc$Upper)),col= rgb(0,0.8,1,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  
  
  #lines(time,ci_yr_site_lai_agb$Means,col=rgb(1,0.2,0.2)) #red, LAI and AGB
  # polygon(c(time ,rev(time)),c(ci_yr_site_lai_agb$Lower, rev(ci_yr_site_lai_agb$Upper)),col= rgb(1,0.2,0.2,alpha=0.5),border=NA)  #rgb(red, green, blue, alpha)
  
  obs_lower<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]-1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
  obs_upper<-mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]
  obs_lower[obs_lower<0] <- 0
  
  arrows(time,obs_lower, time,obs_upper,col= "#009E73",angle=90,code=3,lwd=2,length=0) #range of data
  points(time,mean_obs_12yr[soilgrids$Site_ID==site_id[i],],pch=16,col="#009E73",cex=1.5) #mean of data
  
  arrows(2010,ic_soc[,colnames(soildata)==site_id[i]][1],2010,ic_soc[,colnames(soildata)==site_id[i]][3],length=0.05,angle=90,code=3,lwd=2)  #initail value
  points(2021,loca_iscn$soc[loca_iscn$site_ID==site_id[i]],pch=2,col="blue",cex=2.2,lwd=2)
  #lines(time,rep(soildata_spinup[1212,dirnames_sp==id_fc_uq[i]],12),col="red",lty=2,lwd=2)
  axis(1, at = time,cex.axis=1.5)
  axis(2,cex.axis=1.5)
  text(2015,1.4*max(max(ci_yr_site_noc$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),site_locs$Site_name[site_locs$siteId==site_id[i]],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
  text(2010.3,1.4*max(max(ci_yr_site_noc$Upper),max(ci_yr_site$Upper),max(ci_yr_site_free$Upper),max(soildata[,colnames(soildata)==site_id[i]],na.rm =TRUE),max(mean_obs_12yr[soilgrids$Site_ID==site_id[i],]+1.96*sd_obs_12yr [soilgrids$Site_ID==site_id[i],]),na.rm =TRUE),label_id[k],col=colpft[pft$pft[pft$site==site_id[i]]],cex=2,font=2)
    }
  }
}
mtext(bquote("Total soil organic carbon (kg/"~m^2~")"), side = 2, line = 2, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,1, 1), mar = c(0.2, 4,2, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom', legend=c("Free run","LAI+AGB+SWC+SOC","LAI+AGB+SWC+SOC_spatial","SoilGrids data","Initial state","ISCN validation data"),lty=c(0,0,0,1,1,0),col=c(rgb(1,0.4,0.8,alpha=0.5),rgb(0,0.8,1,alpha=0.5),adjustcolor("#E69F00",alpha=0.5),"#009E73","black","blue"),pch=c(15,15,15,16,NA,2),lwd=c(0,0,0,2.5,2.5,2.5),cex=1.8,pt.cex=3,xpd = TRUE, ncol=2)



##############validation against ISCN
loca_iscn$soc[3]<-NA
iscn<-c()
for (i in seq_along(id_fc_uq)){
  
  iscn[i]<-as.numeric(loca_iscn$soc[as.character(loca_iscn$site_ID)==id_fc_uq[i]])
}

ci.yr_soc_loc_2021<-sda_soc_500km %>% filter(Variable=="TotSoilCarb",Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_soc_2021<-sda_soc_0km %>% filter(Variable=="TotSoilCarb",Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_2021<-sda_all_obs %>% filter(Variable=="TotSoilCarb",Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS") 
ci.yr_2021_nosoil_loc<- sda_no_soc %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_2021_nosoil<- sda_no_soc_0km %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
#ci.yr_2021_lai_agb<- sda_lai_agb %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_2021_free<- freerun %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_2021_noloc<- sda_all_obs_0km %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")
ci.yr_2021_soc2012<- sda_soc_2012 %>% filter(Variable=="TotSoilCarb", Date==as.Date("2021-07-15"),Type=="SDA_ANALYSIS")

forecast<-ci.yr_2021$Means    #mean of analysis with soilgrids
forecast_free<-ci.yr_2021_free$Means #mean of analysis of free run
forecast_nosoil<-ci.yr_2021_nosoil$Means  #mean of analysis without soilgrids
forecast_nosoilloc<-ci.yr_2021_nosoil_loc$Means  #mean of analysis without soilgrids
#forecast_lai_agb<-ci.yr_2021_lai_agb$Means
forecast_socloc<-ci.yr_soc_loc_2021$Means
forecast_soc<-ci.yr_soc_2021$Means 
forecast_noloc<-ci.yr_2021_noloc$Means
forecast_soc2012<-ci.yr_2021_soc2012$Means

soilgrids_sda_order <- soilgrids[match(id_fc_uq,soilgrids$Site_ID),]
soc_sg <- soilgrids_sda_order$Total_soilC_0.200cm

iscn_nona<-iscn[!(is.na(iscn))]
forecast_nona<-forecast[!(is.na(iscn))]
forecast_free_nona<-forecast_free[!(is.na(iscn))]
forecast_nosoil_nona<-forecast_nosoil[!(is.na(iscn))]
forecast_nosoilloc_nona<-forecast_nosoilloc[!(is.na(iscn))]
forecast_soc_nona<-forecast_soc[!(is.na(iscn))]
forecast_socloc_nona<-forecast_socloc[!(is.na(iscn))]
#forecast_lai_agb_nona<-forecast_lai_agb[!(is.na(iscn))]
forecast_noloc_nona<-forecast_noloc[!(is.na(iscn))]
forecast_soc2012_nona<-forecast_soc2012[!(is.na(iscn))]
id_fc_uq_nona<-id_fc_uq[!(is.na(iscn))]

bias_allobs <- -bias(iscn_nona,forecast_nona)
bias_free   <- -bias(iscn_nona,forecast_free_nona)
bias_nosoil<- -bias(iscn_nona,forecast_nosoil_nona)
bias_nosoilloc<- -bias(iscn_nona,forecast_nosoilloc_nona)
bias_noloc <- -bias(iscn_nona,forecast_noloc_nona)
bias_soc2012 <- -bias(iscn_nona,forecast_soc2012_nona)
bias_soc <- -bias(iscn_nona,forecast_soc_nona)
bias_socloc <- -bias(iscn_nona,forecast_socloc_nona)

bias_sg_allobs <- -bias(soc_sg,forecast)
bias_sg_free <- -bias(soc_sg,forecast_free)
bias_sg_nosoil <- -bias(soc_sg,forecast_nosoil)
bias_sg_nosoilloc <- -bias(soc_sg,forecast_nosoilloc)
bias_sg_socloc <- -bias(soc_sg,forecast_socloc)
bias_sg_soc <- -bias(soc_sg,forecast_soc)
bias_sg_noloc <- -bias(soc_sg,forecast_noloc)
bias_sg_soc2012 <- -bias(soc_sg,forecast_soc2012)


regres<-summary(lm(iscn_nona~forecast_nona))
regres_free<-summary(lm(iscn_nona~forecast_free_nona))
regres_nosoil<-summary(lm(iscn_nona~forecast_nosoil_nona))
regres_nosoilloc<-summary(lm(iscn_nona~forecast_nosoilloc_nona))
#regres_lai_agb<-summary(lm(iscn_nona~forecast_lai_agb_nona))
regres_noloc<-summary(lm(iscn_nona~forecast_noloc_nona))
regres_soc2012<-summary(lm(iscn_nona~forecast_soc2012_nona))
regres_soc <-summary(lm(iscn_nona~forecast_soc_nona))
regres_socloc<-summary(lm(iscn_nona~forecast_socloc_nona))

r2=regres$adj.r.squared
r2_free=regres_free$adj.r.squared
r2_nosoil=regres_nosoil$adj.r.squared
r2_nosoilloc=regres_nosoilloc$adj.r.squared
#r2_lai_agb=regres_lai_agb$adj.r.squared
r2_noloc=regres_noloc$adj.r.squared
r2_soc2012=regres_soc2012$adj.r.squared
r2_soc=regres_soc$adj.r.squared
r2_socloc=regres_socloc$adj.r.squared

pva<-regres$coefficients[2,4]
pva_free<-regres_free$coefficients[2,4]
pva_nosoil<-regres_nosoil$coefficients[2,4]
pva_nosoilloc<-regres_nosoilloc$coefficients[2,4]
#pva_lai_agb<-regres_lai_agb$coefficients[2,4]
pva_noloc<-regres_noloc$coefficients[2,4]
pva_soc2012<-regres_soc2012$coefficients[2,4]
pva_soc<-regres_soc$coefficients[2,4]
pva_socloc<-regres_socloc$coefficients[2,4]

rmse_sg_allobs <- rmse(soc_sg,forecast)
rmse_sg_free <- rmse(soc_sg,forecast_free)
rmse_sg_nosoil <- rmse(soc_sg,forecast_nosoil)
rmse_sg_nosoilloc <- rmse(soc_sg,forecast_nosoilloc)
rmse_sg_socloc <- rmse(soc_sg,forecast_socloc)
rmse_sg_soc <- rmse(soc_sg,forecast_soc)
rmse_sg_noloc <- rmse(soc_sg,forecast_noloc)
rmse_sg_soc2012 <- rmse(soc_sg,forecast_soc2012)

rmse<-rmse(iscn_nona,forecast_nona)
rmse_free<-rmse(iscn_nona,forecast_free_nona)
rmse_nosoil<-rmse(iscn_nona,forecast_nosoil_nona)
rmse_nosoilloc<-rmse(iscn_nona,forecast_nosoilloc_nona)
#rmse_lai_agb<-rmse(iscn_nona,forecast_lai_agb_nona)
rmse_noloc<-rmse(iscn_nona,forecast_noloc_nona)
rmse_soc2012<-rmse(iscn_nona,forecast_soc2012_nona)
rmse_soc<-rmse(iscn_nona,forecast_soc_nona)
rmse_socloc<-rmse(iscn_nona,forecast_socloc_nona)

rp = vector('expression',3)
rp[1] = substitute(expression(italic(R)^2 == r2_va), 
                       list(r2_va = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == pva_va), 
                       list(pva_va = format(pva, digits = 2)))[2]
rp[3] = paste("RMSE =", format(rmse, digits = 2))

rp_free = vector('expression',3)
rp_free[1] = substitute(expression(italic(R)^2 == r2_free_va), 
                   list(r2_free_va = format(r2_free,dig=3)))[2]
rp_free[2] = substitute(expression(italic(p) == pva_free_va), 
                   list(pva_free_va = format(pva_free, digits = 2)))[2]
rp_free[3] = paste("RMSE =", format(rmse_free, digits = 2))

rp_nosoil = vector('expression',3)
rp_nosoil[1] = substitute(expression(italic(R)^2 == r2_nosoil_va), 
                      list(r2_nosoil_va = format(r2_nosoil,dig=3)))[2]
rp_nosoil[2] = substitute(expression(italic(p) == pva_nosoil_va), 
                      list(pva_nosoil_va = format(pva_nosoil, digits = 2)))[2]
rp_nosoil[3] = paste("RMSE =", format(rmse_nosoil, digits = 2)) 

# rp_lai_agb = vector('expression',3)
# rp_lai_agb[1] = substitute(expression(italic(R)^2 == r2_lai_agb_va), 
#                           list(r2_lai_agb_va = format(r2_lai_agb,dig=3)))[2]
# rp_lai_agb[2] = substitute(expression(italic(p) == pva_lai_agb_va), 
#                           list(pva_lai_agb_va = format(pva_lai_agb, digits = 2)))[2]
# rp_lai_agb[3] = paste("RMSE =", format(rmse_lai_agb, digits = 2)) 

rp_noloc= vector('expression',3)
rp_noloc[1] = substitute(expression(italic(R)^2 == r2_noloc_va), 
                           list(r2_noloc_va = format(r2_noloc,dig=3)))[2]
rp_noloc[2] = substitute(expression(italic(p) == pva_noloc_va), 
                           list(pva_noloc_va = format(pva_noloc, digits = 2)))[2]
rp_noloc[3] = paste("RMSE =", format(rmse_noloc, digits = 2))

rp_soc2012= vector('expression',3)
rp_soc2012[1] = substitute(expression(italic(R)^2 == r2_soc2012_va), 
                         list(r2_soc2012_va = format(r2_soc2012,dig=3)))[2]
rp_soc2012[2] = substitute(expression(italic(p) == pva_soc2012_va), 
                         list(pva_soc2012_va = format(pva_soc2012, digits = 2)))[2]
rp_soc2012[3] = paste("RMSE =", format(rmse_soc2012, digits = 2)) 

par(mar=c(5, 5, 1, 1))
#plot(forecast_nosoil_nona,iscn_nona,pch=2,lwd=2,cex=1.8,col="#009E73",xlim=c(0,125),ylim=c(-11,130),xlab="",ylab="",xaxt = "n",yaxt = "n")
plot(forecast_nosoilloc_nona,iscn_nona,pch=2,lwd=2.2,cex=1.8,col=rgb(0.3, 0.7, 0.9),xlim=c(0,125),ylim=c(-11,130),xlab="",ylab="",xaxt = "n",yaxt = "n")
points(forecast_free_nona,iscn_nona,pch=0,lwd=2.2,cex=1.8,col="#FF0099")
points(forecast_nona,iscn_nona,pch=1,lwd=2.2,cex=1.8,col="#33CC33")
points(forecast_soc2012_nona,iscn_nona,pch=4,lwd=2,cex=1.8,col=rgb(0,0.8,1))
points(forecast_soc_nona,iscn_nona,pch=4,lwd=2,cex=1.8,col='blue')
points(forecast_socloc_nona,iscn_nona,pch=4,lwd=2,cex=1.8,col="#FF0099")
#points(forecast_lai_agb_nona,iscn_nona,pch=0,lwd=2,cex=1.2,col=rgb(1,0.2,0.2))

#abline(lm(iscn_nona~forecast_nosoil_nona),col="#009E73")
#abline(lm(iscn_nona~forecast_free_nona),col=rgb(1,0.4,0.8))
#abline(lm(iscn_nona~forecast_nona),col="#E69F00")
#abline(lm(iscn_nona~forecast_noloc_nona),col=rgb(0,0.8,1))

lines(seq(0,120,by=10), seq(0,120,by=10),type = "l", lwd=2,col="black",lty = 2)
legend(-5,52, legend = paste(rp[3],"\n","Bias=",format(bias_allobs,digits = 3)), text.col="#33CC33",bty = 'n',cex=2)
legend(30,42, legend = paste(rp_free[3], "\n","Bias=",format(bias_free,digits = 3)),text.col="#FF0099",bty = 'n',cex=2)
legend(64,39, legend = paste(rp_nosoil[3], "\n","Bias=",format(bias_nosoil,digits = 3)), text.col=rgb(0.3, 0.7, 0.9),bty = 'n',cex=2)
legend(9,4.8, legend = paste(rp_soc2012[3], "\n","Bias=",format(bias_soc2012,digits = 3)), text.col=rgb(0,0.8,1),bty = 'n',cex=2)
#legend(60,40, legend = rp_lai_agb[3], text.col=rgb(1,0.2,0.2),bty = 'n',cex=1.5)

#legend(55,100,legend=c("Free run", "LAI+AGB","LAI+AGB+SoilGirds"),pch=c(0,2,1),col=c(rgb(1,0.4,0.8),rgb(0,0.8,1),rgb(0.3,0.8,0.1)),cex=c(1,1,1),pt.cex=1.5,lty=0,lwd=2,bty="n")
legend(40,125,legend=c("Free run", "LAI+AGB+SWC","LAI+AGB+SWC+SOC"),pch=c(0,2,1),col=c("#FF0099",rgb(0.3, 0.7, 0.9),"#33CC33"),cex=1.8,pt.cex=1.8,lty=0,lwd=2,bty="n")
mtext("Reanalysis SOC (kg/m\U00B2)",side=1,line=3.5,cex=1.5)
mtext("ISCN SOC (kg/m\U00B2)",side=2,line=3.5,cex=1.5)
axis(1, at=seq(0,130,by=20),col="black",cex.axis=1.8)
axis(2, at=seq(0,130,by=20),col="black",cex.axis=1.8)
text(116,105,"1:1 line",cex=2,col="black")

################ for t-test of Analysis means and uncertainty

ci_yr_free<- freerun %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")
ci_yr_soc_500km<-sda_soc_500km %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS")
ci_yr_soc_0km<- sda_soc_0km %>% filter(Variable=="TotSoilCarb", Type=="SDA_ANALYSIS")
ci_yr_no_soc <- sda_no_soc %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")
ci_yr_no_soc_0km <- sda_no_soc_0km %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")
ci_yr_all_obs <- sda_all_obs %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")
ci_yr_all_obs_0km <-sda_all_obs_0km %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")
ci_yr_soc_2012 <- sda_soc_2012 %>% filter(Variable=="TotSoilCarb",Type=="SDA_ANALYSIS")

#######for Analysis means
soc_freerun <- ci_yr_free$Means
soc_500km <-ci_yr_soc_500km$Means
soc_0km <- ci_yr_soc_0km$Means
no_soc <- ci_yr_no_soc$Means
no_soc_0km <- ci_yr_no_soc_0km$Means
all_obs<- ci_yr_all_obs$Means
all_obs_0km <- ci_yr_all_obs_0km$Means
soc_2012  <- ci_yr_soc_2012$Means
soc_obs<-rep(soilgrids$Total_soilC_0.200cm,12)
my_data <- data.frame( 
  group = rep(c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs"), each = 390),
  weight = c(soc_freerun[79:468],no_soc_0km[79:468],no_soc[79:468],soc_0km[79:468], soc_500km[79:468],all_obs_0km[79:468],all_obs[79:468],soc_2012[79:468],soc_obs[79:468])
)

library("ggpubr")
library("ggplot2")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#009E73", "#CC79A7","#999999","#E69F00","#56B4E9","#D55E00","#0072B2","red","black"))+labs(y=bquote("Mean of SOC (kg/"~m^2~")"),x= "\nExperiments")+
  theme(axis.title = element_text(size = 15),axis.text=element_text(size = 13),legend.position="none")
#######violin plot
my_data %>%
  mutate( group=factor(group,levels=c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs"))) %>% ggplot(aes(x =group, y = weight))+geom_violin(trim=FALSE,aes(color=group),show.legend = F)+scale_color_manual("",values=c("#009E73", "#CC79A7","#999999","#E69F00","#56B4E9","#D55E00","#0072B2","red","black"))+stat_summary(fun.y=mean, geom="point",aes(shape = "mean"), size = 12, color="red")+labs(y=bquote("Mean of SOC (kg/"~m^2~")"),x= "\nExperiments")+scale_shape_manual("",values=95)+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text.x = element_text(size=15,angle = 45,hjust = 0.5, vjust = 0.65),axis.text.y = element_text(size=15),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15),legend.text = element_text(size=15),legend.position = c(0.9, 0.9),panel.border = element_blank(),axis.line = element_line(color = 'black'))


#pairwise.wilcox.test(x=my_data$weight,g=my_data$group,data = my_data,paired = TRUE,p.adj = "bonf")
pairwise.t.test(x=my_data$weight,g=my_data$group,data = my_data,paired = TRUE,p.adj = "bonf")

#res.aov <- aov(my_data$weight ~ my_data$group, data = my_data)
#summary(res.aov)

Effect_Size <- cohens_d(weight ~ group, data = my_data,paired = TRUE)
print(Effect_Size,n=36)

mean_freerun <- mean(my_data$weight[my_data$group == 'freerun'])
mean_no_SOC <- mean(my_data$weight[my_data$group == 'no_SOC'])
mean_no_SOC_s <- mean(my_data$weight[my_data$group == 'no_SOC_s'])
mean_SOC_only <- mean(my_data$weight[my_data$group == 'SOC_only'])
mean_SOC_only_s <- mean(my_data$weight[my_data$group == 'SOC_only_s'])
mean_all_obs <- mean(my_data$weight[my_data$group == 'all_obs'])
mean_all_obs_s <- mean(my_data$weight[my_data$group == 'all_obs_s'])
mean_1yrSOC_s <- mean(my_data$weight[my_data$group == '1yrSOC_s'])
mean_obs <- mean(my_data$weight[my_data$group == 'obs'])
mean_pair<-c(mean_freerun,mean_no_SOC,mean_no_SOC_s,mean_SOC_only,mean_SOC_only_s,mean_all_obs,mean_all_obs_s,mean_1yrSOC_s,mean_obs)

mean_difference <- outer(mean_pair,mean_pair, `-`)
colnames(mean_difference) <- c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs")
rownames(mean_difference) <- c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs")
write.csv(mean_difference,"/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/SOC_mean_diff.csv", row.names = FALSE)

###############for Analysis uncertainty
soc_freerun <- (ci_yr_free$Upper-ci_yr_free$Lower)/(1.96*2)
soc_500km <-(ci_yr_soc_500km$Upper-ci_yr_soc_500km$Lower)/(1.96*2)
soc_0km <- (ci_yr_soc_0km$Upper-ci_yr_soc_0km$Lower)/(1.96*2)
no_soc <- (ci_yr_no_soc$Upper-ci_yr_no_soc$Lower)/(1.96*2)
no_soc_0km <- (ci_yr_no_soc_0km$Upper-ci_yr_no_soc_0km$Lower)/(1.96*2)
all_obs<- (ci_yr_all_obs$Upper-ci_yr_all_obs$Lower)/(1.96*2)
all_obs_0km <- (ci_yr_all_obs_0km$Upper-ci_yr_all_obs_0km$Lower)/(1.96*2)
soc_2012  <- (ci_yr_soc_2012$Upper-ci_yr_soc_2012$Lower)/(1.96*2)
soc_obs_sd_12 <-rep(sd_obs,12)
soc_obs_sd <-rep(sd_one_yr,12)
my_data <- data.frame( 
  group = rep(c("freerun", "no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs","obs_inflated"), each = 390),
  weight = c(soc_freerun[79:468],no_soc_0km[79:468],no_soc[79:468],soc_0km[79:468], soc_500km[79:468],all_obs_0km[79:468],all_obs[79:468],soc_2012[79:468],soc_obs_sd[79:468],soc_obs_sd_12[79:468]))

library("ggpubr")
library("rstatix")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group",linetype = c(rep(1,9),2), palette = c("#009E73", "#CC79A7","#999999","#E69F00","#56B4E9","#D55E00","#0072B2","red","black","black"))+labs(y=bquote("Standard deviation of SOC (kg/"~m^2~")"),x= "\nExperiments")+
  theme(axis.title = element_text(size = 15),axis.text=element_text(size = 13),legend.position="none")
#######violin plot
my_data %>%
  mutate( group=factor(group,levels=c("freerun", "no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs","obs_inflated"))) %>% ggplot(aes(x =group, y = weight))+geom_violin(trim=FALSE,aes(color=group,linetype=group),show.legend = F)+scale_color_manual("",values=c("#009E73", "#CC79A7","#999999","#E69F00","#56B4E9","#D55E00","#0072B2","red","black","black"))+scale_linetype_manual("",values=c(rep(1,9),2))+stat_summary(fun.y=mean, geom="point",aes(shape = "mean"), size = 12, color="red")+labs(y=bquote("Standard deviation of SOC (kg/"~m^2~")"),x= "\nExperiments")+scale_shape_manual("",values=95)+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text.x = element_text(size=15,angle = 45,hjust = 0.5, vjust = 0.65),axis.text.y = element_text(size=15),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15),legend.text = element_text(size=15),legend.position = c(0.9, 0.9),panel.border = element_blank(),axis.line = element_line(color = 'black'))

pairwise.t.test(x=my_data$weight,g=my_data$group,data = my_data,paired = TRUE,p.adj = "bonf")
#pairwise.t.test(x=my_data$weight,g=my_data$group,data = my_data,paired = TRUE,alternative = "greater",p.adj = "bonf") #if p<0.05, "greater" is confirmed
Effect_Size <- cohens_d(weight ~ group, data = my_data,paired = TRUE)
print(Effect_Size,n=45)

std_freerun <- mean(my_data$weight[my_data$group == 'freerun'])
std_no_SOC <- mean(my_data$weight[my_data$group == 'no_SOC'])
std_no_SOC_s <- mean(my_data$weight[my_data$group == 'no_SOC_s'])
std_SOC_only <- mean(my_data$weight[my_data$group == 'SOC_only'])
std_SOC_only_s <- mean(my_data$weight[my_data$group == 'SOC_only_s'])
std_all_obs <- mean(my_data$weight[my_data$group == 'all_obs'])
std_all_obs_s <- mean(my_data$weight[my_data$group == 'all_obs_s'])
std_1yrSOC_s <- mean(my_data$weight[my_data$group == '1yrSOC_s'])
std_obs <- mean(my_data$weight[my_data$group == 'obs'])
std_obs_inf <- mean(my_data$weight[my_data$group == 'obs_inflated'])
std_pair<-c(std_freerun,std_no_SOC,std_no_SOC_s,std_SOC_only,std_SOC_only_s,std_all_obs,std_all_obs_s,std_1yrSOC_s,std_obs,std_obs_inf)

std_difference <- outer(std_pair,std_pair, `-`)
colnames(std_difference) <- c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs","obs_inf")
rownames(std_difference) <- c("freerun","no_SOC","no_SOC_s","SOC_only","SOC_only_s","all_obs","all_obs_s","1yrSOC_s","obs","obs_inf")
write.csv(std_difference,"/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/SOC_std_diff.csv", row.names = FALSE)


