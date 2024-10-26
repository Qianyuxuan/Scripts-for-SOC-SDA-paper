rm(list = ls())
library(dplyr)
library(purrr)
library(paletteer)


load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/site_loc_id_name.Rdata")
site_locs<-site.locs
soilgrids<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/soilgrids_soilC_data_neon39sites.csv")

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/soilc_neon.Rdata") #neon data,initial value
ic_soc <- apply(soildata,2,quantile,c(0.025,0.5,0.975),na.rm=TRUE)

soilgrids<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/soilgrids_soilC_data_neon39sites.csv")
t=12

pft<-read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/site_pft.csv")
colpft<-c("#AA4499","#888888","#D55E00")
names(colpft)<-c("boreal.coniferous","temperate.deciduous.HPDA","semiarid.grassland_HPDA")
cols <- paletteer_d("ggsci::default_igv")[4:42]

load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_q0.5.Rdata") #aqq and bqq are all 2
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_soc_500km_q0.5 <- c('FORECAST','ANALYSIS')%>%
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


load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_q2.Rdata") #aqq and bqq are all 2
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_soc_500km_q2 <- c('FORECAST','ANALYSIS')%>%
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


load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_500km.Rdata") #aqq and bqq are all 1
All.my.data <- list(FORECAST=FORECAST,ANALYSIS=ANALYSIS)
site.ids <- attr(FORECAST[[1]], 'Site')
sda_soc_500km_q1 <- c('FORECAST','ANALYSIS')%>%
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

time <- seq(2010,2021,1)
id_fc_uq<-unique(site.ids)
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
###################plot
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(8, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==id_fc_uq[i]]==unique(pft$pft)[j]){
      k=k+1
      ci_yr_site_q0.5<-sda_soc_500km_q0.5 %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
      ci_yr_site_q1<-sda_soc_500km_q1 %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
      ci_yr_site_q2<- sda_soc_500km_q2 %>% filter(Variable=="TotSoilCarb", Site==id_fc_uq[i],Type=="SDA_ANALYSIS")
 
      rownames(ci_yr_site_q1) <- time
      plot(time,ci_yr_site_q0.5$Means,type="l",xaxt = "n",yaxt = "n",col="#E69F00",lwd=2,xlab="",ylab="",ylim=c(0,100)) #median
      polygon(c(time ,rev(time)),c(ci_yr_site_q0.5$Lower, rev(ci_yr_site_q0.5$Upper)),col= adjustcolor("#E69F00",alpha=0.5),border=NA)  #green SDA_obs
      
      lines(time,ci_yr_site_q1$Means,col="red",ylim=c(0,100),lwd=2) 
      polygon(c(time ,rev(time)),c(ci_yr_site_q1$Lower, rev(ci_yr_site_q1$Upper)),col= adjustcolor("red",alpha=0.5),border=NA)
      
      lines(time,ci_yr_site_q2$Means,col="blue",ylim=c(0,100),lwd=2) 
      polygon(c(time ,rev(time)),c(ci_yr_site_q2$Lower, rev(ci_yr_site_q2$Upper)),col= adjustcolor("blue",alpha=0.3),border=NA)  #pink,freerun
      text(2016.5,95,site_locs$Site_name[site_locs$siteId==id_fc_uq[i]],col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
      text(2011.5,95,paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==id_fc_uq[i]]],cex=1.5,font=2)
      axis(1, at = time,cex.axis=1.5)
      axis(2,cex.axis=1.5)
    }
  }
}  

mtext(bquote("SOC (kg/"~m^2~")"), side = 2, line = 1, outer = TRUE,cex=1.5)
mtext("Year", side = 1, line = 1.5, outer = TRUE,cex=1.5)

par(fig = c(0, 1,0, 1), oma = c(0, 5,0, 1), mar = c(0.2, 4,1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend("bottom", legend=c(bquote(alpha[i]~"=0.5, "~beta[i]~"=0.5"),bquote(alpha[i]~"=1, "~beta[i]~"=1"),bquote(alpha[i]~"=2, "~beta[i]~"=2")),lty=c(0,0,0),col=c(adjustcolor("#E69F00",alpha=0.5),adjustcolor("red",alpha=0.5),adjustcolor("blue",alpha=0.3)),pch=c(15,15,15),lwd=c(1,1,1),cex=1.8,pt.cex=3,xpd = TRUE, ncol=3)

###########
plot(density(rgamma(1e5,shape=1, rate=1)),col="red",ylim=c(0,2),xlab="Q")
lines(density(rgamma(1e5,shape=0.5, rate=0.5)),col="#E69F00")
lines(density(rgamma(1e5,shape=2, rate=2)),col="blue")
legend("topright",legend=c("aqq=0.5,bqq=0.5","aqq=1,bqq=1","aqq=2,bqq=2"),lty=c(0,0,0),col=c(adjustcolor("#E69F00",alpha=0.5),adjustcolor("red",alpha=0.5),adjustcolor("blue",alpha=0.3)),pch=c(15,15,15),lwd=c(1,1,1),cex=1.8,pt.cex=3,xpd = TRUE, ncol=3)

median(rgamma(1e5,shape=1, rate=1))
median(rgamma(1e5,shape=2, rate=2))

sd(rgamma(1e5,shape=1, rate=1))
sd(rgamma(1e5,shape=2, rate=2))