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
########SOC q=0.5
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_q0.5.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 10)   #10 blocks of sites
bqq_soc<-vector("list", 10)
site_idb<-vector("list", 10)
for (b in seq_along(block.list.all[[t]])){
  site_idb[[b]]<-block.list.all[[t]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["aqq"]][4*s,] #aqq_soc is for SOC
    bqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["bqq"]][4*s,]
  }
}

aqq_q0.5<-unlist(aqq_soc, recursive = FALSE)
bqq_q0.5<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_q0.5[[i]][[b]], rate=bqq_q0.5[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_q0.5[[i]][[b]], rate=bqq_q0.5[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_q0.5 <- q_mean
q_std_q0.5 <- q_std


aqq_m_q0.5<-t(matrix(unlist(aqq_q0.5),nrow=39,byrow=TRUE))
bqq_m_q0.5<-t(matrix(unlist(bqq_q0.5),nrow=39,byrow=TRUE))
colnames(aqq_m_q0.5)<-site_id_39
colnames(bqq_m_q0.5)<-site_id_39


########SOC q=1
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_500km.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 10)   #10 blocks of sites
bqq_soc<-vector("list", 10)
site_idb<-vector("list", 10)
for (b in seq_along(block.list.all[[t]])){
  site_idb[[b]]<-block.list.all[[t]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["aqq"]][4*s,] #aqq_soc is for SOC
    bqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["bqq"]][4*s,]
  }
}

aqq_q1<-unlist(aqq_soc, recursive = FALSE)
bqq_q1<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_q1[[i]][[b]], rate=bqq_q1[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_q1[[i]][[b]], rate=bqq_q1[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_q1 <- q_mean
q_std_q1 <- q_std


aqq_m_q1<-t(matrix(unlist(aqq_q1),nrow=39,byrow=TRUE))
bqq_m_q1<-t(matrix(unlist(bqq_q1),nrow=39,byrow=TRUE))
colnames(aqq_m_q1)<-site_id_39
colnames(bqq_m_q1)<-site_id_39

########SOC q=2
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_q2.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 10)   #10 blocks of sites
bqq_soc<-vector("list", 10)
site_idb<-vector("list", 10)
for (b in seq_along(block.list.all[[t]])){
  site_idb[[b]]<-block.list.all[[t]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["aqq"]][4*s,] #aqq_soc is for SOC
    bqq_soc[[b]][[s]]<-block.list.all[[t]][[b]][["bqq"]][4*s,]
  }
}

aqq_q2<-unlist(aqq_soc, recursive = FALSE)
bqq_q2<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_q2[[i]][[b]], rate=bqq_q2[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_q2[[i]][[b]], rate=bqq_q2[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_q2 <- q_mean
q_std_q2 <- q_std

aqq_m_q2<-t(matrix(unlist(aqq_q2),nrow=39,byrow=TRUE))
bqq_m_q2<-t(matrix(unlist(bqq_q2),nrow=39,byrow=TRUE))
colnames(aqq_m_q2)<-site_id_39
colnames(bqq_m_q2)<-site_id_39

time<-seq(2010,2021,by=1)
label_id<-c(letters[1:26], paste0("a",letters[1:26]))
#plot aqq
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==colnames(aqq_m_q0.5)[i]]==unique(pft$pft)[j]){
      k=k+1
  plot(time,aqq_m_q0.5[1:12,i],type="l",col="#E69F00",ylim=c(0,8))
  lines(time,aqq_m_q1[1:12,i],col="red")
  lines(time,aqq_m_q2[1:12,i],col="blue")
  text(2016.5,7,site_locs$Site_name[site_locs$siteId==colnames(aqq_m_q0.5)[i]],col=colpft[pft$pft[pft$site==colnames(aqq_m_q0.5)[i]]],cex=1.5,font=2)
  text(2011.5,7,paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==colnames(aqq_m_q0.5)[i]]],cex=1.5,font=2)
   }
  }
}
mtext("Year", side = 1, line = 2,  outer = TRUE,cex=1.5)
mtext(bquote("Shape parameter"~alpha), side = 2, line = 2,  outer = TRUE,cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend("bottom", legend=c(bquote(alpha[i]~"=0.5"),bquote(alpha[i]~"=1"),bquote(alpha[i]~"=2")),lty=c(1,1,1),col=c("#E69F00","red","blue"),lwd=c(2,2,2),cex=1.8,pt.cex=3,xpd = TRUE, ncol=3)

############plot bqq
par(mai=c(.5,.6,.2,.2), mfrow = c(5, 8), oma = c(10, 5, 0.2, 0.2),mar = c(2, 2, 1, 1))
k=0
for (j in seq_along(unique(pft$pft))){
  for (i in 1:39){
    if (pft$pft[pft$site==colnames(aqq_m_q0.5)[i]]==unique(pft$pft)[j]){
      k=k+1
  plot(time,1/bqq_m_q0.5[1:12,i],type="l",col="#E69F00",ylim=c(0,4))
  lines(time,1/bqq_m_q1[1:12,i],col="red")
  lines(time,1/bqq_m_q2[1:12,i],col="blue")
  text(2016.5,3.5,site_locs$Site_name[site_locs$siteId==colnames(bqq_m_q0.5)[i]],col=colpft[pft$pft[pft$site==colnames(bqq_m_q0.5)[i]]],cex=1.5,font=2)
  text(2011.5,3.5,paste0("(",label_id[k],")"),col=colpft[pft$pft[pft$site==colnames(aqq_m_q0.5)[i]]],cex=1.5,font=2)
    }
  } 
}

mtext("Year", side = 1, line = 2,  outer = TRUE,cex=1.5)
mtext(bquote("Scale parameter"~1/beta), side = 2, line = 2,  outer = TRUE,cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend("bottom", legend=c(bquote(beta[i]~"=0.5"),bquote(beta[i]~"=1"),bquote(beta[i]~"=2")),lty=c(1,1,1),col=c("#E69F00","red","blue"),lwd=c(2,2,2),cex=1.8,pt.cex=3,xpd = TRUE, ncol=3)


