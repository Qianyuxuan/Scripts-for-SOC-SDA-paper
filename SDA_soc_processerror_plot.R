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

cols <- paletteer_d("ggsci::default_igv")[4:42]
########SOC only_500km
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

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_soc_500km <- q_mean
q_std_soc_500km <- q_std


#plot a nd b
aqq_m<-t(matrix(unlist(aqq_soc_39),nrow=39,byrow=TRUE))
bqq_m<-t(matrix(unlist(bqq_soc_39),nrow=39,byrow=TRUE))
colnames(aqq_m)<-site_id_39
colnames(bqq_m)<-site_id_39

par(mfrow = c(1, 2),
    mar = c(6, 6, 1, 0),
    oma = c(3,1, 1, 1))

matplot(2010:2021,aqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,8),cex.main=2) #SOC_only_s
text(2020,6,site_locs$Site_name[names(which(aqq_m[12,]>4))==site_locs$siteId],col=cols[which(aqq_m[13,]>4)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,7.7,"(a)",cex=1.5)
mtext(bquote("Shape parameter"~alpha), side = 2, line = 3, cex=1.5)

matplot(2010:2021,1/bqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,cex.main=2)
text(2020,0.15,site_locs$Site_name[names(which(1/bqq_m[12,]<0.6))==site_locs$siteId],col=cols[which(1/bqq_m[12,]<0.6)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,1.9,"(b)",cex=1.5)
mtext(bquote("Scale parameter"~1/beta), side = 2, line = 3, cex=1.5)

par(fig = c(0,1,0.2,0.4), oma = c(3,1, 2, 1),mar = c(1, 4,1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
mtext("Year", side = 1, line = 3, cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom',col=cols,lty=1:6,lwd=2, site_locs[match(colnames(aqq_m),site_locs$siteId),]$Site_name,ncol=13,cex=0.7)

########SOC only_0km
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_0km.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 39)
bqq_soc<-vector("list", 39)
site_idb<-vector("list",39)
for (b in seq_along(block.list.all[[12]])){
  site_idb[[b]]<-block.list.all[[12]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["aqq"]][4*s,]
    bqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["bqq"]][4*s,]
  }
}

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_soc_0km <- q_mean
q_std_soc_0km <- q_std
############39 sites all obs
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_allobs_500km.Rdata")
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

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_allobs_500km <- q_mean
q_std_allobs_500km <- q_std

#plot a nd b
aqq_m<-t(matrix(unlist(aqq_soc_39),nrow=39,byrow=TRUE))
bqq_m<-t(matrix(unlist(bqq_soc_39),nrow=39,byrow=TRUE))
colnames(aqq_m)<-site_id_39
colnames(bqq_m)<-site_id_39

par(mfrow = c(1, 2),
    mar = c(6, 6, 1, 0),
    oma = c(3,1, 1, 1))

matplot(2010:2021,aqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,8),cex.main=2) #SOC_only_s
text(2020,6.7,site_locs$Site_name[names(which(aqq_m[12,]>4))==site_locs$siteId],col=cols[which(aqq_m[12,]>4)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,7.7,"(a)",cex=1.5)
mtext(bquote("Shape parameter"~alpha), side = 2, line = 3, cex=1.5)

matplot(2010:2021,1/bqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,2),cex.main=2)
text(2020,0.15,site_locs$Site_name[names(which(1/bqq_m[12,]<0.6))==site_locs$siteId],col=cols[which(1/bqq_m[12,]<0.6)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,1.9,"(b)",cex=1.5)
mtext(bquote("Scale parameter"~1/beta), side = 2, line = 3, cex=1.5)

par(fig = c(0,1,0.2,0.4), oma = c(3,1, 2, 1),mar = c(1, 4,1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
mtext("Year", side = 1, line = 3, cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom',col=cols,lty=1:6,lwd=2, site_locs[match(colnames(aqq_m),site_locs$siteId),]$Site_name,ncol=13,cex=0.7)


##############39 sites no corr
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_allobs_0km.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 39)
bqq_soc<-vector("list", 39)
site_idb<-vector("list",39)
for (b in seq_along(block.list.all[[12]])){
  site_idb[[b]]<-block.list.all[[12]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["aqq"]][4*s,]
    bqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["bqq"]][4*s,]
  }
}

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_allobs_0km <- q_mean
q_std_allobs_0km <- q_std

#################################
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_nosoc_500km.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 10)
bqq_soc<-vector("list", 10)
site_idb<-vector("list", 10)
for (b in seq_along(block.list.all[[12]])){
  site_idb[[b]]<-block.list.all[[12]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["aqq"]][4*s,]
    bqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["bqq"]][4*s,]
  }
}

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_nosoc <- q_mean
q_std_nosoc <- q_std

#plot a nd b
aqq_m<-t(matrix(unlist(aqq_soc_39),nrow=39,byrow=TRUE))
bqq_m<-t(matrix(unlist(bqq_soc_39),nrow=39,byrow=TRUE))
colnames(aqq_m)<-site_id_39
colnames(bqq_m)<-site_id_39

par(mfrow = c(1, 2),
    mar = c(6, 6, 1, 0),
    oma = c(3,1, 1, 1))

matplot(2010:2021,aqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,8),cex.main=2) #SOC_only_s
#text(2020,7,site_locs$Site_name[names(which(aqq_m[12,]>4))==site_locs$siteId],col=cols[which(aqq_m[13,]>4)],cex=2)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,7.7,"(a)",cex=1.5)
mtext(bquote("Shape parameter"~alpha), side = 2, line = 3, cex=1.5)

matplot(2010:2021,1/bqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,2),cex.main=2)
#text(2020,0.2,site_locs$Site_name[names(which(1/bqq_m[12,]<0.6))==site_locs$siteId],col=cols[which(1/bqq_m[12,]<0.6)],cex=2)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,1.9,"(b)",cex=1.5)
mtext(bquote("Scale parameter"~1/beta), side = 2, line = 3, cex=1.5)

par(fig = c(0,1,0.2,0.4), oma = c(3,1, 2, 1),mar = c(1, 4,1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
mtext("Year", side = 1, line = 3, cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom',col=cols,lty=1:6,lwd=2, site_locs[match(colnames(aqq_m),site_locs$siteId),]$Site_name,ncol=13,cex=0.7)

############
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiments_new/sda.output_SOC_2012_500km.Rdata")
block.list.all<- enkf.params[[t]]$block.list.all
aqq_soc<-vector("list", 10)
bqq_soc<-vector("list", 10)
site_idb<-vector("list", 10)
for (b in seq_along(block.list.all[[12]])){
  site_idb[[b]]<-block.list.all[[12]][[b]]$site.ids
  for (s in seq_along(site_idb[[b]])){
    aqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["aqq"]][4*s,]
    bqq_soc[[b]][[s]]<-block.list.all[[12]][[b]][["bqq"]][4*s,]
  }
}

aqq_soc_39<-unlist(aqq_soc, recursive = FALSE)
bqq_soc_39<-unlist(bqq_soc, recursive = FALSE)
site_id_39<-unlist(site_idb,recursive = FALSE)
q_mean<-matrix(NA,nrow=12,ncol=39)
q_std<-matrix(NA,nrow=12,ncol=39)
for (i in 1:39){
  for (b in 1:12){
    q_mean[b,i]<-median(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
    q_std[b,i]<-sd(rgamma(1e5,shape=aqq_soc_39[[i]][[b]], rate=bqq_soc_39[[i]][[b]]))
  }
}
colnames(q_mean)<-site_id_39
colnames(q_std)<-site_id_39
q_mean_2012 <- q_mean
q_std_2012 <- q_std

#plot a nd b
aqq_m<-t(matrix(unlist(aqq_soc_39),nrow=39,byrow=TRUE))
bqq_m<-t(matrix(unlist(bqq_soc_39),nrow=39,byrow=TRUE))
colnames(aqq_m)<-site_id_39
colnames(bqq_m)<-site_id_39

par(mfrow = c(1, 2),
    mar = c(6, 6, 1, 0),
    oma = c(3,1, 1, 1))

matplot(2010:2021,aqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,8),cex.main=2) #SOC_2012
text(2016,1.7,site_locs$Site_name[names(which(aqq_m[12,]>1.38 & aqq_m[12,]<1.4))==site_locs$siteId],col=cols[which(aqq_m[12,]>1.38 & aqq_m[12,]<1.4)],cex=1.5)
text(2019,1.7,site_locs$Site_name[names(which(aqq_m[12,]>1.3 & aqq_m[12,]<1.33))==site_locs$siteId],col=cols[which(aqq_m[12,]>1.3 & aqq_m[12,]<1.33)],cex=1.5)
text(2013,-0.2,site_locs$Site_name[names(which(aqq_m[12,]>0.4 & aqq_m[12,]<0.6))==site_locs$siteId],col=cols[which(aqq_m[12,]>0.4 & aqq_m[12,]<0.6)],cex=1.5)
text(2016,-0.2,site_locs$Site_name[names(which(aqq_m[12,]>0.25 & aqq_m[12,]<0.28))==site_locs$siteId],col=cols[which(aqq_m[12,]>0.25 & aqq_m[12,]<0.28)],cex=1.5)
text(2019,-0.2,site_locs$Site_name[names(which(aqq_m[12,]>0.2 & aqq_m[12,]<0.25))==site_locs$siteId],col=cols[which(aqq_m[12,]>0.2 & aqq_m[12,]<0.25)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,7.7,"(a)",cex=1.5)
mtext(bquote("Shape parameter"~alpha), side = 2, line = 3, cex=1.5)

matplot(2010:2021,1/bqq_m[1:12,],type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,2),cex.main=2)
text(2020,1.4,site_locs$Site_name[names(which(1/bqq_m[12,]>1.2 & 1/bqq_m[12,]<1.4))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>1.2 & 1/bqq_m[12,]<1.4)],cex=1.5)
text(2020,0.6,site_locs$Site_name[names(which(1/bqq_m[12,]>0.6 & 1/bqq_m[12,]<0.8))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.6 & 1/bqq_m[12,]<0.8)],cex=1.5)
text(2020,0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.2 & 1/bqq_m[12,]<0.4))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.2 & 1/bqq_m[12,]<0.4)],cex=1.5)
text(2012,-0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.0002 & 1/bqq_m[12,]<0.00022))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.0002 & 1/bqq_m[12,]<0.00022)],cex=1.5)
text(2014,-0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.0006 & 1/bqq_m[12,]<0.00067))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.0006 & 1/bqq_m[12,]<0.00067)],cex=1.5)
text(2016,-0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.00333 & 1/bqq_m[12,]<0.00334))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.00333 & 1/bqq_m[12,]<0.00334)],cex=1.5)
text(2018,-0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.00339 & 1/bqq_m[12,]<0.0034))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.00339 & 1/bqq_m[12,]<0.0034)],cex=1.5)
text(2020,-0.17,site_locs$Site_name[names(which(1/bqq_m[12,]>0.0051 & 1/bqq_m[12,]<0.0052))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.0051 & 1/bqq_m[12,]<0.0052)],cex=1.5)
text(2020,-0.4,site_locs$Site_name[names(which(1/bqq_m[12,]>0.0052 & 1/bqq_m[12,]<0.0053))==site_locs$siteId],col=cols[which(1/bqq_m[12,]>0.0052 & 1/bqq_m[12,]<0.0053)],cex=1.5)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5) 
text(2010,1.9,"(b)",cex=1.5)
mtext(bquote("Scale parameter"~1/beta), side = 2, line = 3, cex=1.5)

par(fig = c(0,1,0.2,0.4), oma = c(3,1, 2, 1),mar = c(1, 4,1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
mtext("Year", side = 1, line = 3, cex=1.5)

par(fig = c(0,1,0,0.2),mar = c(0, 4,0, 1),oma = c(1,1, 1, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom',col=cols,lty=1:6,lwd=2, site_locs[match(colnames(aqq_m),site_locs$siteId),]$Site_name,ncol=13,cex=0.7)


par(mfrow = c(1, 4),
    mar = c(6.5, 0, 2, 0),
    oma = c(5,5, 1, 1))

###no SOC
std_nosoc<- 1/sqrt(q_mean_nosoc)
matplot(2010:2021,std_nosoc,type="l",xlab="",ylab="",col=cols,lwd=2,ylim=c(-1,75),lty=1:6,yaxt="n",xaxt="n",main="no_SOC_s",cex.main=2)
axis(2,cex.axis=1.5) 
axis(1,cex.axis=1.5)
mtext(bquote("Process error for SOC (kg/"~m^2~")"), side = 2, line = 3, cex=1.2)
text(2010.5,74,"(a)",cex=2)

#####SOC only
std_soc<- 1/sqrt(q_mean_soc_500km)
matplot(2010:2021,std_soc,type="l",xlab="",ylab="",yaxt="n",xaxt="n",col=cols,lwd=2,ylim=c(-1,75),lty=1:6,yaxt="n",xaxt="n",main="SOC_only_s",cex.main=2)
axis(2,labels = FALSE,cex.axis=1.5) 
axis(1,cex.axis=1.5)
text(2020,33,site_locs$Site_name[names(which(std_soc[2,]>65))==site_locs$siteId],col=cols[which(std_soc[2,]>65)],cex=2)
text(2010.5,74,"(b)",cex=2)

#######SOC 2012
std_2012<- 1/sqrt(q_mean_2012)
matplot(2010:2021,std_2012,type="l",xlab="",ylab="",col=cols,yaxt="n",xaxt="n",lty=1:6,lwd=2,ylim=c(-1,75),main="1yrSOC_s",cex.main=2)
text(2020,5,site_locs$Site_name[names(which(std_2012[4,]>7.4&std_2012[4,]<7.5))==site_locs$siteId],col=cols[which(std_2012[4,]>7.4&std_2012[4,]<7.5)],cex=2)
text(2020,10,site_locs$Site_name[names(which(std_2012[4,]>7.8&std_2012[4,]<7.9))==site_locs$siteId],col=cols[which(std_2012[4,]>7.8&std_2012[4,]<7.9)],cex=2)
text(2020,17,site_locs$Site_name[names(which(std_2012[4,]>15&std_2012[4,]<16))==site_locs$siteId],col=cols[which(std_2012[4,]>15&std_2012[4,]<16)],cex=2)
text(2020,21,site_locs$Site_name[names(which(std_2012[4,]>18.6&std_2012[4,]<18.7))==site_locs$siteId],col=cols[which(std_2012[4,]>18.6&std_2012[4,]<18.7)],cex=2)
text(2020,24,site_locs$Site_name[names(which(std_2012[4,]>18.8&std_2012[4,]<18.9))==site_locs$siteId],col=cols[which(std_2012[4,]>18.8&std_2012[4,]<18.9)],cex=2)
text(2020,33,site_locs$Site_name[names(which(std_2012[4,]>35.8&std_2012[4,]<35.9))==site_locs$siteId],col=cols[which(std_2012[4,]>35.8&std_2012[4,]<35.9)],cex=2)
text(2020,44,site_locs$Site_name[names(which(std_2012[4,]>38.6&std_2012[4,]<38.7))==site_locs$siteId],col=cols[which(std_2012[4,]>38.6&std_2012[4,]<38.7)],cex=2)
text(2020,69,site_locs$Site_name[names(which(std_2012[4,]>66))==site_locs$siteId],col=cols[which(std_2012[4,]>66)],cex=2)
axis(1,cex.axis=1.5)
axis(2,labels = FALSE,cex.axis=1.5) 
text(2010.5,74,"(c)",cex=2)

######all obs
std_allobs_500km <- 1/sqrt(q_mean_allobs_500km)
matplot(2010:2021,std_allobs_500km,type="l",xlab="",ylab="",col=cols,lty=1:6,lwd=2,ylim=c(-1,75),yaxt="n",xaxt="n",main="all_obs_s",cex.main=2)
text(2020,33,site_locs$Site_name[names(which(std_allobs_500km[2,]>64))==site_locs$siteId],col=cols[which(std_allobs_500km[2,]>64)],cex=2)
axis(1,cex.axis=1.5)
axis(2,labels = FALSE,cex.axis=1.5) 
text(2010.5,74,"(d)",cex=2)

par(fig = c(0,1,0.2,0.3), oma = c(0, 1,0, 1), mar = c(0, 4,0, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
mtext("Year", side = 1, line = 1.5, cex=1.2)

par(fig = c(0,1,0,0.2), oma = c(0.3, 1,0, 0.5), mar = c(0, 4,0, 1), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n',xlab="",ylab="",col=NA)
legend('bottom',col=cols,lty=1:6,lwd=2, site_locs[match(colnames(std_2012),site_locs$siteId),]$Site_name,ncol=13,cex=1.3)

