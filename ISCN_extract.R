rm(list = ls())
library(readxl)
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output/39_neon_sites_list_new.Rdata")
iscnf<-read_excel("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/ISCN/ISCN_SOC-DATA_PROFILE_1-1.xlsx")
soc_us <- iscnf$`soc (g cm-2)`[iscnf$`country (country)`=="United States"]
lat_us <- iscnf$`lat (dec. deg)`[iscnf$`country (country)`=="United States"]
long_us <- iscnf$`long (dec. deg)`[iscnf$`country (country)`=="United States"]
depth<-  iscnf$`soc_depth (cm)`[iscnf$`country (country)`=="United States"]
date <- iscnf$`observation_date (YYYY-MM-DD)`[iscnf$`country (country)`=="United States"]


long_lat_idx <- rep(NA, 39)
lon_neon <- as.numeric(site_list_new$lon)
lat_neon <- as.numeric(site_list_new$lat)
loca_neon <-as.data.frame(cbind(lon_neon,lat_neon))
colnames(loca_neon)<-c("long","lat")

for (i in 1:39) {
  long_lat_idx[i] <- which.min(sqrt((long_us  - lon_neon[i])^2+(lat_us - lat_neon[i])^2))
}

soc_iscn<-soc_us[long_lat_idx]*10 #change the unit from g/cm2 to kg/m2
date_iscn<-date[long_lat_idx]
year <- as.numeric(format(date_iscn,'%Y'))
depth_neon<-depth[long_lat_idx]
loca_iscn<-as.data.frame(cbind(as.numeric(long_us[long_lat_idx]),as.numeric(lat_us[long_lat_idx]),lon_neon,lat_neon,as.character(as.numeric(site_list_new$id)),year,soc_iscn,depth_neon))
colnames(loca_iscn)<-c("long_iscn","lat_iscn","lon_neon","lat_neon","site_ID","year","soc","depth")
save(loca_iscn,file="/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output/iscn_soc.Rdata")

