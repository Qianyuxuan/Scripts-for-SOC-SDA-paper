rm(list = ls())
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(cowplot)
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/site_loc_id_name.Rdata")
site_locs <- site.locs
pft <- read.csv("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/site_pft.csv")
load("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/results/output_new/sda_output/experiment_09/sda.output_soc_500km.Rdata")
site_list<-enkf.params[["2010-07-15"]][["block.list.all"]][[1]]
for (i in seq_along(site_list)){
  block_ids <-site_list[[i]]$site.ids
  for (j in seq_along(block_ids)){
  site_locs$group[block_ids[j] == site_locs$siteId] <- i 
}
}

site_ids <- site_locs$siteId
site_locs <- cbind(as.numeric(site_locs$Lon),as.numeric(site_locs$Lat),site_locs$group,site_locs$Site_name) %>%
  `colnames<-`(c("Lon","Lat","group","name")) %>%
  `rownames<-`(site_ids)
site_locs <- as.data.frame(site_locs)

site_locs$Lon <- abs(as.numeric(site_locs$Lon))
site_locs$Lat <- as.numeric(site_locs$Lat)
site_locs$group <- factor(site_locs$group,levels=1:10)
site_locs$PFT <- NA
for (i in 1:39) {
  ind <- which(pft$site == rownames(site.locs)[i])
  if (pft$pft[ind] == "semiarid.grassland_HPDA") {
    site_locs$PFT[i] <- "1"
  } else if (pft$pft[ind] == "boreal.coniferous") {
    site_locs$PFT[i] <- "3"
  } else if (pft$pft[ind] == "temperate.deciduous.HPDA") {
    site_locs$PFT[i] <- "2"
  }
}
# site.locs$siteId <- site.ids
level_order <- c("Grassland", "Deciduous Forest", "Coniferous Forest")

# coords <- st_coordinates(aoi_boundary_HARV)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
 "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2","darkturquoise", # lt orange
  "gray70",  "orchid1",
  "maroon", "khaki2", "deeppink1", "blue1", "steelblue4",
 "#FDBF6F", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


neon_shp <- st_read("/Users/qianyu/Library/CloudStorage/OneDrive-Personal/OneDrive\ -\ Brookhaven\ National\ Laboratory/NASA_CMS/data/NEON_shp/NEON_Domains.shp")
shppft<-c(16, 17, 5)

map.plot <- ggplot() +
  geom_sf(aes(fill=DomainName),data = neon_shp, alpha=0.35,lwd=0.2,color="black")+
  xlim(125, 65) +
  ylim(25, 50) +
  ggrepel::geom_label_repel(
    data = site_locs,
    aes(
      x = Lon,
      y = Lat,
      label = name,
      color = group
    ),
    vjust = 1,
    fontface = "bold",
    size = 3,
    show.legend = FALSE
  ) +
  geom_point(data = site_locs,
             aes(x = Lon, y = Lat,color = group,shape=PFT),
             size = 2,stroke=1) +
  scale_fill_manual(values = c("#A6CEE3",
                               "#1F78B4","#B2DF8A",
                               "#33A02C","#FB9A99",
                               "#E31A1C","#FDBF6F",
                               "#FF7F00","#CAB2D6",
                               "#6A3D9A","#FFFF99",
                               "#B15928","#FCCDE5",
                               "#D9D9D9","#66C2A5",
                               "#FFD92F","#8DD3C7",
                               "#80B1D3","#D9D9D9",
                               "#FDBF6F"),name="Eco-Region")+
  
  #coord_sf(datum = sf::st_crs(2163),default = F)+
  scale_color_manual(name="Site groups",values= c25[3:12])+
  scale_shape_manual(name="PFTs",labels = c("Grassland","Deciduous Forest","Coniferous Forest"),values=shppft)+
  theme(axis.text = element_blank())+
  theme_minimal()+
  theme(legend.position="none")+
  theme(plot.margin = unit(c(1, 1, 0, 5.5), units = "pt"))
  
 legend_climate <- ggplot() +
   geom_sf(aes(fill=DomainName),data = neon_shp, alpha=0.35,lwd=0.2,color="black")+
   xlim(125, 65) +
   ylim(25, 53) +
   scale_fill_manual(values = c("#A6CEE3",
                                "#1F78B4","#B2DF8A",
                                "#33A02C","#FB9A99",
                                "#E31A1C","#FDBF6F",
                                "#FF7F00","#CAB2D6",
                                "#6A3D9A","#FFFF99",
                                "#B15928","#FCCDE5",
                                "#D9D9D9","#66C2A5",
                                "#FFD92F","#8DD3C7",
                                "#80B1D3","#D9D9D9",
                                "#FDBF6F"),name="Eco-Region")+
   guides(fill=guide_legend(ncol=2))
 
 legend_group <-  ggplot() +  ggrepel::geom_label_repel(
   data = site_locs,
   aes(
     x = Lon,
     y = Lat,
     label = name,
     color = group
   ),
   vjust = 1,
   fontface = "bold",
   size = 3,
   show.legend = FALSE
 )+geom_point(data = site_locs,
              aes(x = Lon, y = Lat,color = group),
              size = 2,stroke=1) +
   scale_color_manual(name="Site groups",values= c25[3:12])+
   guides(color=guide_legend(ncol=2))

legend_pft <- ggplot() +
  geom_point(data = site_locs,
             aes(x = Lon, y = Lat,shape=PFT),
             size = 2,stroke=1) +
  scale_shape_manual(name="PFTs",labels = c("Grassland","Deciduous Forest","Coniferous Forest"),values=shppft)
 
plot_grid(
  map.plot,plot_grid(
  plot_grid(
    get_legend(legend_group)
    , get_legend(legend_pft)
    , nrow = 1)
    , get_legend(legend_climate), nrow =2,rel_heights = c(1,3))
  , ncol = 2,
   rel_widths = c(2,1)
)

pdf("~/site.PDF")
print(map.plot)
dev.off()
save(site.locs, file="~/site.locs.Rdata")
