getwd()
.libPaths()
.libPaths("/Users/batool/Documents/PhD_GlobeWQ/RPackages")

##Import libraries

library(raster)
library(readxl)
library(xlsx)
library(ggplot)
library(tidyverse)
library(maptools)
library(Matrix)
library(readxl)
library(writexl)
library(zoo)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(rgdal)
library(sf)
#make sure that the history is empty 
rm(list=ls())
.rs.restartR()

rm(list=ls())


#### Create maps with ggplot2 ####

### Import shapefiles of countries ####
country <- readOGR("This_study/crop_production/Inputs/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

## Import raster datasets and convert them into dataframe ###
A_1995 <- raster("This_study/crop_production/Inputs/Ray_et_al_2012/Wheat/Wheat_1995_Area.tif") %>%
  as.data.frame(., xy=T) 
A_2000 <- raster("This_study/crop_production/Inputs/Ray_et_al_2012/Wheat/Wheat_2000_Area.tif") %>%
  as.data.frame(., xy=T) 
A_2005 <- raster("This_study/crop_production/Inputs/Ray_et_al_2012/Wheat/Wheat_2005_Area.tif") %>%
  as.data.frame(., xy=T) 
A_Avg <- raster("This_study/crop_production/Outputs/Step_2/ARay_mean_Wheat.tif") %>%
  as.data.frame(., xy=T)
EU_A_Avg <- raster("This_study/crop_production/Outputs/Step_2/EU_ARay_mean_Wheat.tif") %>%
  as.data.frame(., xy=T)



#### Plot data  with classes###

p <- ggplot(df, aes(y=Latitude, x=Longitude)) +
  geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,10,20,30,40,50,Inf),
                         labels = c("<10", "10-20", "20-30", "30-40", "40-50", ">50")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_1990 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2000 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  scale_fill_manual(values = c("<10" = "#999933", "10-20" = "#44AA99",
                               "20-30" = "#661100", "30-40" = "#332288","40-50" ="#009E73" ,">50" = "#E69F00"),
                    limits = c("<10", "10-20", "20-30", "30-40", "40-50", ">50")) +
  geom_polygon(data=country, aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.25)+
  labs(title = paste0("Wheat production in 2010 (10^3 tonne/yr)")) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        axis.line = element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        legend.position="right",
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0,margin = unit(c(-15,0,0,0), "mm")),
        plot.margin = margin(0, 0, 0, 0, "cm")
  ) +
  scale_y_continuous(limits = c(25.99995, 70)) +
  scale_x_continuous(limits = c(-27, 40))+
  coord_equal() +
  theme(axis.text.x = element_text(face = "bold",colour = "black", size = 24, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(face = "bold",colour = "black", size = 24),
        plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 25),
        strip.text = element_text(face = "bold", hjust = 0.15, size = 14),
        axis.title.y = element_text(face="bold",size = 24),
        axis.title.x = element_text(face="bold",size = 24),
        panel.grid = element_blank() ,
        legend.position = "right",
        legend.text = element_text(face="bold",size = 22),
        legend.title= element_blank()) +
  theme(legend.key.width=unit(2, "cm"))+
  ggsave("This_study/crop_production/Plots/Wheat_P_2010.png", width=20, height=15)



### Join all dataframes (It depends on type of dataset###
df <- inner_join(P_1965, P_1990, by = c("x", "y")) %>%
  inner_join(., P_2000, by = c("x", "y")) %>%
  inner_join(., P_2010, by = c("x", "y")) %>%
  mutate(Wheat_P_tonne_1965 = Wheat_P_tonne_1965/10^3 ) %>%
  mutate(Wheat_P_tonne_1990 = Wheat_P_tonne_1990/10^3 ) %>%
  mutate(Wheat_P_tonne_2000 = Wheat_P_tonne_2000/10^3 ) %>%
  mutate(Wheat_P_tonne_2010 = Wheat_P_tonne_2010/10^3 ) 

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "Wheat_P_tonne_1965", 
                  "Wheat_P_tonne_1990", "Wheat_P_tonne_2000", "Wheat_P_tonne_2010")



#### Plot data  with classes###

p <- ggplot(df, aes(y=Latitude, x=Longitude)) +
  geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,10,20,30,40,50,Inf),
        labels = c("<10", "10-20", "20-30", "30-40", "40-50", ">50")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_1990 ,breaks=c(0,20,40,80,Inf),
                         #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2000 ,breaks=c(0,20,40,80,Inf),
                         #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,20,40,80,Inf),
                         #labels = c("<20", "20-40", "40-80", ">80")))) +
  scale_fill_manual(values = c("<10" = "#999933", "10-20" = "#44AA99",
   "20-30" = "#661100", "30-40" = "#332288","40-50" ="#009E73" ,">50" = "#E69F00"),
  limits = c("<10", "10-20", "20-30", "30-40", "40-50", ">50")) +
  geom_polygon(data=country, aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.25)+
  labs(title = paste0("Wheat production in 2010 (10^3 tonne/yr)")) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        axis.line = element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        legend.position="right",
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0,margin = unit(c(-15,0,0,0), "mm")),
        plot.margin = margin(0, 0, 0, 0, "cm")
  ) +
  scale_y_continuous(limits = c(25.99995, 70)) +
  scale_x_continuous(limits = c(-27, 40))+
  coord_equal() +
theme(axis.text.x = element_text(face = "bold",colour = "black", size = 24, angle = 90, hjust = 0.5, vjust = 0.5),
      axis.text.y = element_text(face = "bold",colour = "black", size = 24),
      plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 25),
      strip.text = element_text(face = "bold", hjust = 0.15, size = 14),
      axis.title.y = element_text(face="bold",size = 24),
      axis.title.x = element_text(face="bold",size = 24),
      panel.grid = element_blank() ,
      legend.position = "right",
      legend.text = element_text(face="bold",size = 22),
      legend.title= element_blank()) +
  theme(legend.key.width=unit(2, "cm"))+
  ggsave("This_study/crop_production/Plots/Wheat_P_2010.png", width=20, height=15)







###### Orignal map ####
ggplot(df, aes(y=Latitude, x=Longitude)) +
  #geom_raster(aes(fill=Wheat_P_tonne_1965)) +
  geom_tile(aes(fill=cut(Wheat_P_tonne_1965 ,breaks=c(0,20,40,80,Inf),
                         labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_1990 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2000 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  scale_fill_manual(values = c("<20" = "#999933", "20-40" = "#44AA99",
                               "40-80" = "#661100", ">80" = "#332288", "na,value" = "lightgrey"),
                    limits = c("<20", "20-40", "40-80", ">80")) +
  #geom_tile(aes(fill= Wheat_P_tonne_2000) , alpha = 0.8) +
  geom_polygon(data=country, aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.25)+
  labs(#x= "lat",
    #y = "lon"), 
    title = paste0("Wheat production in 2000 (10^3 tonne/yr")) +
  #facet_wrap(. ~ layer, ncol = 4) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        axis.line = element_blank(),
        #axis.title=element_text(size=14,face="bold"),
        #panel.grid.major = element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        legend.position="right",
        #legend.box.spacing = unit(-1, 'cm'),
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0,margin = unit(c(-15,0,0,0), "mm")),
        #plot.subtitle = element_text(color="blue"),
        #plot.background = element_rect(fill="green"),
        plot.margin = margin(0, 0, 0, 0, "cm")
  ) +
  scale_y_continuous(limits = c(25.99995, 70)) +
  scale_x_continuous(limits = c(-27, 40))+
  coord_equal() +
  theme(axis.text.x = element_text(face = "bold",colour = "black", size = 24, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(face = "bold",colour = "black", size = 24),
        plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 20),
        strip.text = element_text(face = "bold", hjust = 0.15, size = 14),
        axis.title.y = element_text(face="bold",size = 24),
        axis.title.x = element_text(face="bold",size = 24),
        panel.grid = element_blank() ,
        legend.position = "right",
        legend.text = element_text(face="bold",size = 22),
        legend.title= element_blank()) +
  #scale_fill_viridis(na.value="grey")+
  theme(legend.key.width=unit(2, "cm"))+
  #scale_color_brewer(palette="Set2")  +
  #scale_color_manual(values = c("lightpurple", "orange", "blue", "darkpink")) +
  #scale_colour_viridis() +
  #scale_color_aaas() +
  #scale_fill_manual(name = "Values", values=setNames(colors, 1:8))+
  #scale_fill_gradientn(
  #colours = terrain.colors(15), 
  #breaks = seq(from = 0, to =4000, by = 500)
  #)+
  ggsave("This_study/crop_production/Plots/Wheat_P_1990.png", width=20, height=15)




rm(list=ls())
####
## Import raster datasets and convert them into dataframe ###
ARay_mean <- raster("This_study/crop_production/Outputs/Step_2/ARay_mean_Wheat.tif") %>%
  as.data.frame(., xy=T) 
EU_ARay_mean <- raster("This_study/crop_production/Outputs/Step_2/EU_ARay_mean_Wheat.tif") %>%
  as.data.frame(., xy=T) 

colnames(ARay_mean) <- c("Longitude", "Latitude", "ARay_mean_Wheat")
colnames(EU_ARay_mean) <- c("Longitude", "Latitude", "EU_ARay_mean_Wheat")           

p <- ggplot(ARay_mean, aes(y=Latitude, x=Longitude)) +
 geom_tile(aes(fill= ARay_mean_Wheat)) + ## Insert breaks
  #geom_tile(aes(fill=cut(ARay_mean_Wheat, breaks=c(0,0.5,1.00,1.5,2, 2.5),
  #labels = c("0", "0-0.5", "1-1.5", "1.5-2", "> 2" )))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2000 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #geom_tile(aes(fill=cut(Wheat_P_tonne_2010 ,breaks=c(0,20,40,80,Inf),
  #labels = c("<20", "20-40", "40-80", ">80")))) +
  #scale_fill_manual(values = c("0" = "#999933", "0-0.5" = "#44AA99",
   # "1-1.5" = "#661100", "1.5-2" = "#332288","> 2" ="#009E73"), #,">50" = "#E69F00"),
  #limits = c("0", "0-0.5", "1-1.5", "1.5-2", "> 2")) +
  geom_polygon(data=country, aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.25)+ ## Set country boundary
  labs(title = paste0("Average of wheat harvested area from Ray et al")) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks.length = unit(0, "mm"),
        axis.line = element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        legend.position="right",
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0,margin = unit(c(-15,0,0,0), "mm")),
        plot.margin = margin(0, 0, 0, 0, "cm")
  ) +
  #scale_y_continuous(limits = c(25.99995, 70)) +
  #scale_x_continuous(limits = c(-27, 40))+
  coord_equal() +
  theme(axis.text.x = element_text(face = "bold",colour = "black", size = 24, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(face = "bold",colour = "black", size = 24),
        plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 25),
        strip.text = element_text(face = "bold", hjust = 0.15, size = 14),
        axis.title.y = element_text(face="bold",size = 24),
        axis.title.x = element_text(face="bold",size = 24),
        panel.grid = element_blank() ,
        legend.position = "right",
        legend.text = element_text(face="bold",size = 22),
        legend.title= element_blank()) +
  scale_fill_viridis(na.value="grey")+
  theme(legend.key.width=unit(2, "cm"))+
  ggsave("This_study/crop_production/Plots/ARay_mean_wheat.png", width=20, height=15)






