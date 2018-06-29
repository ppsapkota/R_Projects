source("./R/r_ps_library_init.R")

library(rgdal)
library(ggplot2)
library(sf)
library(sp)
library(RColorBrewer)
library(classInt)
library(scales)
#-------------------
library(tmap)
library(tmaptools)
library(leaflet)

##set path of shapefile
shp_path<-"./Data/GIS"
admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"
#-----main data to map
d_map_admin3<-admin3_cluster_budget_pivot
dataset<-d_map_admin3
##---Read shapefile-------------
# shp_admin3<-readOGR(shp_path,admin3_shp_name)
# ### use left_join to join data to attributes table without changing order
# shp_admin3@data<- left_join(shp_admin3@data, d_map_admin3,by=c("PCODE"="SubDistrict_Pcode"))
# map_admin3<-shp_admin3
# plot(map_admin3,border=gray(.8), axes = FALSE, las = 1,col=map_admin3@data$Education,main="Education",font=15) 
# #

##--Convert shapefile 
#d_shp_admin3<-fortify(shp_admin3)

####-------sf feature class---
shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
#shp_admin3 <-read_shape(paste0(shp_path,"/",admin3_shp_name,".shp"), as.sf=TRUE) #tmaptool package
map_admin3<- left_join(shp_admin3, d_map_admin3,by=c("admin3Pcod"="SubDistrict_Pcode"))
map_admin1<-shp_admin1
map_admin1_centroid<-st_centroid(shp_admin1)
#Add Long and Lat coordinate
map_admin1_centroid<-separate(map_admin1_centroid,geometry,into=c("long","lat"),sep=",",remove=FALSE, extra='drop')
map_admin1_centroid$long<-str_replace_all(map_admin1_centroid$long,c('\\c'='','\\('='','\\)'=''))
map_admin1_centroid$lat<-str_replace_all(map_admin1_centroid$lat,c('\\c'='','\\('='','\\)'=''))
#aa<-fortify(map_admin1)
#Color pallete
#display.brewer.all()
pal<-brewer.pal(n=5,"OrRd")
breaks_qt<-classIntervals(map_admin3$Education,n=5,style="quantile")
br<-breaks_qt$brks
#for first and last value
offs <- 0.0000001 
br[1] <- br[1] - offs 
br[length(br)] <- br[length(br)] + offs 
##
##br_format<-format(round(br,0),big.mark=",",scientific=FALSE)
br_legend_txt<-Make_Discrete_Class_Labels(br,0)

#OR
#br<- c(0,1,60000,10000,200000,300000,max(br))
#br_legend_txt<-c()
#
map_admin3$map_br_val<-cut(map_admin3$Education,br,right = TRUE)
map_class_interval<-cut(map_admin3$Education,br,right=TRUE)
color_val<-pal[as.numeric(map_class_interval)]

#
# lev<-levels(cut(map_admin3$Education,br,right = TRUE))
# lev2 <- gsub("\\,", "% to ", lev)
# lev3 <- gsub("\\]$", "%", lev2)
# lev3
# lev4 <- gsub("\\(|\\)", "", lev3)
# lev4

###PLOT METHOD 1--Using simple feature (sf) and plot
# plot(map_admin3["Education"],border=gray(.8), axes = FALSE, main="Education",col=color_val)
# legend(x="bottomright",legend = paste("<", round(br[-1])), fill = pal, title = "Legend",border = NULL)

#PLOT METHOD 2---using ggplot 2 to sf features
#---Map theme-------
theme_map <- theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_line(color = "#f5f5f2", size = 0.2),
    panel.grid.major = element_line(color = "#f5f5f2", size = 0.1),
    #panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.border = element_blank(),
    #panel.grid = element_blank(),
    #panel.grid.major = element_blank(),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #legend.direction = "horizontal"
  )
#plot map using sf feature class
color_val<-pal[as.numeric(map_class_interval)]
ggplot()+
  geom_sf(data=map_admin3, aes(fill=cut(Education,br, right = TRUE)),color='grey80')+
  geom_sf(data=map_admin1,fill=NA, color='grey50', size=1)+
  #geom_sf(data=map_admin1_centroid)+
  geom_text(data=map_admin1_centroid,aes(as.numeric(long),as.numeric(lat),label=admin1Name),hjust = 1, nudge_x = 0.0,check_overlap = TRUE)+
  ggtitle('Education')+
  #scale_color_brewer(palette = pal)+
  scale_fill_brewer("Education",palette = "OrRd", na.value="grey90", labels=br_legend_txt)+
  #scale_fill_manual(breaks=br,labels=br_legend_txt,values = pal)+
  coord_sf() +
  theme_map

# ###PLOT METHOD 3-----PLOT using tmaptools packages-------------
# #qtm(map_admin3,"Education")
# map_field<-"Protection"
# map_title<-map_field
# #simple approach
# tm_shape(map_admin3) +
#   tm_polygons(map_field,style="quantile", title=map_title)
# 
# #--Alternate
# pal<-brewer.pal(n=6,"OrRd")
# data_br<-c(0,1,5000,10000,50000,100000,500000)
# tm_shape(map_admin3) +
#   tm_fill(map_field,title = map_title, 
#           breaks=data_br,
#           colorNA="grey90",
#           textNA='Not available',
#           palette = pal)+
#   tm_borders(alpha = 0.3) +
#   tm_layout("",
#             legend.title.size = 1,
#             legend.text.size = 0.6,
#             legend.position = c("right","bottom"),
#             legend.bg.color = "white",
#             legend.frame=FALSE,
#             #legend.digits = 5,
#             legend.bg.alpha = 1)+
#   tm_facets(free.scales = FALSE)
#   #tm_facets("ADM1_EN")
# #map2<-map1
# #tmap_arrange(map1,asp=NA)
# #save_tmap(map1, "World_map.png", width=1920, height=1080)
# 
# 
#   #tm_text("NAME_EN", size = 0.8)
# tmap_mode("view") ##interactive viewing
# tmap_mode("plot")
# last_map()
# tmap_leaflet(map1)


###------------FUNCTION-------------------------------
Make_Discrete_Class_Labels<-function(x, n){
  x=round(x,0)
  
 # for (i in seq(2,length(x),by=2)){
 #   x[i]<-ceiling_dec(x[i],0)
 # }
 x_le<-str_trim(format(round(x[1],n),big.mark=",",scientific=FALSE))
 for (i in 2:length(x)){
   x_le_i_<-ceiling_dec(round(x[i-1],n)+0.000001,n)
   x_le_i_<-str_trim(format(x_le_i_,big.mark=",",scientific=FALSE))
   x_le_i<-str_trim(format(round(x[i],n),big.mark=",",scientific=FALSE))
   #concat interval
   x_le<-c(x_le, paste0(x_le_i_, " - ",x_le_i))
 }
 ##
  if (length(x_le)>1){
    x_le<-x_le[2:length(x_le)]
    }
  x_le
}

###-----------------
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)






# first define a set of layout/design parameters to re-use in each map
mapTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 11, color = "#1c5074", hjust = 0, vjust = 2, face = "bold"), 
      plot.subtitle = element_text(size = 8, color = "#3474A2", hjust = 0, vjust = 0),
      axis.ticks = element_blank(), 
      legend.direction = "vertical", 
      legend.position = "right",
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm")
    ) 
}

