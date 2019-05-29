# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

#dataset <- data.frame(SubDistrict_Pcode, ChfProjectCode)
#dataset <- unique(dataset)

# Paste or type your script code here:
library(tidyverse)
library(rgdal)
library(ggplot2)
library(sf)
#library(sp)
library(RColorBrewer)
library(classInt)
library(scales)

#functions
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

#----assign data---------------
data<-as.data.frame(dataset)
#rename
data<-data %>% rename("nProjects"="ChfProjectCode")
dataset<-data
#

d_map_admin3<-data
#define join field, this can be made a paramer input
joinfield_data<-"SubDistrict_Pcode"
joinfield_shp<-"admin3Pcod"
map_field<-"nProjects"
legend_title<-"# of projects"
##set path of shapefile
shp_path<-"C:/01_OCHA_TR/03_IM_Tools/R/R_Projects/GIS/SHP"
admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"

#---method 1 -----RGDAL---
shp_admin3_rgdal <- readOGR(paste0(shp_path,"/",admin3_shp_name,".shp"), stringsAsFactors = F)
shp_admin3_rgdal_df<-fortify(shp_admin3_rgdal,region="admin3Pcod")
#rename id field

#
shp_admin1_rgdal <- readOGR(paste0(shp_path,"/",admin1_shp_name,".shp"), stringsAsFactors = F)
#bring data value
map_admin3_rgdal <- left_join(shp_admin3_rgdal_df,d_map_admin3,by=c(setNames(joinfield_data,"id")))

####---method 2----sf feature class---
shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
#join data value to the shape file DF
map_admin3<- left_join(shp_admin3, d_map_admin3,by=c(setNames(joinfield_data,joinfield_shp)))
map_admin1<-shp_admin1
map_admin1_centroid<-shp_admin1 %>% 
  mutate(long=map_dbl(geometry,~st_centroid(.x)[[1]]),
         lat=map_dbl(geometry,~st_centroid(.x)[[2]])
  )  

#----color
#Alternate bread options
max_data_val<-max(map_admin3_rgdal[,c(map_field)])
br<- c(1,2,5,10,max_data_val)
lb_legend<-c("1-2","3-5","6-10",">10")
#assign color
#pal<-c("red","yellow", "green","blue","brown","purple")
#shades <- colorRampPalette(c("grey50", "Blue"))(6)
pal<-c("#ccd9ed","#99b8db","#6192c7","#096bb5")
#for first and last value
offs <- 0.0000001  
br[1] <- br[1] - offs 
#br[2] <- br[2] - offs 
br[length(br)] <- br[length(br)] + offs 
##
##br_format<-format(round(br,0),big.mark=",",scientific=FALSE)
br_legend_txt<-Make_Discrete_Class_Labels(br,0)
#
#get column index for map_field - field with numeric data to create choropleth map
map_admin3_col_i<-which(names(map_admin3_rgdal)==map_field)
#
map_admin3_rgdal$map_br_val<-cut(map_admin3_rgdal[,map_admin3_col_i][[1]],br, right = TRUE)
map_class_interval<-cut(map_admin3_rgdal[,map_admin3_col_i][[1]], br, right=TRUE)
#map_class_interval<-cut(map_admin3$nProjects, br, right=TRUE)
color_val<-pal[as.numeric(map_class_interval)]
map_admin3_rgdal$map_color_var<-color_val


#---Map theme-------
theme_map <- theme_classic() +
  theme(
    text = element_text(color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_blank(),
    #panel.grid.minor = element_line(color = "#f5f5f2", size = 0.2),
    #panel.grid.major = element_line(color = "#f5f5f2", size = 0.1),
    panel.background = element_blank(), 
    panel.border = element_blank(),
    #panel.grid = element_line(color="white"),
    panel.grid.major = element_line(color="white"),
    #panel.grid.minor = element_blank(),
    #legend.position = c(0, 1),
    #legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white"),
    #legend.direction = "horizontal"
    legend.justification=c(0,0),
    legend.position="right"
  )


##---Method 1 -- PLOT-------
p<-ggplot()+
  geom_polygon(data = map_admin3_rgdal, aes(x = long, y = lat, group=group, fill=nProjects), colour = "grey85", size=0.3)+
  #scale_fill_manual(values = pal,labels=lb_legend,name=legend_title)+
  scale_fill_gradient(high = "#096bb5", low = "#ccd9ed", guide = "colorbar",na.value = "grey95")+
  # scale_fill_gradientn(colours = pal,
  #                      breaks=br,
  #                      na.value = "grey95"
  #                      )+
  
  guides(fill=guide_colorbar(title="# of projects")) +
  
  geom_polygon(data = shp_admin1_rgdal, aes(x = long, y = lat, group=group), colour = "grey70", size=1, fill = NA)+
  #geom_sf(data=map_admin1,fill=NA, color='grey70', size=0.6)+
  #geom_sf(data=map_admin1_centroid)+
  #geom_point(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat)))+
  geom_text(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat),label=str_wrap(admin1Na_1,width=12)),hjust =0.5,vjust=0, nudge_x = 0.0,check_overlap = FALSE, size=5)+
  #ggtitle(legend_title)+
  #scale_color_brewer(palette = pal)+
  #scale_fill_brewer(legend_title,palette = color, na.value="grey95", labels=br_legend_txt)+
  #Reference
  #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
  # Using a manual scale instead of hue
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
  #                         name="Experimental\nCondition",
  #                         breaks=c("ctrl", "trt1", "trt2"),
  #                         labels=c("Control", "Treatment 1", "Treatment 2"))+
  #coord_sf() +
coord_fixed(1) +
  theme_map
#plot
p
