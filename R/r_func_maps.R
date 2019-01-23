source("./R/r_ps_library_init.R")

#library(rgdal)
library(ggplot2)
library(sf)
#library(sp)
library(RColorBrewer)
library(classInt)
library(scales)
#-------------------
# library(tmap)
# library(tmaptools)
# library(leaflet)
library(ggiraph)
#data<-nprojects_subdistrict
make_admin3_basemap<-function(){
  #define join field, this can be made a paramer input
  shp_path<-"./GIS/SHP"
  admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
  admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"
  
  ####-------sf feature class---
  shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
  shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
  
  #join data value to the shape file DF
  map_admin3<- shp_admin3
  map_admin1<-shp_admin1
  map_admin1_centroid<-shp_admin1 %>% 
    mutate(long=map_dbl(geometry,~st_centroid(.x)[[1]]),
           lat=map_dbl(geometry,~st_centroid(.x)[[2]])
    )
  #PLOT METHOD---using ggplot 2 to sf features
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
      legend.background = element_rect(fill = "white")
      #legend.direction = "horizontal"
    )
  ##
  p<-ggplot()+
    geom_sf(data=map_admin3,fill=NA,color='grey85', size=0.1)+
    geom_sf(data=map_admin1,fill=NA, color='grey70', size=0.6)+
    geom_text(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat),label=str_wrap(admin1Na_1,width=12)),hjust =0.5,vjust=0, nudge_x = 0.0,check_overlap = FALSE, size=3.25)+
    coord_sf() +
    theme_map
  p
  return(p)
}



make_admin3_map<-function(data){
  dataset<-data##assign data to map
  d_map_admin3<-data
  #define join field, this can be made a paramer input
  joinfield_data<-"SubDistrict_Pcode"
  joinfield_shp<-"admin3Pcod"
  map_field<-"nProjects"
  legend_title<-"# of projects"
  ##set path of shapefile
  shp_path<-"./GIS/SHP"
  admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
  admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"
  
  ####-------sf feature class---
  shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
  shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
  
  #join data value to the shape file DF
  map_admin3<- left_join(shp_admin3, d_map_admin3,by=c(setNames(joinfield_data,joinfield_shp)))
  map_admin1<-shp_admin1
  map_admin1_centroid<-shp_admin1 %>% 
                       mutate(long=map_dbl(geometry,~st_centroid(.x)[[1]]),
                               lat=map_dbl(geometry,~st_centroid(.x)[[2]])
                              )  
  #map_admin1_centroid<-st_centroid(shp_admin1)
  #Add Long and Lat coordinate
  #map_admin1_centroid$long<-map_admin1_centroid$geometry[[1]] %>% as.double()
  #map_admin1_centroid$lat<-map_admin1_centroid$geometry[[2]] %>% as.double()
  
  #Color pallete
  #display.brewer.all()
  #color<-"OrRd"
  #color<-"Blues"
  #pal<-brewer.pal(n=5,color)
  #shades <- colorRampPalette(c("white", "Blue"))(6)
  #breaks_qt<-classIntervals(map_admin3[,c(map_field)],n=5,style="quantile")
  #br<-breaks_qt$brks
  #Alternate bread options
  max_data_val<-max(dataset[,c(map_field)])
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
  map_admin3_col_i<-which(names(map_admin3)==map_field)
  #
  map_admin3$map_br_val<-cut(map_admin3[,map_admin3_col_i][[1]],br, right = TRUE)
  map_class_interval<-cut(map_admin3[,map_admin3_col_i][[1]], br, right=TRUE)
  #map_class_interval<-cut(map_admin3$nProjects, br, right=TRUE)
  color_val<-pal[as.numeric(map_class_interval)]
  map_admin3$map_color_var<-color_val
  #PLOT METHOD---using ggplot 2 to sf features
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
      legend.background = element_rect(fill = "white")
      #legend.direction = "horizontal"
    )
  p<-ggplot()+
    geom_sf(data=map_admin3, aes(fill=map_class_interval),color='grey85', size=0.1)+
    geom_sf(data=map_admin1,fill=NA, color='grey70', size=0.6)+
    #geom_sf(data=map_admin1_centroid)+
    #geom_point(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat)))+
    geom_text(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat),label=str_wrap(admin1Na_1,width=12)),hjust =0.5,vjust=0, nudge_x = 0.0,check_overlap = FALSE, size=3.25)+
    ggtitle(legend_title)+
    #scale_color_brewer(palette = pal)+
    #scale_fill_brewer(legend_title,palette = color, na.value="grey95", labels=br_legend_txt)+
    #Reference
    #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    # Using a manual scale instead of hue
    # bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
    #                        name="Experimental\nCondition",
    #                        breaks=c("ctrl", "trt1", "trt2"),
    #                        labels=c("Control", "Treatment 1", "Treatment 2"))
    scale_fill_manual(values = pal,labels=lb_legend,name=legend_title)+
    coord_sf() +
    theme_map 
  #p
  return(p)
  
  # ggplotly(p) %>%
  #   highlight(
  #     "plotly_hover",
  #     selected = attrs_selected(line = list(color = "black"))
  #   ) %>%
  #   widgetframe::frameWidget()
  
  
}

make_admin3_map_interactive<-function(data){
  dataset<-data##assign data to map
  d_map_admin3<-data
  #define join field, this can be made a paramer input
  joinfield_data<-"SubDistrict_Pcode"
  joinfield_shp<-"admin3Pcod"
  map_field<-"nProjects"
  legend_title<-"# of projects"
  ##set path of shapefile
  shp_path<-"./GIS/SHP"
  admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
  admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"
  
  ####-------sf feature class---
  shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
  shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
  
  #join data value to the shape file DF
  map_admin3<- left_join(shp_admin3, d_map_admin3,by=c(setNames(joinfield_data,joinfield_shp)))
  map_admin1<-shp_admin1
  map_admin1_centroid<-shp_admin1 %>% 
    mutate(long=map_dbl(geometry,~st_centroid(.x)[[1]]),
           lat=map_dbl(geometry,~st_centroid(.x)[[2]])
    )  
  #map_admin1_centroid<-st_centroid(shp_admin1)
  #Add Long and Lat coordinate
  #map_admin1_centroid$long<-map_admin1_centroid$geometry[[1]] %>% as.double()
  #map_admin1_centroid$lat<-map_admin1_centroid$geometry[[2]] %>% as.double()
  
  #Color pallete
  #display.brewer.all()
  #color<-"OrRd"
  #color<-"Blues"
  #pal<-brewer.pal(n=5,color)
  #shades <- colorRampPalette(c("white", "Blue"))(6)
  #breaks_qt<-classIntervals(map_admin3[,c(map_field)],n=5,style="quantile")
  #br<-breaks_qt$brks
  #Alternate bread options
  max_data_val<-max(dataset[,c(map_field)])
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
  map_admin3_col_i<-which(names(map_admin3)==map_field)
  #
  map_admin3$map_br_val<-cut(map_admin3[,map_admin3_col_i][[1]],br, right = TRUE)
  map_class_interval<-cut(map_admin3[,map_admin3_col_i][[1]], br, right=TRUE)
  #map_class_interval<-cut(map_admin3$nProjects, br, right=TRUE)
  color_val<-pal[as.numeric(map_class_interval)]
  map_admin3$map_color_var<-color_val
  #PLOT METHOD---using ggplot 2 to sf features
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
      legend.background = element_rect(fill = "white")
      #legend.direction = "horizontal"
    )
  p<-ggplot()+
    geom_sf_interactive(data=map_admin3, aes(fill=map_class_interval, data_id=admin3Pcod, tooltip=paste0(nProjects,"\n",admin3Pcod)),color='grey85', size=0.1)+
    geom_sf_interactive(data=map_admin1,fill=NA, color='grey70', size=0.6)+
    #geom_sf(data=map_admin1_centroid)+
    #geom_point(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat)))+
    geom_text(data=map_admin1_centroid,aes(x=as.numeric(long),y=as.numeric(lat),label=str_wrap(admin1Na_1,width=12)),hjust =0.5,vjust=0, nudge_x = 0.0,check_overlap = FALSE, size=3.25)+
    ggtitle(legend_title)+
    #scale_color_brewer(palette = pal)+
    #scale_fill_brewer(legend_title,palette = color, na.value="grey95", labels=br_legend_txt)+
    #Reference
    #http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    # Using a manual scale instead of hue
    # bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
    #                        name="Experimental\nCondition",
    #                        breaks=c("ctrl", "trt1", "trt2"),
    #                        labels=c("Control", "Treatment 1", "Treatment 2"))
    scale_fill_manual(values = pal,labels=lb_legend,name=legend_title)+
    coord_sf() +
    theme_map 
  p_interactive<-girafe(code=print(p))
  return(p_interactive)
  # ggplotly(p) %>%
  #   highlight(
  #     "plotly_hover",
  #     selected = attrs_selected(line = list(color = "black"))
  #   ) %>%
  #   widgetframe::frameWidget()
}










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



#plot map using sf feature class
# ggplot()+
#   geom_sf(data=map_admin3,fill=color_val, color='grey80')+
#   #geom_sf(data=map_admin3,fill=NA, color='grey80')+
#   geom_sf(data=map_admin1,fill=NA, color='grey50', size=1)+
#   #geom_sf(data=map_admin1_centroid)+
#   geom_text(data=map_admin1_centroid,aes(as.numeric(long),as.numeric(lat),label=admin1Na_1),hjust = 1, nudge_x = 0.0,check_overlap = TRUE)+
#   ggtitle(legend_title)+
#   #scale_color_brewer(palette = pal)+
#   #scale_fill_brewer(legend_title,palette = "OrRd", na.value="grey90", labels=br_legend_txt)+
#   scale_fill_manual(breaks=br,values = pal,labels=br)+
#   coord_sf() +
#   theme_map 
