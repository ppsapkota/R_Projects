source("./R/r_ps_library_init.R")
library(gridExtra)


f_name<-"./Data/SCSO/2019_05/3ws_updated_source.xlsx"
f_name_admin<-"./GIS/syr_admin_180627.xlsx"
data<-read_excel(f_name,na="NA")

admin3<-read_excel(f_name_admin,na="NA",sheet="admin3")
#dropsome columns
admin3<-admin3 %>% 
        select(-c(admin0Name_en,admin0Name_ar,admin0Pcode,LastUpdateDate,validOn,validTo))
###--------FOR RESTRUCTURING---------------------
#org names
f_orgname<-"./Data/SCSO/2019_05/3ws_updated_source.xlsx"
d_orgname<-read_excel(f_orgname,na="NA")
d_orgname<-select(d_orgname,NGO,NGO_ar)

#name mapping
d_sector<-read_excel("./Data/SCSO/2019_05/DBName2sectorNames_May2019.xlsx",na="NA")

#Read merged data
f_name<-"./Data/SCSO/2019_05/scso_data_merged_20190513.xlsx"
data<-read_excel(f_name,na="NA")
#--key
key<-row.names(data)
data<-cbind(key, data)
#some clean ups
data_heading<-names(data)
#col_ind_sd<-which(str_detect(data_heading,"_sdis"))
#get index for organisation names
org_name<-c("key","Sub-district", "Pcode","org_name")
col_ind_org_name <- which(data_heading %in% org_name)
n_cols<-length(col_ind_org_name)
#select required columns only
#d_3w<-select(data,col_ind_org_name,col_ind_sd)
flag<-0
#loop through sector table
for (s_row_i in 1:nrow(d_sector)){
  #s_row_i<-2
  print (paste0("row - ", s_row_i))
  d_row_koboname<-d_sector$KoBoName[s_row_i]
  sector<-d_sector$Sector[s_row_i]
  sectortype<-d_sector$SectorType[s_row_i]
  #find column index for d_row_koboname
  col_ind_sector<-which(data_heading %in% c(d_row_koboname))
  #
  #col_ind_locations<-which(data_heading %in% c("Sub-district", "Pcode"))
  
  #select data for each cluster
  d_3w_sector<-select(data,col_ind_org_name,col_ind_sector)
  d_3w_sector<-drop_na(d_3w_sector,d_row_koboname)
  #rename the field
  col_ind_sector<-which(names(d_3w_sector) %in% c(d_row_koboname))
  names(d_3w_sector)[col_ind_sector]<-"Presence"
  if(nrow(d_3w_sector)>0){
      #add sector name
      d_3w_sector$sector<-sector
      d_3w_sector$sectortype<-sectortype
      #
      key_row<-row.names(d_3w_sector)
      d_3w_sector<-cbind(key_row, d_3w_sector)
      #
      #merge to the main table
      if (flag==0){
          d_3w_restructure<-d_3w_sector
      } else {
          d_3w_restructure<-bind_rows(d_3w_restructure,d_3w_sector)
      }
      #
      flag<-flag+1
  }
  
}

#d_3w_restructure<-left_join(d_3w_restructure,d_sector,by=c("cluster"="KoBoName"))
#d_3w_restructure<-select(d_3w_restructure,1,2,3,5,6)

#Some clean up

d_3w_restructure<-rename(d_3w_restructure,
                         "subdistrict_pcode"="Pcode",
                         "admin3_name_en"="Sub-district",
                         "Organization_EN"="org_name"
                         )

# d_3w_restructure<-rename(d_3w_restructure,
#                          "Organization_AR"="g_main/g_info/g_co_na/org_na_ar",
#                          "Organization_EN"="g_main/g_info/g_co_na/org_na_en"
#                          )
#remove rows where no presence information is available
d_3w_restructure<-filter(d_3w_restructure,!is.na(Presence))
#remove rows if no is mentioned 'ل'
d_3w_restructure<-filter(d_3w_restructure,Presence != 'لا')

#bring governorate names
d_3w_restructure<-right_join(admin3,d_3w_restructure,by=c("admin3Pcode"="subdistrict_pcode"))

#drop a column
d_3w_restructure<-select(d_3w_restructure, -admin3_name_en)

#bring arabic name
d_3w_restructure<-left_join(d_3w_restructure, d_orgname,by=c("Organization_EN"="NGO"))
d_3w_restructure<-rename(d_3w_restructure,
                         "Organization_AR"="NGO_ar"
)

#replace arabic yes to Y
col_ind<-which(names(d_3w_restructure) %in% c('Presence'))
d_3w_restructure[,col_ind]<-ifelse(d_3w_restructure[,col_ind]=='نعم',"Y",d_3w_restructure[,col_ind])

#
row_index<-row.names(d_3w_restructure)
d_3w_restructure<-cbind(row_index, d_3w_restructure)

print(paste0("Done data restructuring...writing ", nrow(d_3w_restructure), " rows."))

#NOW save file
wb<-createWorkbook()
addWorksheet(wb,"scso_3w_data")
writeDataTable(wb, x=d_3w_restructure, sheet="scso_3w_data")
#openxlsx::write.xlsx(d_3w_restructure, gsub(".xlsx","_restructured.xlsx",f_name), sheetName = "sngo_3w_data",row.names = FALSE)
#some reshaping
d_3w_restructure$Answer<-"Y"
d_3w_mapping<-d_3w_restructure %>%
              select(-c("key_row", "key", "sectortype")) %>%
              distinct() %>% 
              spread(key=sector,value=Answer)

#openxlsx::write.xlsx(d_3w_mapping, gsub(".xlsx","_pivot.xlsx",f_name), sheetName = "sngo_3w_data",row.names = FALSE)
addWorksheet(wb,"map_sd_sector")
writeDataTable(wb, x=d_3w_mapping, sheet="map_sd_sector")


#Some stats
#number of organisations per subdistrict
d_n_org_sd <- d_3w_restructure %>%
  select(Organization_EN, admin1Name_en,admin3Name_en,admin3Pcode) %>%
  distinct() %>% 
  group_by(admin1Name_en,admin3Name_en,admin3Pcode) %>% 
  summarise(total_n_org=n()) %>%
  ungroup()

addWorksheet(wb,"summary_org_sd")
writeDataTable(wb, x=d_n_org_sd, sheet="summary_org_sd")


#number of organisations per subdistrict by sector type
d_n_org_sectortype_sd <- d_3w_restructure %>%
              select(Organization_EN,sectortype,admin1Name_en,admin3Name_en,admin3Pcode) %>%
              distinct() %>% 
              group_by(admin1Name_en,admin3Name_en,admin3Pcode,sectortype) %>% 
              summarise(n_org=n()) %>% 
              spread(sectortype,n_org) %>% 
              ungroup()

#names
names(d_n_org_sectortype_sd)<-make.names(names(d_n_org_sectortype_sd),unique = TRUE, allow_ = TRUE)
names(d_n_org_sectortype_sd)<-str_replace(names(d_n_org_sectortype_sd),"\\.","_")

addWorksheet(wb,"summary_org_sectortype_sd")
writeDataTable(wb, x=d_n_org_sectortype_sd, sheet="summary_org_sectortype_sd")


#number of organisations per subdistrict per sector
d_n_org_sd_sector <- d_3w_restructure %>%
  select(Organization_EN,sector,sectortype,admin1Name_en,admin3Name_en,admin3Pcode) %>%
  distinct() %>% 
  group_by(admin1Name_en,admin3Name_en,admin3Pcode,sector) %>% 
  summarise(n_org=n()) %>% 
  spread(sector,n_org) %>% 
  ungroup()
   
# Join relief and non-relief information in the same table
d_n_org_sd_sector <- d_n_org_sd_sector %>% 
                     left_join(select(d_n_org_sectortype_sd,3:ncol(d_n_org_sectortype_sd)),by="admin3Pcode") %>% 
                     left_join(select(d_n_org_sd,3:ncol(d_n_org_sd)),by="admin3Pcode")

addWorksheet(wb,"summary_org_sd_sector")
writeDataTable(wb, x=d_n_org_sd_sector, sheet="summary_org_sd_sector")

#number of organisations per sector
d_n_org_sector <- d_3w_restructure %>%
  select(Organization_EN,sectortype,sector) %>%
  distinct() %>% 
  group_by(sectortype,sector) %>% 
  summarise(n_org=n()) %>% 
  ungroup()

addWorksheet(wb,"summary_org_sector")
writeDataTable(wb, x=d_n_org_sector, sheet="summary_org_sector")

#number of organisations per governorate sector type
d_n_org_sectortype_gov<-d_3w_restructure %>% 
  select(admin1Name_en,Organization_EN,sectortype) %>% 
  distinct() %>% 
  group_by(admin1Name_en,sectortype) %>% 
  summarise(n_org=n()) %>% 
  spread(key = sectortype,n_org) %>% 
  ungroup()

addWorksheet(wb,"summary_sectortype_gov")
writeDataTable(wb, x=d_n_org_sectortype_gov, sheet="summary_sectortype_gov")
#number of organisations per governorate by sector
d_n_org_sector_gov<-d_3w_restructure %>% 
                    select(admin1Name_en,Organization_EN,sectortype, sector) %>% 
                    distinct() %>% 
                    group_by(admin1Name_en,sector) %>% 
                    summarise(n_org=n()) %>% 
                    spread(key = sector,n_org) %>% 
                    ungroup() %>% 
                    left_join(d_n_org_sectortype_gov,by="admin1Name_en")

addWorksheet(wb,"summary_org_sector_gov")
writeDataTable(wb, x=d_n_org_sector_gov, sheet="summary_org_sector_gov")



#number of subdistrict per governorate per organisation
d_n_sd_org<-d_3w_restructure %>% 
            select(admin1Name_en, admin3Pcode,Organization_EN) %>% 
            distinct() %>% 
            group_by(admin1Name_en,Organization_EN) %>% 
            summarise(n_sd=n()) %>%
            spread(key=admin1Name_en,n_sd) %>% 
            ungroup() %>% 
            arrange(Organization_EN)
            
addWorksheet(wb,"summary_org_gov_numsd")
writeDataTable(wb, x=d_n_sd_org, sheet="summary_org_gov_numsd")

#number of subdistrict per organisation by relief/non-relief
d_n_sd_org_sectortype<- d_3w_restructure %>% 
                        select(admin3Pcode,Organization_EN,sectortype) %>% 
                        distinct() %>% 
                        group_by(Organization_EN,sectortype) %>% 
                        summarise(n_sd=n()) %>% 
                        spread(key=sectortype,n_sd) %>% 
                        ungroup() %>% 
                        arrange(Organization_EN)

addWorksheet(wb,"summary_org_sectortype_numsd")
writeDataTable(wb, x=d_n_sd_org_sectortype, sheet="summary_org_sectortype_numsd")

#
saveWorkbook(wb, gsub(".xlsx","_restructured.xlsx",f_name),overwrite = TRUE)

# ####__________________PLOTS_________________________________________________
# #PLOT heatmap
# 
# plot_d_n_org_sector_gov <- d_n_org_sector_gov %>% 
#                            gather(key="sector",value="n_org",-1) 
# value<-"n_org"
# base_size <- 9
# p<- ggplot(data = plot_d_n_org_sector_gov,aes_string(x = "sector", y = "admin1Name_en")) +
#               geom_tile(aes_string(fill=value),colour = "white")+
#               geom_text(aes_string(label=value))+
#               coord_fixed(expand = FALSE) +
#               scale_fill_gradient(low = "lightblue", high = "steelblue")
# 
# 
# heatmap<- p + 
#           theme_grey(base_size = base_size) + 
#           labs(x = "", y = "") +
#           scale_x_discrete(expand = c(0, 0),position = "top") +
#           scale_y_discrete(expand = c(0, 0)) + 
#           theme(axis.text.x = element_text(size = base_size*0.8, angle = 90, hjust = 0, colour = "grey50"))+
#           theme(legend.position="none")
# heatmap
# 
# #####----------------------MAPS---------------------------------------------------------------
# #-----main data to map
# d_map_admin3<-d_n_org_sd_sector
# dataset<-d_map_admin3
# 
# ##set path of shapefile
# shp_path<-"./Data/GIS"
# admin3_shp_name<-"syr_admbnda_adm3_uncs_unocha"
# admin1_shp_name<-"syr_admbnda_adm1_uncs_unocha"
# admin1_shp_name_c<-"syr_admin1_centroid"
# #read shapefiles
# ####-------sf feature class---
# shp_admin3<-sf::read_sf(paste0(shp_path,"/",admin3_shp_name,".shp"))
# shp_admin1<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name,".shp"))
# shp_admin1_centroid<-sf::read_sf(paste0(shp_path,"/",admin1_shp_name_c,".shp"))
# #shp_admin3 <-read_shape(paste0(shp_path,"/",admin3_shp_name,".shp"), as.sf=TRUE) #tmaptool package
# map_admin3<- left_join(shp_admin3, d_map_admin3,by=c("admin3Pcod"="admin3Pcode"))
# map_admin1<-shp_admin1
# map_admin1_centroid<-shp_admin1_centroid
# # map_admin1_centroid<-st_centroid(shp_admin1)
# # #Add Long and Lat coordinate
# # map_admin1_centroid<-separate(map_admin1_centroid,geometry,into=c("long","lat"),sep=",",remove=FALSE, extra='drop')
# # map_admin1_centroid$long<-str_replace_all(map_admin1_centroid$long,c('\\c'='','\\('='','\\)'=''))
# # map_admin1_centroid$lat<-str_replace_all(map_admin1_centroid$lat,c('\\c'='','\\('='','\\)'=''))
# 
# #PLOT METHOD 2---using ggplot 2 to sf features
# 
# ###-----prepare map data for looping purpose -------------
# f_field<-which(names(map_admin3)=="Agriculture")
# l_field<-which(names(map_admin3)=="total_n_org")
# map_field_list<-names(map_admin3[f_field:l_field])
# #remove geometry from the list
# map_field_list<-map_field_list[-which(map_field_list=="geometry")]
# 
# for (a_field in map_field_list){
#   map_val_field<-a_field
#   map_val_label<-"# of organizations"
#   
#   plot_savename<-paste0("./Data/SCSO/",map_val_field,".pdf")
#   print(plot_savename)
#   
#   map_data_plot<-map_admin3 %>% 
#           select(admin3Name,admin3Pcod, map_val_field,geometry) %>% 
#           rename_(map_val= map_val_field)
#   
#   #plot thematic map
#   map_plot_i<-plot_thematic_map(map_data_plot,map_val_field,plot_savename)
#   plot_print<-grid.arrange(map_plot_i,heatmap,nrow=2)
#   ggsave(plot_savename,plot=plot_print,dpi=300, width=16.54, height=11.69, units="in", scale=2)
# }
# #--------------------
# #--map_data must have field 'map_val' which is used for plotting
# plot_thematic_map<-function(map_data, field_name, plot_savename){
#     #pal<-brewer.pal(n=7,"OrRd")
#     pal<-brewer.pal(n=5,"Blues")
#     palette_name<-"Blues"
#     #breaks_qt<-classIntervals(map_admin3$total_n_org,n=5,style="quantile")
#     #br<-breaks_qt$brks
#     br<- c(1,2,5,10,20,500)
#     br_legend_txt<-c("1-2","3-5","6-10","10-20",">20","NA")
#     #--run some offsets
#     offs <- 0.0000001 
#     br[1] <- br[1] - offs 
#     br[length(br)] <- br[length(br)] + offs 
#     
#     ###color value
#     #map_data$map_br_val<-cut(map_data$map_val,br,right = FALSE)
#     #map_class_interval<-cut(map_data$map_val,br,right=FALSE)
#     #color_val<-pal[as.numeric(map_class_interval)]
#     
#     #---Map theme-------
#     theme_map <- theme_minimal() +
#       theme(
#         text = element_text(color = "#22211d"),
#         axis.line = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.grid.minor = element_line(color = "#ffffff", size = 0.2),
#         panel.grid.major = element_line(color = "#ffffff", size = 0.1),
#         #panel.grid.major = element_blank(),
#         plot.background = element_rect(fill = "#ffffff", color = NA), 
#         panel.background = element_rect(fill = "white", color = NA), 
#         panel.border = element_blank(),
#         #panel.grid = element_blank(),
#         #panel.grid.major = element_blank(),
#         legend.background = element_rect(fill = "#ffffff", color = NA)
#         #legend.direction = "horizontal"
#       )
#     #PLOT map using sf feature class
#    map_plot<- ggplot()+
#       #geom_sf(data=map_admin3, aes(fill=cut(total_n_org ,br, right = FALSE)),color='grey80')+
#       geom_sf(data=map_data, aes(fill=cut(map_val,br)),color='grey80',size=0.1)+
#       geom_sf(data=map_admin1, fill=NA, color='grey50', size=0.5)+
#       #geom_sf(data=map_admin1_centroid)+
#       geom_text(data=map_admin1_centroid,aes(as.numeric(Longitude),as.numeric(Latitude),label=admin1Name),hjust = 0, nudge_x = 0.0,nudge_y = 0.0,check_overlap = TRUE)+
#       ggtitle(map_val_label)+
#       #scale_color_brewer(palette = pal)+
#       #scale_fill_brewer(map_val_label,palette = palette_name, na.value="grey90", labels=br_legend_txt)+
#       scale_fill_manual(breaks=br,values = pal,labels=br_legend_txt)+
#       #scale_fill_manual(breaks=br,labels=br_legend_txt,values = pal)+
#       coord_sf() +
#       theme_map
#     #
#     return(map_plot)
# }






