rm(list = ls())

source("./R/r_ps_library_init.R")

f_name<-"./Data/SNGos/3Ws_Sep_Clean_Data.xlsx"
data<-read_xlsx(f_name)
#name mapping
d_sector<-read_excel("./Data/SNGOs/DBName2sectorNames.xlsx")
#--key
key<-row.names(data)
data<-cbind(key, data)
#some clean ups
data_heading<-names(data)
col_ind_sd<-which(str_detect(data_heading,"_sdis"))
#
d_3w<-select(data,12,13,col_ind_sd)

flag<-0

for (i in 1:nrow(d_3w))
{
  org_name_en <- d_3w$`org_page/group_focalpoing/org_en_name`[i]
  #org_name_ar<-d_3w$`org_page/group_focalpoing/org_ar_name`[i]
  d_org_i<-d_3w[i,1:2]
  #print(paste0("working..."," ",i," ", org_name_en ))
  for (j in 3:ncol(d_3w)){
  #for (j in 4:6){ #for testing purpose
    cluster<-names(d_3w)[j]
    location_sd<-unlist(str_split(d_3w[i,j]," "))
    location_sd<-as.data.frame(location_sd)
    #some clean ups
    location_sd[1]<-str_replace(location_sd[,1],"PSY","SY")
    #pcode name
    names(location_sd)[1]<-"subdistrict_pcode"
    #
    location_sd<-cbind(d_org_i,cluster,location_sd,row.names=NULL)
    #remove NA from the data
    location_sd<-filter(location_sd,!is.na(subdistrict_pcode))
    if (flag==0){
      d_3w_restructure<-location_sd
    }else {
      d_3w_restructure<-bind_rows(d_3w_restructure,location_sd)    
    }
    flag<-flag+1
  }
}
key<-row.names(d_3w_restructure)
d_3w_restructure<-cbind(key, d_3w_restructure)
d_3w_restructure<-left_join(d_3w_restructure,d_sector,by=c("cluster"="KoBoName"))
d_3w_restructure<-select(d_3w_restructure,1,2,3,5,6)

names(d_3w_restructure)[2]<-"Organization_AR"
names(d_3w_restructure)[3]<-"Organization_EN"


print(paste0("Done data restructuring...writing ", nrow(d_3w_restructure), " rows."))
openxlsx::write.xlsx(d_3w_restructure, gsub(".xlsx","_restructured.xlsx",f_name), sheetName = "sngo_3w_data",row.names = FALSE)
#some reshaping
d_3w_restructure$Answer<-"Y"
d_3w_mapping<-d_3w_restructure %>%
  select(-1) %>% 
   spread(key=Sector,value=Answer)
openxlsx::write.xlsx(d_3w_mapping, gsub(".xlsx","_pivot.xlsx",f_name), sheetName = "sngo_3w_data",row.names = FALSE)




