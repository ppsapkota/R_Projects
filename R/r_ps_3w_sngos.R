rm(list = ls())

source("./R/r_ps_library_init.R")

f_name<-"./Data/SCSO/CSO_data_merged_20180628.xlsx"
f_name_admin<-"./Data/Admin/syr_admin_180627.xlsx"
data<-read_excel(f_name,na="NA")

admin3<-read_excel(f_name_admin,na="NA",sheet="admin3")
#dropsome columns
admin3<-admin3 %>% 
        select(-c(admin0Name_en,admin0Name_ar,admin0Pcode,LastUpdateDate,validOn,validTo))


#name mapping
d_sector<-read_excel("./Data/SCSO/DBName2sectorNames_Jun2018.xlsx",na="NA")
#--key
key<-row.names(data)
data<-cbind(key, data)
#some clean ups
data_heading<-names(data)
#col_ind_sd<-which(str_detect(data_heading,"_sdis"))
#get index for organisation names
koboname_org<-c("key","g_main/g_info/g_co_na/org_na_ar",	"g_main/g_info/g_co_na/org_na_en","g_main/g_info/g_co_na/org_na_tr")
col_ind_org_name <- which(data_heading %in% koboname_org)
n_cols<-length(col_ind_org_name)
#select required columns only
#d_3w<-select(data,col_ind_org_name,col_ind_sd)
flag<-0
#loop through sector table
for (s_row_i in 1:nrow(d_sector)){
  #r_row<-1
  d_row_koboname<-d_sector$KoBoName[s_row_i]
  sector<-d_sector$Sector[s_row_i]
  sectortype<-d_sector$SectorType[s_row_i]
  #find column index for d_row_koboname
  col_ind_sd<-which(data_heading %in% c(d_row_koboname))
  #select data for each cluster
  d_3w_sector<-select(data,col_ind_org_name,col_ind_sd)
  for (i in 1:nrow(d_3w_sector))
  {
    org_name_en <- d_3w_sector$`g_main/g_info/g_co_na/org_na_en`[i]
    d_org_i<-d_3w_sector[i,1:n_cols]
    #print(paste0("working..."," ",i," ", org_name_en ))
    #for (j in 3:ncol(d_3w)){
    #for (j in 4:6){ #for testing purpose
      #cluster<-names(d_3w)[j]
      location_sd<-unlist(str_split(d_3w_sector[i,n_cols+1]," "))
      location_sd<-as.data.frame(location_sd)
      #some clean ups
      #location_sd[1]<-str_replace(location_sd[,1],"PSY","SY")
      #pcode name
      names(location_sd)[1]<-"subdistrict_pcode"
      #
      location_sd<-cbind(d_org_i,sector,sectortype,location_sd,row.names=NULL)
      #remove NA from the data
      #location_sd<-filter(location_sd,!is.na(subdistrict_pcode))
      if (flag==0){
        d_3w_restructure<-location_sd
      }else {
        d_3w_restructure<-bind_rows(d_3w_restructure,location_sd)    
      }
      flag<-flag+1
    }
  }

key_row<-row.names(d_3w_restructure)
d_3w_restructure<-cbind(key_row, d_3w_restructure)
#d_3w_restructure<-left_join(d_3w_restructure,d_sector,by=c("cluster"="KoBoName"))
#d_3w_restructure<-select(d_3w_restructure,1,2,3,5,6)
#Sonme clean up
d_3w_restructure<-rename(d_3w_restructure,
                         "Organization_AR"="g_main/g_info/g_co_na/org_na_ar",
                         "Organization_EN"="g_main/g_info/g_co_na/org_na_en",
                         "Organization_TR"="g_main/g_info/g_co_na/org_na_tr"
                         )
#remove rows without subdistrict_pcode
d_3w_restructure<-filter(d_3w_restructure,!is.na(subdistrict_pcode))

#bring governorate names
d_3w_restructure<-inner_join(admin3,d_3w_restructure,by=c("admin3Pcode"="subdistrict_pcode"))

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

#number of organisations per governorate by sector
d_n_org_sector_gov<-d_3w_restructure %>% 
                    select(admin1Name_en,Organization_EN,sectortype, sector) %>% 
                    distinct() %>% 
                    group_by(admin1Name_en,sector) %>% 
                    summarise(n_org=n()) %>% 
                    spread(key = sector,n_org) %>% 
                    ungroup()

addWorksheet(wb,"summary_org_sector_gov")
writeDataTable(wb, x=d_n_org_sector_gov, sheet="summary_org_sector_gov")

#number of organisations per governorate
d_n_org_sectortype_gov<-d_3w_restructure %>% 
                        select(admin1Name_en,Organization_EN,sectortype) %>% 
                        distinct() %>% 
                        group_by(admin1Name_en,sectortype) %>% 
                        summarise(n_org=n()) %>% 
                        spread(key = sectortype,n_org) %>% 
                        ungroup()

addWorksheet(wb,"summary_sectortype_gov")
writeDataTable(wb, x=d_n_org_sectortype_gov, sheet="summary_sectortype_gov")

#
saveWorkbook(wb, gsub(".xlsx","_restructured.xlsx",f_name),overwrite = TRUE)


