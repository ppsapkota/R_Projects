
source("./R/r_ps_library_init.R")

d_fname<-"./Data/PPE/NW Syria_Non-Health_PPEs_Items_Data_20200623_Distribution.xlsx"
data<-read_excel(d_fname,sheet ="Data_Output", na="NA")
d_sectorlist<- read_excel(d_fname,sheet ="sector_list", na="NA")

#index key
key<-row.names(data)
data<-cbind(key, data)
#
#
d_sector<-select(data, c("key","A04 - Sector/القطاع","Value"))
#add sector count
#d_sector$n_sector<-str_count(d_sector$`A04 - Sector/القطاع`, pattern="/")

d_sector<-d_sector %>% 
  mutate(sector = strsplit(as.character(`A04 - Sector/القطاع`), ",")) %>% unnest(sector) %>% 
  mutate(sector=str_trim(sector))
#
d_sector<-d_sector %>% 
          left_join(d_sectorlist,by=c("sector"="sector"))

#count sector
d_s_count<-select(d_sector,c("key","sector_name")) %>% 
           distinct %>% 
           group_by(key) %>% 
           summarise(n_sector=n()) %>% 
           ungroup()
           
d_sector<-left_join(d_sector, d_s_count,by=c("key"="key"))
#now for calculation - take protection as one row
d_sector_val<-d_sector %>% 
              select("key","A04 - Sector/القطاع","sector_name", "Value", "n_sector") %>% 
              distinct %>% 
              mutate(val_sector=round(Value/n_sector,0))

#now bring back to the main table
data_val<-data %>% 
          left_join(select(d_sector_val, c("key", "sector_name","n_sector","val_sector")), by=c("key"="key"))


#---------------------------------------------------------#
save_fname<-gsub(".xlsx","_sectors.xlsx",d_fname)
wb<-createWorkbook()
addWorksheet(wb,"data_bysectors")
#write data
writeDataTable(wb,sheet="data_bysectors",x=data_val,tableName ="sectors")
saveWorkbook(wb,save_fname,overwrite = TRUE)
