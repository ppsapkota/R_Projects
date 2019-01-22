rm(list = ls())

source("./R/r_ps_library_init.R")


f_name<-"./Data/newlyadded_admin4/syr_pplp_adm4_unocha_thiessen_ply_newlyadded.xlsx"
s_name<-gsub(".xlsx","_thiessen_ply.xlsx",f_name)
#read data
data<-read_excel(f_name)

#summary
d_summary_num<-data %>%
               filter(admin4Pcode!=admin4Pcode_old) %>% 
               group_by(admin3Name_en_old,admin3Pcode_old,admin4Name_en_old,admin4Pcode_old) %>% 
               summarise(num_comm=n()) %>% 
               ungroup()

d_summary_list<-data %>%
                filter(admin4Pcode!=admin4Pcode_old) %>% 
                mutate(admin4Pcode_Name=paste0(admin4Name_en,"|",admin4Pcode)) %>% 
                group_by(admin1Name_en_old, admin3Name_en_old,admin3Pcode_old,admin4Name_en_old,admin4Pcode_old) %>% 
                summarise(list_comm=paste0(admin4Pcode_Name,collapse = ",")) %>% 
                ungroup()
  
d_summary_num_list<- d_summary_list %>% 
                     left_join(select(d_summary_num,admin4Pcode_old,num_comm),by="admin4Pcode_old") %>% 
                     separate(list_comm,into=paste("C",1:25,sep="_"),sep=",",remove=FALSE)


write.xlsx(d_summary_num_list,s_name,asTable=TRUE)


