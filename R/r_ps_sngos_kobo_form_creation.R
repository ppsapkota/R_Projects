rm(list = ls())

source("./R/r_ps_library_init.R")

f_kobo_fname<-"C:\\Dropbox (OCHA)\\GIS\\01_Analysis_Projects\\02_Thematic\\4W\\SNGOs\\kobo_form\\SCSO_Form_20180220_V1_0.xlsx"
f_org_kobo_un<-"C:\\Dropbox (OCHA)\\GIS\\01_Analysis_Projects\\02_Thematic\\4W\\SNGOs\\kobo_form\\partner_list\\SCSO_partner_list_usernames_kobo.xlsx"
f_save_location <- "C:\\Dropbox (OCHA)\\GIS\\01_Analysis_Projects\\02_Thematic\\4W\\SNGOs\\kobo_form\\kobo_partners_forms\\"

#read sheets
data_survey<-read_excel(f_kobo_fname,sheet="survey")
data_choices<-read_excel(f_kobo_fname,sheet="choices")
data_settings<-read_excel(f_kobo_fname,sheet="settings")

#read file for the list of organisation and kobo user name
data <- read_excel(f_org_kobo_un)

for (i in 1:nrow(data))
{
  #i<-1
  data_survey_i <-data_survey
  data_choices_i <- data_choices
  data_settings_i <- data_settings
  
  ####get the code and names
  org_code <- data$org_code[i]
  org_name_en <- data$org_name_en[i]
  org_name_ar <- data$org_name_ar[i]
  org_kobo_un <- data$username_kobo[i]
  org_acronym <- data$org_acronym[i]
  
  #prepare file name
  f_kobo_savename <- paste0(f_save_location,org_code,"_",org_acronym,"_","SCSO_Form_20180220_V1_0.xlsx")
  
  #SETTINGS sheet - add org name and make the id unique
  data_settings_i$form_title <- paste0(org_code,"_",org_acronym,"_",data_settings_i$form_title)
  data_settings_i$id_string <- paste0(org_code,"_",org_acronym,"_","SNGO_3W")
  
  #SURVEY sheet - add org name en/ar as default
  i_row <- which(data_survey_i$name=="org_ar_name")
  data_survey_i$default[i_row] <- org_name_ar
  #
  i_row <- which(data_survey_i$name=="org_en_name")
  data_survey_i$default[i_row] <- org_name_en

#NOW save file
  wb<-createWorkbook()
  addWorksheet(wb,"survey")
  addWorksheet(wb,"choices")
  addWorksheet(wb,"settings")
  #
  writeData(wb,sheet="survey",x=data_survey_i)
  writeData(wb,sheet="choices",x=data_choices_i)
  writeData(wb,sheet="settings",x=data_settings_i)
  #
  saveWorkbook(wb,f_kobo_savename,overwrite = TRUE)
}
 




