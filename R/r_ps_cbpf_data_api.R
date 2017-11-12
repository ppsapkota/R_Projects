
'-
***********************************************
Developed by: Punya Prasad Sapkota
Last Modified: 11 November 2017
***********************************************
#---USAGE
#-----Country Based Pooled Fund data Access using API v1
#-----https://cbpfapi.unocha.org/vo1/
#-----USE CSV FORMAT
-'

source("./R/r_ps_library_init.R")
source("./R/r_func_cbpf_api.R")
#initialisation
u<-''
pw<-''

d_path<-"./Data/HF/"
d_day<-format(Sys.Date(),"%Y%m%d")

#CBPF Contibution
  url<-"https://cbpfapi.unocha.org/vo1/odata/Contribution?poolfundAbbrv=TUR70&$format=csv"
  
  d_contribution<-restapi_getdata_csv(url, u, pw)
  
  write.xlsx(d_contribution,paste0(d_path,"d_contribution_",d_day,".xlsx"),sheetName="contribution")

#Project summaries
  url <- "https://cbpfapi.unocha.org/vo1/odata/ProjectSummary?poolfundAbbrv=TUR70&$format=csv"
  d_projectsummary<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_projectsummary,paste0(d_path,"d_projectsummary_",d_day,".xlsx"),sheetName="projectsummary")
#
  url <- "https://cbpfapi.unocha.org/vo1/odata/ProjectLocationActivities?poolfundAbbrv=TUR70&AdminlocationLevel=1_2_3_4_5&$format=csv"
  d_projectlocation<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_projectlocation,paste0(d_path,"d_projectlocation_",d_day,".xlsx"),sheetName="projectlocation")
  
#Score card - Partner
  url<-"https://cbpfapi.unocha.org/vo1/odata/CBPFPartnerScorecard?poolfundAbbrv=TUR70&$format=csv"
  d_partnerscorecard<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_partnerscorecard,paste0(d_path,"d_partner_score_card_",d_day,".xlsx"),sheetName="partner_score_card")
#Score card - Project
  url<-"https://cbpfapi.unocha.org/vo1/odata/CBPFProjectScorecard?poolfundAbbrv=TUR70&AllocationYear=2017_2016&$format=csv"
  d_projectscorecard<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_projectscorecard,paste0(d_path,"d_project_score_card_",d_day,".xlsx"),sheetName="project_score_card")

#Beneficiary reached - project report
  url<-"https://cbpfapi.unocha.org/vo1/odata/NarrativeReportBeneficiary?poolfundAbbrv=TUR70&$format=csv"
  d_project_ben_reached<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_project_ben_reached,paste0(d_path,"d_project_ben_reached_",d_day,".xlsx"),sheetName="project_ben_reached")
#Project all locations
  url<-"https://cbpfapi.unocha.org/vo1/odata/ProjectAllLocationsByActivity?poolfundAbbrv=TUR70&$format=csv"
  d_project_activity_location<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_project_activity_location,paste0(d_path,"d_project_activity_location_",d_day,".xlsx"),sheetName="location")
#Project
  url<-"https://cbpfapi.unocha.org/vo1/odata/ProjectLocation?poolfundAbbrv=TUR70&$format=csv"
  d_project_location<-restapi_getdata_csv(url, u, pw)
  write.xlsx(d_project_activity_location,paste0(d_path,"d_project_activity_location_",d_day,".xlsx"),sheetName="location")
  
  url<-"https://cbpfapi.unocha.org/vo1/odata/ProjectLocationLevel?poolfundAbbrv=TUR70&AdminlocationLevel=1_2_3_4&$format=csv"
  d_project_location<-restapi_getdata_csv(url, u, pw)
#
  url<-"https://cbpfapi.unocha.org/vo1/odata/ProjectLocationActivities?poolfundAbbrv=TUR70&AdminlocationLevel=1_2_3_4&$format=csv"
  d_project_location<-restapi_getdata_csv(url, u, pw)
  
  
  
  