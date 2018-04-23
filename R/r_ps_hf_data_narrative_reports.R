# @Author: Punya Prasad Sapkota
# Generate summary statistics table for Humanitarian Country Baed Pooled Fund data
# Input: CBPF data dunmp 
# 
# Args: No specific arguments are required
# 
# 
# Returns: 
# 
#Load libraries
source("C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\R\\r_ps_library_init.R")
#----Define path------------------
d_fname<-"C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\Data\\HF\\Narrative_Report_Analysis_2017.xlsx"
pcode_fname<-"C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\Data\\admin.xlsx"

d_fname_save<-gsub(".xlsx", "_REACHED.xlsx",d_fname)

#for API
source("./R/r_func_cbpf_api.R")
#initialisation
u<-''
pw<-''


##--Read data sheets-----------
d_beneficiary<-read_excel(d_fname,sheet="Beneficiaries")
d_cluster<-read_excel(d_fname,sheet="Cluster")
##remove space in the field header
names(d_beneficiary)<-gsub(" ","_",names(d_beneficiary))
names(d_beneficiary)<-gsub("/","_",names(d_beneficiary))
names(d_cluster)<-gsub(" ","_",names(d_cluster))

#rename cluster field
d_beneficiary<-rename(d_beneficiary,"Cluster_X"="Cluster")
d_beneficiary_cluster<-left_join(d_beneficiary,d_cluster,by=c("Fund_Code" = "CHF_Code"))
#
d_beneficiary_cluster$Men_REACHED_C = d_beneficiary_cluster$Men_REACHED * d_beneficiary_cluster$Reached_PERC_2017_Annual_Report * d_beneficiary_cluster$Percentage/100
d_beneficiary_cluster$Women_REACHED_C = d_beneficiary_cluster$Women_REACHED * d_beneficiary_cluster$Reached_PERC_2017_Annual_Report * d_beneficiary_cluster$Percentage/100
d_beneficiary_cluster$Boys_REACHED_C = d_beneficiary_cluster$Boys_REACHED * d_beneficiary_cluster$Reached_PERC_2017_Annual_Report * d_beneficiary_cluster$Percentage/100
d_beneficiary_cluster$Girls_REACHED_C = d_beneficiary_cluster$Girls_REACHED * d_beneficiary_cluster$Reached_PERC_2017_Annual_Report * d_beneficiary_cluster$Percentage/100


openxlsx::write.xlsx(d_beneficiary_cluster,file = d_fname_save,asTable = TRUE)


