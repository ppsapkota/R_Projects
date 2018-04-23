# @Author: Punya Prasad Sapkota
# Generate summary statistics table for Humanitarian Country Baed Pooled Fund data
# Input: CBPF data dunmp 
# 
# Args: No specific arguments are required
# 
# 
# Returns: 
# 
#  

#Load libraries
source("C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\R\\r_ps_library_init.R")
#----Define path------------------
d_fname<-"C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\Data\\HF\\ProjectFullDump_ALL_2014_2017.xlsx"
pcode_fname<-"C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\Data\\admin.xlsx"

#for API
source("C:\\01_OCHA_TR\\03_IM_Tools\\R\\R_Projects\\R\\r_func_cbpf_api.R")
#initialisation
u<-''
pw<-''

##--Read data sheets-----------
d_projectdata<-read_excel(d_fname,sheet="ProjectData")
d_srp<-read_excel(d_fname,sheet="SRP")
d_location<-read_excel(d_fname,sheet="Location")
d_cluster<-read_excel(d_fname,sheet="Cluster")
#--Read PCODE file--
admin_pcode<-read_excel(pcode_fname,sheet="admin4")
##remove space in the field header
names(d_projectdata)<-gsub(" ","_",names(d_projectdata))
names(d_srp)<-gsub(" ","_",names(d_srp))
names(d_location)<-gsub(" ","_",names(d_location))
names(d_cluster)<-gsub(" ","_",names(d_cluster))

# # ##--FILTER PROJECTS for specific year
filter_year1<-"TUR-16"
filter_year2<-"TUR-17"
d_projectdata<-filter(d_projectdata,substr(Project_Code,1,6)==filter_year1 | substr(Project_Code,1,6)==filter_year2)
        d_srp<-filter(d_srp,substr(CHF_Code,1,6)==filter_year1 | substr(CHF_Code,1,6)==filter_year2)
   d_location<-filter(d_location, substr(ChfProjectCode,1,6)==filter_year1 | substr(ChfProjectCode,1,6)==filter_year2)
    d_cluster<-filter(d_cluster,substr(CHF_Code,1,6)==filter_year1 | substr(CHF_Code,1,6)==filter_year2)
#others to new national ngo
#d_projectdata$Org_Type<-gsub("Others", "National NGO",d_projectdata$Org_Type)

##--Project allocation time
Project_Time<-substr(d_projectdata$Project_Code,1,6)
##--
Allocation_Name<-d_projectdata[,c("Allocation_type")]
Allocation_Name<-as.data.frame(Allocation_Name)
Allocation_Name<-ifelse(Allocation_Name$Allocation_type=="Rolling basis","Reserve Allocation",Allocation_Name$Allocation_type)
Allocation_Name<-as.data.frame(Allocation_Name)
Allocation_Name<-ifelse(grepl("Standard Allocation",Allocation_Name$Allocation_Name),"Standard Allocation",Allocation_Name$Allocation_Name)
Allocation_Name<-as.data.frame(Allocation_Name)
Allocation_Name<-ifelse(grepl("Emergency Allocation",Allocation_Name$Allocation_Name),"Reserve Allocation",Allocation_Name$Allocation_Name)
Allocation_Name<-as.data.frame(Allocation_Name)
##--
d_projectdata<-cbind(Project_Time,Allocation_Name,d_projectdata)

##--Location sheet----
d_location<-as.data.frame(d_location)
d_location<-d_location[1:10]

ind_col_perc<-which(names(d_location)=="Percentage")
d_location$Percentage<-gsub("%",'',d_location$Percentage)
#-location data
for (i in 1:2){
  d_location$Location<-str_replace_all(d_location$Location,c("\\?"=''," _"='',"  ->"="->","->  "="->"," ->"="->","-> "="->", "\\("='',"\\)"=''))
}
d_location$Location<-gsub("Jebel Saman Aleppo", 'Jebel Saman',d_location$Location)
#*split type column - Punya
d_location<-separate(d_location,Location,into = c("Governorate","District","SubDistrict","Community"),sep="->", remove=FALSE,extra="drop", fill="right")
#trim texts
d_location$Governorate<-str_trim(d_location$Governorate, side=c("both"))
d_location$District<-str_trim(d_location$District, side=c("both"))
d_location$SubDistrict<-str_trim(d_location$SubDistrict, side=c("both"))
d_location$Community<-str_trim(d_location$Community, side=c("both"))

#join with the admin names
admin1<-distinct(admin_pcode[,c("admin1Pcode","admin1Name_en")])
admin2<-distinct(admin_pcode[,c("admin2Pcode","admin2Name_en")])
admin3<-distinct(admin_pcode[,c("admin3Pcode","admin3Name_en","admin1Pcode")])
admin4<-distinct(admin_pcode[,c("admin4Pcode","admin4Name_en","admin3Pcode")])
#Join with the location table one by one
d_location<-left_join(d_location,admin1,by=c("Governorate"="admin1Name_en"))
d_location<-left_join(d_location,admin2,by=c("District"="admin2Name_en"))
d_location<-left_join(d_location,admin3,by=c("admin1Pcode"="admin1Pcode","SubDistrict"="admin3Name_en"))
d_location<-left_join(d_location,admin4,by=c("admin3Pcode"="admin3Pcode","Community"="admin4Name_en"))
d_location<-as.data.frame(d_location)
#rename columns
d_location<- rename(d_location,"Governorate_Pcode"="admin1Pcode")
d_location<- rename(d_location,"District_Pcode"="admin2Pcode")
d_location<- rename(d_location,"SubDistrict_Pcode"="admin3Pcode")
d_location<- rename(d_location,"Community_Pcode"="admin4Pcode")
#reorder fields
d_location<-select(d_location,1:6,15:18,7:14)
###----Project cluster and Location table preparation   
###--MAIN TABLE---->>>CLUSTER - BUDGET - BENEFICIARY
d_projectdata_cluster<-inner_join(d_projectdata,d_cluster,by=c("Project_Code"="CHF_Code"))
#Add column for Budget per cluster
d_projectdata_cluster$Budget_Cluster<-d_projectdata_cluster$Budget*d_projectdata_cluster$Percentage/100
d_projectdata_cluster$Men_C<-round(d_projectdata_cluster$Beneficiaries_Men*d_projectdata_cluster$Percentage/100)
d_projectdata_cluster$Women_C<-round(d_projectdata_cluster$Beneficiaries_Women*d_projectdata_cluster$Percentage/100)
d_projectdata_cluster$Boys_C<-round(d_projectdata_cluster$Beneficiaries_Boys*d_projectdata_cluster$Percentage/100)
d_projectdata_cluster$Girls_C<-round(d_projectdata_cluster$Beneficiaries_Girls*d_projectdata_cluster$Percentage/100)
#
d_projectdata_cluster$Beneficiaries<-d_projectdata_cluster$Men_C+
  d_projectdata_cluster$Women_C+
  d_projectdata_cluster$Boys_C+
  d_projectdata_cluster$Girls_C


#API Project cluster planned/actual
#Beneficiary reached - project report
url<-"https://cbpfapi.unocha.org/vo1/odata/NarrativeReportBeneficiary?poolfundAbbrv=TUR70&$format=csv"
d_project_ben_reached<-restapi_getdata_csv(url, u, pw)
d_project_ben_reached<-d_project_ben_reached[,c("ChfProjectCode","BeneficiaryName","PlannedMen","PlannedWomen","PlannedBoys","PlannedGirls","PlannedTotal",
                                                "ActualMen", "ActualWomen","ActualBoys","ActualGirls","ActualTotal")]

#Project summaries - status
url <- "https://cbpfapi.unocha.org/vo1/odata/ProjectSummary?poolfundAbbrv=TUR70&$format=csv"
d_projectsummary<-restapi_getdata_csv(url, u, pw)
d_projectsummary_prjstatus<-d_projectsummary[,c("ChfProjectCode","ProjectStatus","ProcessStatus")]

#Bring project status in the project cluster sheet
d_projectdata_cluster<-left_join(d_projectdata_cluster,d_projectsummary_prjstatus, by=c("Project_Code"="ChfProjectCode"))
#
d_projectdata_cluster_ben_reached<-left_join(d_projectdata_cluster[,c("Project_Time","Allocation_Name","Project_Code","Organization","Org_Type","ProjectStatus","ProcessStatus","Cluster","Percentage")],d_project_ben_reached, by=c("Project_Code"="ChfProjectCode"))

#Distribute planned beneficiaries by cluster
d_projectdata_cluster_ben_reached$PlannedMen_C<-round(d_projectdata_cluster_ben_reached$PlannedMen*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$PlannedWomen_C<-round(d_projectdata_cluster_ben_reached$PlannedWomen*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$PlannedBoys_C<-round(d_projectdata_cluster_ben_reached$PlannedBoys*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$PlannedGirls_C<-round(d_projectdata_cluster_ben_reached$PlannedGirls*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$PlannedTotal_C<-round(d_projectdata_cluster_ben_reached$PlannedTotal*d_projectdata_cluster_ben_reached$Percentage/100)

#Distribute actual beneficiaries by cluster
d_projectdata_cluster_ben_reached$ActualMen_C<-round(d_projectdata_cluster_ben_reached$ActualMen*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$ActualWomen_C<-round(d_projectdata_cluster_ben_reached$ActualWomen*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$ActualBoys_C<-round(d_projectdata_cluster_ben_reached$ActualBoys*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$ActualGirls_C<-round(d_projectdata_cluster_ben_reached$ActualGirls*d_projectdata_cluster_ben_reached$Percentage/100)
d_projectdata_cluster_ben_reached$ActualTotal_C<-round(d_projectdata_cluster_ben_reached$ActualTotal*d_projectdata_cluster_ben_reached$Percentage/100)

#Sum per project
d_projectdata_cluster_ben_reached <- d_projectdata_cluster_ben_reached %>% 
  group_by(Project_Time,Allocation_Name,Project_Code,Organization,Org_Type,Cluster,Percentage) %>% 
  summarise(PlannedMen_C = sum(PlannedMen_C, na.rm = TRUE),
            PlannedWomen_C = sum(PlannedWomen_C, na.rm = TRUE),
            PlannedBoys_C = sum(PlannedBoys_C, na.rm = TRUE),
            PlannedGirls_C = sum(PlannedGirls_C, na.rm = TRUE),
            PlannedTotal_C = sum(PlannedTotal_C, na.rm = TRUE),
            ActualMen_C = sum(ActualMen_C, na.rm = TRUE),
            ActualWomen_C = sum(ActualWomen_C, na.rm = TRUE),
            ActualBoys_C = sum(ActualBoys_C, na.rm = TRUE),
            ActualGirls_C = sum(ActualGirls_C, na.rm = TRUE),
            ActualTotal_C = sum(ActualTotal_C, na.rm = TRUE)) %>% 
  ungroup()


###--MAIN TABLE ----->>>>>LOCATION - CLUSTER - BUDGET - BENEFICIARY
#--bring Cluster to Location
d_cluster_t<-d_cluster #temp table
names(d_cluster_t)<-gsub("Percentage","Percentage_C",names(d_cluster_t))
d_location_cluster<-inner_join(d_location,d_cluster_t,by=c("ChfProjectCode"="CHF_Code"))
#bring project budget to location
d_project_budget<-distinct(d_projectdata[,c("Project_Code","Project_Time","Allocation_Name","Allocation_Name","Organization", "Org_Type","Budget")])
d_location_cluster<-inner_join(d_location_cluster,d_project_budget,by=c("ChfProjectCode"="Project_Code"))
#budget per cluster per location
d_location_cluster$Budget_Loc_C<-as.numeric(d_location_cluster$Budget)*as.numeric(d_location_cluster$Percentage)/100*as.numeric(d_location_cluster$Percentage_C)/100
#beneficiary per cluste per location note - Percentage in location table is for Budget distribution
d_location_cluster$Men_Loc_C<-round(as.numeric(d_location_cluster$Men)*as.numeric(d_location_cluster$Percentage_C)/100)
d_location_cluster$Women_Loc_C<-round(as.numeric(d_location_cluster$Women)*as.numeric(d_location_cluster$Percentage_C)/100)
d_location_cluster$Boys_Loc_C<-round(as.numeric(d_location_cluster$Boys)*as.numeric(d_location_cluster$Percentage_C)/100)
d_location_cluster$Girls_Loc_C<-round(as.numeric(d_location_cluster$Girls)*as.numeric(d_location_cluster$Percentage_C)/100)

   
###
    ###-Budget and Beneficiaries by cluster
    cluster_budget_beneficiaries<-d_projectdata_cluster %>% 
                                    group_by(Cluster) %>% 
                                    summarise(Budget=sum(Budget_Cluster, na.rm=TRUE),
                                              Men=round(sum(Beneficiaries_Men*Percentage/100, na.rm=TRUE)),
                                              Women=round(sum(Beneficiaries_Women*Percentage/100, na.rm=TRUE)),
                                              Boys=round(sum(Beneficiaries_Boys*Percentage/100, na.rm=TRUE)),
                                              Girls=round(sum(Beneficiaries_Girls*Percentage/100, na.rm=TRUE))
                                              ) %>% 
                                    arrange(desc(Budget))
   
    ben_v<-data.frame(Beneficiary=NA)
    cluster_budget_beneficiaries<-as.data.frame(append(cluster_budget_beneficiaries,ben_v,after=2))
    #sum values
    cluster_budget_beneficiaries$Beneficiary<-cluster_budget_beneficiaries$Men+
                                              cluster_budget_beneficiaries$Women+
                                              cluster_budget_beneficiaries$Boys+
                                              cluster_budget_beneficiaries$Girls
                
    
    ###-Budget by allocation type
    budget_allocation_type<-d_projectdata %>% 
                            group_by(Allocation_Name) %>% 
                            summarise(Budget=sum(Budget,na.rm=TRUE))
    
    
    ###-Budget by cluster and allocation type
    cluster_budget_allocation_type<-d_projectdata_cluster %>% 
                                    group_by(Cluster, Allocation_Name) %>%
                                    summarise(Budget=sum(Budget_Cluster,na.r=TRUE)) %>% 
                                    spread(Allocation_Name,Budget, fill=0) %>%
                                    ungroup() %>% 
                                    mutate(Total=rowSums(.[-1])) %>% 
                                    arrange(desc(Total))
   
    
    ###-Budget by organisation type
    budget_org_type<-d_projectdata_cluster %>% 
                     group_by(Org_Type) %>% 
                     summarise(Budget=sum(Budget*Percentage/100, na.rm=TRUE))
                                                                  
    
    
    ###-Number of projects by organisation type
    project_org_type_list<-d_projectdata_cluster[,c("Project_Code","Org_Type")]
    project_org_type_list<-distinct(project_org_type_list)
    project_org_type<-project_org_type_list %>% 
                      group_by(Org_Type) %>% 
                      summarise(nProjects=n())
    
    
    
###--PARTNERS
    ###-Number of PARTNERS by organisation type
    partner_org_type_list<-distinct(d_projectdata_cluster[,c("Organization","Org_Type")])
    partner_org_type<-partner_org_type_list %>% 
      group_by(Org_Type) %>% 
      summarise(nPartner=n()) %>% 
      ungroup()
  
    ###-Number of PARTNERS by organisation type and CLUSTER
    partner_org_type_cluster_list<-distinct(d_projectdata_cluster[,c("Organization","Org_Type","Cluster")])
    partner_org_type_cluster<-partner_org_type_cluster_list %>% 
      group_by(Cluster, Org_Type) %>% 
      summarise(nPartner=n()) %>% 
      spread(Org_Type,nPartner) %>% 
      ungroup()
    
###--LOCATION TABLE--
###-projects per subdistrict
    location_project_list<-distinct(d_location_cluster[,c("ChfProjectCode","Governorate","District", "SubDistrict", "Governorate_Pcode","District_Pcode","SubDistrict_Pcode")])
      #number of projects per subdistrict
      nprojects_subdistrict<-location_project_list %>% 
                             group_by(Governorate,Governorate_Pcode,District, District_Pcode, SubDistrict,SubDistrict_Pcode) %>% 
                             summarise(nProjects=n())
###-PARTNERS per subdistrict      
      
      ###-partners per subdistrict
      location_partner_list<-distinct(d_location_cluster[,c("Organization","Governorate","District", "SubDistrict", "Governorate_Pcode","District_Pcode","SubDistrict_Pcode")])
      #number of partners per subdistrict
      npartner_subdistrict<-location_partner_list %>% 
        group_by(Governorate,Governorate_Pcode,District, District_Pcode, SubDistrict,SubDistrict_Pcode) %>% 
        summarise(nPartner=n()) %>% 
        ungroup()
      ###-partners per subdistrict by organisation type
      location_partner_type_list<-distinct(d_location_cluster[,c("Organization","Org_Type","Governorate","District", "SubDistrict", "Governorate_Pcode","District_Pcode","SubDistrict_Pcode")])
      #number of partners per subdistrict
      npartner_type_subdistrict<-location_partner_type_list %>% 
        group_by(Governorate,Governorate_Pcode,District, District_Pcode, SubDistrict,SubDistrict_Pcode,Org_Type) %>% 
        summarise(nPartner=n()) %>%
        spread(Org_Type,nPartner) %>% 
        ungroup()
    ###-PARTNERS per subdistrict per cluster
      location_partner_cluster_list<-distinct(d_location_cluster[,c("Cluster","Organization","Governorate","District", "SubDistrict", "Governorate_Pcode","District_Pcode","SubDistrict_Pcode")])
      #number of partners per subdistrict per cluster
      npartner_cluster_subdistrict<-location_partner_cluster_list %>% 
        group_by(Cluster,Governorate,Governorate_Pcode,District, District_Pcode, SubDistrict,SubDistrict_Pcode) %>% 
        summarise(nPartner=n()) %>% 
        spread (Cluster,nPartner) %>% 
        ungroup()
      
      #Join two tables
      npartner_cluster_subdistrict<-inner_join(npartner_cluster_subdistrict,select(npartner_subdistrict,2,4,6,7),by=c("Governorate_Pcode"="Governorate_Pcode","District_Pcode"="District_Pcode","SubDistrict_Pcode"="SubDistrict_Pcode"))
      
    ###-PARTNERS per governorate
      location_admin1_partner_list<-distinct(d_location_cluster[,c("Governorate","Organization")])
      #number of partners per governorate
      npartner_admin1<-location_admin1_partner_list %>% 
        group_by(Governorate) %>% 
        summarise(nPartner=n())    
    
    ###-PARTNERS per governorate per cluster
      location_admin1_partner_cluster_list<-distinct(d_location_cluster[,c("Governorate","Cluster","Organization")])
      #number of partners per governorate per cluster
      npartner_admin1_cluster<-location_admin1_partner_cluster_list %>% 
        group_by(Cluster,Governorate) %>% 
        summarise(nPartner=n()) %>% 
        spread(Cluster,nPartner)
    
      ###-PARTNERS per governorate and org type
      location_admin1_partner_type_list<-distinct(d_location_cluster[,c("Governorate","Org_Type","Organization")])
      #number of partners per governorate per cluster
      npartner_admin1_org_type<-location_admin1_partner_type_list %>% 
        group_by(Org_Type,Governorate) %>% 
        summarise(nPartner=n()) %>% 
        spread(Org_Type,nPartner)
###-BUDGET sum of budget by governorate
    location_budget_gov<-d_location_cluster %>% 
                            group_by(Governorate_Pcode,Governorate) %>% 
                            summarise(Location_Budget=sum(Budget_Loc_C, na.rm=TRUE))
    location_budget_gov$Location_Budget_m<-location_budget_gov$Location_Budget/1000000
    
    ###SUM of budget by governorate per cluster
    ###-sum of budget by governorate
    location_budget_gov<-d_location_cluster %>% 
      group_by(Governorate_Pcode,Governorate,Cluster) %>% 
      summarise(Location_Budget=sum(Budget_Loc_C, na.rm=TRUE)) %>% 
      spread(Cluster,Location_Budget) 
    
    ###--sum of beneficiaries by subdistrict per cluster
    admin3_cluster_beneficiary<-d_location_cluster %>% 
                                group_by(Governorate,Governorate_Pcode,District, District_Pcode,SubDistrict,SubDistrict_Pcode,Cluster) %>% 
                                summarise(Men=sum(Men_Loc_C,na.rm=TRUE),
                                          Women=sum(Women_Loc_C,na.rm=TRUE),
                                          Boys=sum(Boys_Loc_C,na.rm=TRUE),
                                          Girls=sum(Girls_Loc_C,na.rm=TRUE)) %>% 
                                ungroup() %>%
                                mutate(Total=rowSums(.[-1:-7],na.rm=TRUE))
    #Pivot
    #admin3_cluster_beneficiary_pivot<-select(admin3_cluster_beneficiary,1:3,8)
    admin3_cluster_beneficiary_pivot<-admin3_cluster_beneficiary %>%
                                select(1:7,12) %>% 
                                group_by(Governorate,Governorate_Pcode,District, District_Pcode,SubDistrict,SubDistrict_Pcode,Cluster) %>%
                                spread(Cluster,Total) %>% 
                                ungroup()
    
    admin3_cluster_beneficiary_pivot$Max_Ben<-apply(admin3_cluster_beneficiary_pivot[,-1:-6],1,function(x, na.rm=FALSE) {max(x, na.rm=TRUE)})
    
    
    
    ###--sum of Budget by subdistrict per cluster
    admin3_cluster_budget<-d_location_cluster %>% 
      group_by(Governorate,Governorate_Pcode,District, District_Pcode,SubDistrict,SubDistrict_Pcode,Cluster) %>% 
      summarise(Location_Budget=sum(Budget_Loc_C,na.rm=TRUE)) %>% 
      ungroup()
    
    #now pivot
    admin3_cluster_budget_pivot<-admin3_cluster_budget %>%
      group_by(Governorate,Governorate_Pcode,District, District_Pcode,SubDistrict,SubDistrict_Pcode,Cluster) %>%
      spread(Cluster,Location_Budget) %>% 
      ungroup()
    #add total budget field
    admin3_cluster_budget_pivot$Location_Budget<-apply(admin3_cluster_budget_pivot[,-1:-6],1,function(x, na.rm=FALSE) {sum(x, na.rm=TRUE)})
   
    
    
    
    