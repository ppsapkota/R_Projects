
#Load libraries
source("./R/r_ps_library_init.R")

#----Define path------------------
d_fname<-"./Data/HF_fulldata_20170918.xlsx"


pcode_fname<-"./Data/admin.xlsx"
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

##--
Project_Time<-substr(d_projectdata$Project_Code,1,6)

##--
Allocation_Name<-d_projectdata[,c("Allocation_type")]
Allocation_Name<-as.data.frame(Allocation_Name)
Allocation_Name<-ifelse(Allocation_Name[,1]=="Rolling basis","Reserve Allocation",Allocation_Name[,1])
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
    #export data file to excel
    save_fname<-gsub(".xlsx","_updated.xlsx",d_fname)
    #
    wb<-createWorkbook()
    addWorksheet(wb,"ProjectData")
    addWorksheet(wb,"Location")
    addWorksheet(wb,"SRP")
    addWorksheet(wb,"Cluster")
    #
    writeDataTable(wb,sheet="ProjectData",x=d_projectdata,tableName ="ProjectData")
    writeDataTable(wb,sheet="Location",x=d_location,tableName = "Location")
    writeDataTable(wb,sheet="SRP",x=d_srp,tableName = "SRP")
    writeDataTable(wb,sheet="Cluster",x=d_cluster,tableName = "Cluster")
    #
    saveWorkbook(wb,save_fname,overwrite = TRUE)
    
    #--Cluster Budget Beneficiaries
    d_projectdata_cluster<-left_join(d_projectdata,d_cluster,by=c("Project_Code"="CHF_Code"))

##--ANALYSIS TABLES----
    save_summary_fname<-gsub(".xlsx","_summary.xlsx",d_fname)
    wb<-createWorkbook()
    addWorksheet(wb,"summary")
    addWorksheet(wb,"map_admin3_nproject")
    addWorksheet(wb,"map_admin1_budget")
    addWorksheet(wb,"map_admin3_cluster_ben")
    #
    i_startrow<-1
    
    #Add column for Budget per cluster
    d_projectdata_cluster$Budget_Cluster<-d_projectdata_cluster$Budget*d_projectdata_cluster$Percentage/100
    d_projectdata_cluster$Men_C<-round(d_projectdata_cluster$Beneficiaries_Men*d_projectdata_cluster$Percentage/100)
    d_projectdata_cluster$Women_C<-round(d_projectdata_cluster$Beneficiaries_Women*d_projectdata_cluster$Percentage/100)
    d_projectdata_cluster$Boys_C<-round(d_projectdata_cluster$Beneficiaries_Boys*d_projectdata_cluster$Percentage/100)
    d_projectdata_cluster$Girls_C<-round(d_projectdata_cluster$Beneficiaries_Girls*d_projectdata_cluster$Percentage/100)
    
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
                
    writeDataTable(wb,sheet="summary",x=cluster_budget_beneficiaries,tableName ="cluster_budget", startRow = i_startrow)
    i_startrow<-i_startrow+nrow(cluster_budget_beneficiaries)+5
    
    ###-Budget by allocation type
    budget_allocation_type<-d_projectdata %>% 
                            group_by(Allocation_type) %>% 
                            summarise(Budget=sum(Budget,na.rm=TRUE))
    
    writeDataTable(wb,sheet="summary",x=budget_allocation_type,tableName ="allocaton_type_budget", startRow = i_startrow)
    i_startrow<-i_startrow+nrow(budget_allocation_type)+5
    
    ###-Budget by cluster and allocation type
    cluster_budget_allocation_type<-d_projectdata_cluster %>% 
                                    group_by(Cluster, Allocation_type) %>%
                                    summarise(Budget=sum(Budget_Cluster,na.r=TRUE)) %>% 
                                    spread(Allocation_type,Budget, fill=0) %>%
                                    ungroup() %>% 
                                    mutate(Total=rowSums(.[-1])) %>% 
                                    arrange(desc(Total))
    writeDataTable(wb,sheet="summary",x=cluster_budget_allocation_type,tableName ="cluster_budget_allocaton_type", startRow = i_startrow)
    i_startrow<-i_startrow+nrow(cluster_budget_allocation_type)+5
    
    ###-Budget by organisation type
    #others to new national ngo
    d_projectdata_cluster$Org_Type<-gsub("Others", "National NGO",d_projectdata_cluster$Org_Type)
    budget_org_type<-d_projectdata_cluster %>% 
                     group_by(Org_Type) %>% 
                     summarise(Budget=sum(Budget*Percentage/100, na.rm=TRUE))
                                                                  
    writeDataTable(wb,sheet="summary",x=budget_org_type,tableName ="budget_org_type",startRow = i_startrow)
    i_startrow<-i_startrow+nrow(budget_org_type)+5
    
    ###-Number of projects by organisation type
    project_org_type_list<-d_projectdata_cluster[,c("Project_Code","Org_Type")]
    project_org_type_list<-distinct(project_org_type_list)
    project_org_type<-project_org_type_list %>% 
                      group_by(Org_Type) %>% 
                      summarise(nProjects=n())
    
    writeDataTable(wb,sheet="summary",x=project_org_type,tableName ="project_org_type",startRow = i_startrow)
    i_startrow<-i_startrow+nrow(project_org_type)+5
    
  #--LOCATION TABLE--
    #--Cluster Location
    d_cluster_t<-d_cluster #temp table
    names(d_cluster_t)<-gsub("Percentage","Percentage_C",names(d_cluster_t))
    d_location_cluster<-left_join(d_location,d_cluster_t,by=c("ChfProjectCode"="CHF_Code"))
    #bring project budget
    d_project_budget<-distinct(d_projectdata[,c("Project_Code","Allocation_type","Org_Type","Budget")])
    d_location_cluster<-left_join(d_location_cluster,d_project_budget,by=c("ChfProjectCode"="Project_Code"))
    #budget per cluster per location
    d_location_cluster$Budget_Loc_C<-as.numeric(d_location_cluster$Budget)*as.numeric(d_location_cluster$Percentage)/100*as.numeric(d_location_cluster$Percentage_C)/100
    #beneficiary per cluste per location note - Percentage in location table is for Budget distribution
    d_location_cluster$Men_Loc_C<-round(as.numeric(d_location_cluster$Men)*as.numeric(d_location_cluster$Percentage_C)/100)
    d_location_cluster$Women_Loc_C<-round(as.numeric(d_location_cluster$Women)*as.numeric(d_location_cluster$Percentage_C)/100)
    d_location_cluster$Boys_Loc_C<-round(as.numeric(d_location_cluster$Boys)*as.numeric(d_location_cluster$Percentage_C)/100)
    d_location_cluster$Girls_Loc_C<-round(as.numeric(d_location_cluster$Girls)*as.numeric(d_location_cluster$Percentage_C)/100)
    
    #write.xlsx(d_location_cluster,"./Data/a.xlsx")
    
    ###-projects per subdistrict
    location_project_list<-d_location_cluster[,c("ChfProjectCode","SubDistrict","SubDistrict_Pcode")]
    location_project_list<-distinct(location_project_list)
      #number of projects per subdistrict
      nprojects_subdistrict<-location_project_list %>% 
                             group_by(SubDistrict,SubDistrict_Pcode) %>% 
                             summarise(nProjects=n())
      writeDataTable(wb,sheet="map_admin3_nproject",x=nprojects_subdistrict,tableName ="admin3_nprojects")
    
    ###-sum of budget by governorate
    location_budget_gov<-d_location_cluster %>% 
                            group_by(Governorate_Pcode,Governorate) %>% 
                            summarise(Location_Budget=sum(Budget_Loc_C, na.rm=TRUE))
    location_budget_gov$Location_Budget_m<-location_budget_gov$Location_Budget/1000000
    writeDataTable(wb,sheet="map_admin1_budget",x=location_budget_gov,tableName ="admin1_budget") 
    
    ###--sum of beneficiaries by subdistrict per cluster
    admin3_cluster_beneficiary<-d_location_cluster %>% 
                                group_by(SubDistrict,SubDistrict_Pcode,Cluster) %>% 
                                summarise(Men=sum(Men_Loc_C,na.rm=TRUE),
                                          Women=sum(Women_Loc_C,na.rm=TRUE),
                                          Boys=sum(Boys_Loc_C,na.rm=TRUE),
                                          Girls=sum(Girls_Loc_C,na.rm=TRUE)) %>% 
                                ungroup() %>%
                                mutate(Total=rowSums(.[-1:-3],na.rm=TRUE))
    #now pivot
    #admin3_cluster_beneficiary_pivot<-select(admin3_cluster_beneficiary,1:3,8)
    admin3_cluster_beneficiary_pivot<-admin3_cluster_beneficiary %>%
                                select(1:3,8) %>% 
                                group_by(SubDistrict,SubDistrict_Pcode,Cluster) %>%
                                spread(Cluster,Total) %>% 
                                ungroup()
    
    admin3_cluster_beneficiary_pivot$Max_Ben<-apply(admin3_cluster_beneficiary_pivot[,-1:-2],1,function(x, na.rm=FALSE) {max(x, na.rm=TRUE)})
    
    writeDataTable(wb,sheet="map_admin3_cluster_ben",x=admin3_cluster_beneficiary_pivot,tableName ="admin3_cluster_ben") 
    #save file                     
    saveWorkbook(wb,save_summary_fname,overwrite = TRUE)
    
    
    
    
    
    