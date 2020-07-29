
'----
************************************
Developed by: Punya Prasad Sapkota
Last modified: 12 July 2017
************************************
#-----Merge multiple XLSX files
----'
source("./R/r_ps_library_init.R")




#---------------------------------------------------------#
files_merge_xlsx = function(mypath){
  #mypath <- datawd_csv
  filenames=list.files(path=mypath, full.names=TRUE, ignore.case = TRUE,  pattern = "*.xlsx|*.XLSX|*.xls|*.XLS",)
  #filenames<-Sys.glob(file.path(mypath, "*.xlsx"))
  all_files <- lapply(filenames, function(x) {read_excel(x,sheet="Matrix",col_types = "text")})
  all_files_merged <-Reduce(bind_rows,all_files)
  #returns the merged dataframe
  return(all_files_merged)
}





#--------Merge multiple xlsx files---------------------------
xlsx_path<-paste0("./Data/NWS_IDP")
d_merged<- as.data.frame(files_merge_xlsx(xlsx_path))
d_merged[is.na(d_merged)] <- 'NA'
openxlsx::write.xlsx(d_merged,paste0(xlsx_path,"/NWS_IDP_Merged.xlsx"),sheetName="NWS_IDP_Merged",row.names=FALSE)
