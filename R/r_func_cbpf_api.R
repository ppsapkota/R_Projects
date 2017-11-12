'-
***********************************************
Developed by: Punya Prasad Sapkota
Last Modified: 11 November 2017
***********************************************
#---USAGE
#-----Country Based Pooled Fund data Access using API v1
#-----https://cbpfapi.unocha.org/vo1/

-'
#user names and password to be loaded from external authenticate file - this approach to be checked

#returns list of forms as a dataframe
#url <- "https://cbpfapi.unocha.org/vo1/odata/ProjectSummary?poolfundAbbrv=TUR70"
#url <- "https://cbpfapi.unocha.org/vo1/odata/ProjectSummary?poolfundAbbrv=TUR70&$format=csv"
#baseurl<-"https://cbpfapi.unocha.org/vo1/odata/"

#returns the CSV content of the form

restapi_getdata_csv<-function(url,u,pw){
  #supply url for the data
  rawdata<-GET(url,authenticate(u,pw),progress())
  #rawdata<-GET(url,progress())
  cat("\n")
  d_content <- read_csv(content(rawdata,"raw",encoding = "UTF-8"))
}
