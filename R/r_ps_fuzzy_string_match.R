library(stringdist)

std_admin3<-admin3
string2match<-c("idlib","Hasakeh","Raqqa")
dist_similarity<-1-stringdistmatrix (toupper(std_admin3$admin3Name_en),toupper(string2match),method='jw',p=0.25)
#dist_similarity<-stringsim(toupper(std_admin1$admin1Name_en),toupper(string2match),method='lv')
d<-as.data.frame(dist_similarity)
m_str<-apply(dist_similarity, 1, base::max)
