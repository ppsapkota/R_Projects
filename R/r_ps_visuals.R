#Load libraries
source("./R/r_ps_library_init.R")
##-----GRAPHS and CHARTS---------
    dataset<-cluster_budget_beneficiaries
    #ggplot 2 theme
    ggplot2theme<-theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_text(angle=0, hjust=1, size=10, vjust=0.5, margin=margin(0,0,0,0)),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
    
    #bar chart budget per cluster
    #bar chart budget per cluster
    p<- ggplot(data=dataset,aes(Cluster,Budget)) + 
      geom_bar(stat="sum",width=0.75,fill=rgb(100,190,225, maxColorValue = 255),position = 'dodge') +
      geom_text(stat="sum",aes(y=0,label=format(round(Budget/1000000,1),big.mark=",",scientific=FALSE)),hjust=-0.25, vjust=0.5,size=4,na.rm = FALSE,show.legend = NA)+
      scale_x_discrete(labels=function(x){str_wrap(x,width = 20)})+
      ggplot2theme +
      coord_flip()
    p
    
    #bar chart
     
