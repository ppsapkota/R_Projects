#Load libraries
source("./R/r_ps_library_init.R")
d_fname<-"./Data/Viz/data_viz.xlsx"
d_sankey_gviz<-read_excel(d_fname,sheet="data_sankey_gviz")

#--------GOOGLEVIZ SANKEY---------------------
colors_link<-distinct(d_sankey_gviz[,c("O_Color")])
colors_node<-d_sankey_gviz$O_Color
colors_node_array<-paste0("[",paste0("'",colors_node,"'",collapse = ","),"]")
colors_link_array<-paste0("[",paste0("'",colors_link$O_Color,"'",collapse = ","),"]")

opts <- paste0("{
        link: {colorMode: 'source',
               colors: ", colors_link_array  ," },
        node: {colors: ", colorr_node_array ,",width:'50' }
      }")
#Use googlevis
p<-gvisSankey(data=d_sankey_gviz[,c("Origin","Destination","Value")],from="Origin",to="Destination",weight="Value",options = list(height=400,sankey=opts,vAxis="[{format:'#,###%'},{title:'val1'}, {title:'val2'}]"))
plot(p)


#-----------NETWORK3D-----------------
s_nodes<-as.data.frame(distinct(d_sankey[,c("ID")]))
s_links<-as.data.frame(d_sankey[,c("Origin","Destination","Value")])
#-----------
sankeyNetwork(Links = s_links, Nodes = s_nodes,
              Source = "Origin", Target = "Destination",
              Value = "Value", NodeID = "ID",
              fontSize= 12, nodeWidth = 30)

#-----------------------------------------------------------
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
# Plot
links<-Energy$links
nodes<-Energy$nodes
sankeyNetwork(Links = links, Nodes = nodes , Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


#--------------network3D---------
library(networkD3)
d_links<-read_excel(d_fname,sheet="network3d_links")
d_nodes<-read_excel(d_fname,sheet="network3d_nodes")

simpleNetwork(d_n3d[,c("source","target")])
# Plot
d_links<-d_links[,c("source","target","value")]
d_nodes<-d_nodes[,c("name","group","size")]
forceNetwork(Links = d_links, Nodes = d_nodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 1)


#sankey
# Plot
sankeyNetwork(Links = d_links, Nodes = d_nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)

























#--SAMPLE----
data("MisLinks")
data("MisNodes")
glimpse(MisLinks)
glimpse(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

#
forceNetwork(Links = MisLinks, Target = "target", Value = "value", 
             Nodes = MisNodes, Source = "source", NodeID = "name", Group = "group", 
             opacity = 0.4, zoom = TRUE)





#-------RADIAL-----------
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

# Use subset of data for more readable diagram
Flare$children = Flare$children[1:3]
radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)
diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)



#-------------------------------------------------------------

#Google Viz Sankey

dat <- data.frame(From=c(rep("A",3), rep("B", 3)), 
                  To=c(rep(c("X", "Y", "Z"),2)), 
                  Weight=c(5,7,6,2,9,4))


sk1 <- gvisSankey(dat, from="From", to="To", weight="Weight")
plot(sk1)

sk2 <- gvisSankey(dat, from="From", to="To", weight="Weight",
                  options=list(sankey="{link: {color: { fill: '#d799ae' } },
                                     node: { color: { fill: '#a61d4c' },
                                     label: { color: '#871b47' } }}"))
plot(sk2)

URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
              'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)

s_link<-energy$links
s_nodes<-energy$nodes

# Plot
sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'TWh', fontSize = 12, nodeWidth = 30)

# Colour links
energy$links$energy_type <- sub(' .*', '',
                                energy$nodes[energy$links$source + 1, 'name'])

sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              LinkGroup = 'energy_type', NodeGroup = NULL)







# Data source: http://goo.gl/vcKo6y
UKvisits <- data.frame(origin=c(
  "France", "Germany", "USA",
  "Irish Republic", "Netherlands",
  "Spain", "Italy", "Poland",
  "Belgium", "Australia", 
  "Other countries", rep("UK", 5)),
  visit=c(
    rep("UK", 11), "Scotland",
    "Wales", "Northern Ireland", 
    "England", "London"),
  weights=c(
    c(12,10,9,8,6,6,5,4,4,3,33)/100*31.8, 
    c(2.2,0.9,0.4,12.8,15.5)))
## Uncomment the next 3 lines to install the developer version of googleVis
# install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
# library(devtools)
# install_github("mages/googleVis")
require(googleVis)
plot(
  gvisSankey(UKvisits, from="origin", 
             to="visit", weight="weight",
             options=list(
               height=250,
               sankey="{link:{color:{fill:'lightblue'}}}"
             ))
)



g <- graph.tree(24, children = 4)
set.seed(123)
E(g)$weight = rpois(23, 4) + 1
edgelist <- get.data.frame(g) 
colnames(edgelist) <- c("source","target","value")
edgelist$source <- LETTERS[edgelist$source]
edgelist$target <- LETTERS[edgelist$target]




# Get this figure: fig <- get_figure("alishobeiri", 1573)
      # Get this figure's data: data <- get_figure("alishobeiri", 1573)$data
      # Add data to this figure: p <- add_trace(p, x=c(4, 5), y=c(4, 5), kwargs=list(filename="plot from API (646)", fileopt="extend"))
      
      # Get figure documentation: https://plot.ly/r/get-requests/
      # Add data documentation: https://plot.ly/r/file-options/
      
      # You can reproduce this figure in R with the following code!
      
      # Learn about API authentication here: https://plot.ly/r/getting-started
      # Find your api_key here: https://plot.ly/settings/api
      trace1 <- list(
        domain = list(
          x = c(0, 1), 
          y = c(0, 1)
        ), 
        link = list(
          color = c("rgba(253, 227, 212, 0.5)", "rgba(242, 116, 32, 1)", "rgba(253, 227, 212, 0.5)", "rgba(219, 233, 246, 0.5)", "rgba(73, 148, 206, 1)", "rgba(219, 233, 246,0.5)", "rgba(250, 188, 19, 1)", "rgba(250, 188, 19, 0.5)", "rgba(250, 188, 19, 0.5)", "rgba(127, 194, 65, 1)", "rgba(127, 194, 65, 0.5)", "rgba(127, 194, 65, 0.5)", "rgba(211, 211, 211, 0.5)", "rgba(211, 211, 211, 0.5)", "rgba(211, 211, 211, 0.5)"), 
          source = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), 
          target = c(5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7), 
          value = c(20, 3, 5, 14, 1, 1, 3, 17, 2, 3, 9, 2, 5, 9, 8)
        ), 
        node = list(
          color = c("#F27420", "#4994CE", "#FABC13", "#7FC241", "#D3D3D3", "#8A5988", "#449E9E", "#D3D3D3", NULL, NULL, NULL, NULL, NULL, NULL, NULL), 
          label = c("Remain+No – 28", "Leave+No – 16", "Remain+Yes – 21", "Leave+Yes – 14", "Didn’t vote in at least one referendum – 21", "46 – No", "39 – Yes", "14 – Don’t know / would not vote"), 
          line = list(
            color = "black", 
            width = 0
          ), 
          pad = 10, 
          thickness = 30
        ), 
        orientation = "h", 
        type = "sankey", 
        valueformat = ".0f"
      )
      data <- list(trace1)
      layout <- list(
        font = list(size = 10), 
        height = 772, 
        title = "Scottish Referendum Voters who now want Independence"
      )
      p <- plot_ly()
      p <- add_trace(p, domain=trace1$domain, link=trace1$link, node=trace1$node, orientation=trace1$orientation, type=trace1$type, valueformat=trace1$valueformat)
      #p <- layout(p, font=layout$font, height=layout$height, title=layout$title)
      p
      
      
  #--------------
      nodes = data.frame("name" = 
                           c("Node A", # Node 0
                             "Node B", # Node 1
                             "Node C", # Node 2
                             "Node D"))# Node 3
      links = as.data.frame(matrix(c(
        0, 1, 10, # Each row represents a link. The first number
        0, 2, 20, # represents the node being conntected from. 
        1, 3, 30, # the second number represents the node connected to.
        2, 3, 40),# The third number is the value of the node
        byrow = TRUE, ncol = 3))
      names(links) = c("source", "target", "value")
      
      
#------------------------------------
      library(networkD3)
      nodes = data.frame("name" = 
                           c("Node A", # Node 0
                             "Node B", # Node 1
                             "Node C", # Node 2
                             "Node D"))# Node 3
      links = as.data.frame(matrix(c(
        0, 1, 10, # Each row represents a link. The first number
        0, 2, 20, # represents the node being conntected from. 
        1, 3, 30, # the second number represents the node connected to.
        2, 3, 40),# The third number is the value of the node
        byrow = TRUE, ncol = 3))
      names(links) = c("source", "target", "value")
      sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize= 12, nodeWidth = 30)
    
    