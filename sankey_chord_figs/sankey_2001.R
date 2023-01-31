library(networkD3)
library(htmlwidgets)

#Get names and codes of BW districts
BW_nc<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/District name evolution by census/BW District name and pop_NEW_JP.xlsx')

#2001
OD_2001_0<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/Migration/Migration Matrices/ipums2001_1yr_migration.xlsx')

names_2001<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`2001`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`2001`)==F]))
colnames(names_2001)<-c("Dist Code", "District Name")

OD_2001_1<-as.matrix(OD_2001_0[,-1])
rownames(OD_2001_1)<-as.vector(names_2001$`Dist Code`)
colnames(OD_2001_1)<-as.vector(names_2001$`Dist Code`)

OD_2001_class<-as.data.frame(OD_2001_1) %>% 
  dplyr::mutate(origin=row.names(.)) %>% 
  tidyr::gather("destination", "numpeople", 1:25) %>% 
  left_join(BW_nc[,c("Dist Code", "Classification 2001")], by=c("origin"="Dist Code"))%>% 
  left_join(BW_nc[,c("Dist Code", "Classification 2001")], by=c("destination"="Dist Code"))

count_2001<-OD_2001_class %>% dplyr::group_by(`Classification 2001.x`, `Classification 2001.y`) %>% 
  dplyr::summarise(numpeople_class=sum(numpeople))

View(count_2001)
## create a dataframe with nodes
count_2001.df<-as.data.frame(count_2001)
#nodes = data.frame("name" = rep(as.character(unique(count_2001$`Classification 2001.x`)),2))
nodes =data.frame("name" = rep(as.character(c("rural", "City", "Towns","urban", "mixed")),2))
nodes[nodes == "rural"] <- "Rural"
nodes[nodes == "mixed"] <- "PartiallyUrban"
nodes[nodes == "urban"] <- "Urban"
nodes[nodes == "City"] <- "Cities"

nodes$name[1:5]<-paste0("inicio_", nodes$name[1:5])

#Prepped data from network diagrams
count_2001.df<-as.data.frame(count_2001)
count_2001.df<-rbind(count_2001.df[11:15,], count_2001.df[1:5,], count_2001.df[16:20,], count_2001.df[21:25,], count_2001.df[6:10,])

colnames(count_2001.df)<-c("origin","dest","numppl")

#codes replaced c("City","mixed", "rural",... )<-c("0","1","2",...)
count_2001.df["origin"][count_2001.df["origin"] == "rural"] <- "0"
count_2001.df["dest"][count_2001.df["dest"] == "rural"] <- "5"

count_2001.df["origin"][count_2001.df["origin"] == "City"] <- "1" #origin node
count_2001.df["dest"][count_2001.df["dest"] == "City"] <- "6" #dest node

count_2001.df["origin"][count_2001.df["origin"] == "Towns"] <- "2"
count_2001.df["dest"][count_2001.df["dest"] == "Towns"] <- "7"

count_2001.df["origin"][count_2001.df["origin"] == "urban"] <- "3"
count_2001.df["dest"][count_2001.df["dest"] == "urban"] <- "8"

count_2001.df["origin"][count_2001.df["origin"] == "mixed"] <- "4"
count_2001.df["dest"][count_2001.df["dest"] == "mixed"] <- "9"

## create edges with weights
links <-transform(count_2001.df, origin = as.numeric(origin), dest = as.numeric(dest), numppl = as.numeric(numppl))

## add edge types for coloring purpose
links$group = as.character(links$origin)

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain(["Rural", "Cities", "Towns", "Urban", "PartiallyUrban", 
"Rural", "Cities", "Towns", "Urban", "PartiallyUrban", "0", "1", "2","3", "4"]) .range(["#009051", "#531B93","#0868AC", "orange", "#900C3F","#009051", "#531B93", "#0868AC", "orange", "#900C3F","#009051", "#531B93", "#0868AC", "orange", "#900C3F"])'

## Draw Sankey Diagram
p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "origin", Target = "dest",
                  Value = "numppl", NodeID = "name",
                  fontSize = 16, nodeWidth = 40, sinksRight = FALSE,
                  colourScale = node_color, fontFamily = "sans-serif",
                  LinkGroup="group", iterations = 0)
p
p<-onRender(
  p,
  '
  function(el,x){
  // select all our node text
  d3.select(el)
  .selectAll(".node text")
  .filter(function(d) { return d.name.startsWith("inicio_"); })
  .attr("x", x.options.nodeWidth - 60)
  .attr("text-anchor", "end");
  }
  '
)
p
