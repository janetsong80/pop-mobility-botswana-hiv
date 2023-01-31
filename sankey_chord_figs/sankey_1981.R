library(networkD3)
library(htmlwidgets)

#Get names and codes of BW districts
BW_nc<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/District name evolution by census/BW District name and pop_NEW_JP.xlsx')
#1981
OD_1981_0<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/Migration/Migration Matrices/ipums1981_1yr_migration.xlsx')
#
BW_nc0<-as.data.frame(BW_nc)

names_1981<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`1981`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`1981`)==F]))
colnames(names_1981)<-c("Dist Code", "District Name")

OD_1981_1<-as.matrix(OD_1981_0[,-1])
rownames(OD_1981_1)<-as.vector(names_1981$`Dist Code`)
colnames(OD_1981_1)<-as.vector(names_1981$`Dist Code`)

class(OD_1981_0)
BW_nc0$`Classification UR 1981`

OD_1981_class<-as.data.frame(OD_1981_1) %>% 
  dplyr::mutate(origin=row.names(.)) %>% 
  tidyr::gather("destination", "numpeople", 1:25) %>% 
  left_join(BW_nc[,c("Dist Code", "Classification 1981")], by=c("origin"="Dist Code"))%>% 
  left_join(BW_nc[,c("Dist Code", "Classification 1981")], by=c("destination"="Dist Code"))

count_1981<-OD_1981_class %>% dplyr::group_by(`Classification 1981.x`, `Classification 1981.y`) %>% 
  dplyr::summarise(numpeople_class=sum(numpeople))

#count_1981$`Classification 1981.x` <- paste0(count_1981$`Classification 1981.x`,"_1981")
count_1981[order(count_1981$numpeople_class),]


#Do not run the parts where it adds _1981 to the df cause it wont work then.

## create a dataframe with nodes
nodes = data.frame("name" = rep(as.character(unique(count_1981$`Classification 1981.x`)),2))
nodes[nodes == "rural"] <- "Rural"
nodes[nodes == "City"] <- "Cities"


nodes$name[1:3]<-paste0("inicio_", nodes$name[1:3])

#Prepped data from network diagrams
count_1981.df<-as.data.frame(count_1981)
colnames(count_1981.df)<-c("origin","dest","numppl")

#codes replaced c("City","rural", "towns" )<-c("1","2","3")
count_1981.df["origin"][count_1981.df["origin"] == "City"] <- "0" #origin node
count_1981.df["dest"][count_1981.df["dest"] == "City"] <- "3" #dest node

count_1981.df["origin"][count_1981.df["origin"] == "rural"] <- "1"
count_1981.df["dest"][count_1981.df["dest"] == "rural"] <- "4"

count_1981.df["origin"][count_1981.df["origin"] == "Towns"] <- "2"
count_1981.df["dest"][count_1981.df["dest"] == "Towns"] <- "5"

## create edges with weights
links <-transform(count_1981.df, origin = as.numeric(origin), dest = as.numeric(dest), numppl = as.numeric(numppl))

## add edge types for coloring purpose
links$group = c("type_1", 
                "type_1",
                "type_1", 
                "type_2",
                "type_2",
                "type_2",
                "type_3",
                "type_3",
                "type_3")

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain(["Cities", "Rural", "Towns", "Cities", "Rural", 
"Towns", "type_1", "type_2", "type_3"]) .range(["#531B93", "#009051", "#0868AC", "#531B93", "#009051", "#0868AC", "#531B93", "#009051", "#0868AC"])'

## Draw Sankey Diagram
p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "origin", Target = "dest",
                  Value = "numppl", NodeID = "name",
                  fontSize = 16, nodeWidth = 40, sinksRight = FALSE,
                  colourScale = node_color, fontFamily = "sans-serif",
                  LinkGroup="group")
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

