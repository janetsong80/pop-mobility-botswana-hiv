# OD full: This script  constructs the OD matrix from the csv mobility file
library(readxl)
library(tidyr)
library(readr)
library(openxlsx)
library(data.table)
library(tibble)
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)
library(RColorBrewer)
source("/Users/joanponce/Desktop/FCA/FILES FOR JOAN/Libraries.R")

#Get names and codes of BW districts
BW_nc<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/District name evolution by census/BW District name evolution NEW_JP.xls')
#1981
OD_1981_0<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/Migration/Migration Matrices/ipums1981_1yr_migration.xlsx')

BW_nc0<-as.data.frame(BW_nc)[1:29,]

numchars<-function(txt){
  #basically your code, but to be applied to 1 item
  tmpres<-gregexpr('[a-z]', as.character(txt))[[1]]
  ifelse(tmpres[1]==-1, 0, length(tmpres))
}
#now apply it to all items:
BW_nc0$count <-sapply(BW_nc0$`District Name`, numchars)


#1981
names_1981<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`1981`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`1981`)==F]))
colnames(names_1981)<-c("Dist Code", "District Name")

OD_1981_1<-as.matrix(OD_1981_0[,-1])
rownames(OD_1981_1)<-as.vector(names_1981$`District Name`)
colnames(OD_1981_1)<-as.vector(names_1981$`District Name`)

#remove the internal movements within the district
diag(OD_1981_1) <- 0
OD_1981_1[OD_1981_1 <= 200]<-0

#Make into dataframe
OD_1981.df<-as.data.frame(OD_1981_1)

# I need a long format
OD_1981_long <- OD_1981.df %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = -40, gap.degree = 4, track.margin = c(-0.05, 0.05), points.overflow.warning = FALSE)
par(mar = rep(1, 4))

# color palette

colors_1981.df<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`1981`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`1981`)==F]), as.vector(BW_nc0$`colors`[is.na(BW_nc0$`1981`)==F]))
colnames(colors_1981.df)<-c("Dist Code", "District Name", "Dist colors")
jp_colors_1981<-as.character(colors_1981.df$`Dist colors`)

#pdf("/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/plots_JP/1981_1y_MF_over200.pdf", 17,14)

# Base plot
chordDiagram(
  x = OD_1981_long, 
  grid.col = jp_colors_1981,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)+
  title(main = "",
        cex.main = 1, line = -1,
        sub = "1981 1 year migration M&F", cex.sub = 0.5,
        xlab = NULL, ylab = NULL)  

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
    
    theta0 = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd1 <- ifelse((theta < 225 & theta > 135)||(theta < 45 || theta > 300), "clockwise", "inside")
    
    theta1 = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd2 <- ifelse((theta < 225 & theta > 135)||(theta < 45 || theta > 300), 4.2, 3)
    
    circos.text(
      x = mean(xlim), 
      y = dd2, 
      labels = sector.index, 
      facing = dd1, 
      niceFacing = TRUE,
      cex = 1.5
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      # major.at = round(seq(from = 0, to = xlim[2], length.out=2),0), 
      major.at = c(0,max(xlim[2])), 
      labels.cex = 0.7,
      minor.ticks = 0, 
      major.tick.length = 0.1,
      labels.facing = dd,
      labels.niceFacing = FALSE)
  }
)

dev.off() 

