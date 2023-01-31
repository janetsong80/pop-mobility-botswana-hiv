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
source("/Users/joanponce/Desktop/FCA/FILES FOR JOAN/Libraries.R")

#Get names and codes of BW districts
BW_nc<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/District name evolution by census/BW District name evolution NEW_JP.xls')
#2001
OD_2001_0<-read_excel('/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/Migration/Migration Matrices/ipums2001_1yr_migration.xlsx')

BW_nc0<-as.data.frame(BW_nc)
#2001
names_2001<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`2001`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`2001`)==F]))
colnames(names_2001)<-c("Dist Code", "District Name")

OD_2001_1<-as.matrix(OD_2001_0[,-1])
rownames(OD_2001_1)<-as.vector(names_2001$`District Name`)
colnames(OD_2001_1)<-as.vector(names_2001$`District Name`)

#remove the internal movements within the district
diag(OD_2001_1) <- 0
OD_2001_1[OD_2001_1 <= 200]<-0

#Make into dataframe
OD_2001.df<-as.data.frame(OD_2001_1)

# I need a long format
OD_2001_long <- OD_2001.df %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = -40, gap.degree = 4, track.margin = c(-0.05, 0.05), points.overflow.warning = FALSE)
par(mar = rep(1, 4))


#color scheme NEW
colors_2001.df<-data.frame(as.vector(BW_nc0$`Dist Code`[is.na(BW_nc0$`2001`)==F]), as.vector(BW_nc0$`District Name`[is.na(BW_nc0$`2001`)==F]), as.vector(BW_nc0$`colors`[is.na(BW_nc0$`2001`)==F]))
colnames(colors_2001.df)<-c("Dist Code", "District Name", "Dist colors")
jp_colors_2001<-as.character(colors_2001.df$`Dist colors`)
#pie(rep(1,nrow(colors_2001.df)), col=jp_colors_2001)



#pdf("/Users/joanponce/Dropbox/Data (1)/Botswana/Data/JANET/IPUMS (1981-1991-2001)/plots_JP/2001_1y_MF_over200.pdf", 17,14)
# Base plot
chordDiagram(
  x = OD_2001_long, 
  grid.col = jp_colors_2001,
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
        sub = "2001 1 year migration", cex.sub = 0.5,
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
    dd2 <- ifelse((theta < 225 & theta > 135)||(theta < 45 || theta > 300), 4.5, 3)
    
    circos.text(
      x = mean(xlim), 
      y = dd2, 
      labels = sector.index, 
      facing = dd1, 
      niceFacing = TRUE,
      cex = 1.3
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      # major.at = round(seq(from = 0, to = xlim[2], length.out=2),0), 
      major.at = c(0,max(xlim[2])), 
      labels.cex = 0.5,
      minor.ticks = 0, 
      major.tick.length = 0.1,
      labels.facing = dd,
      labels.niceFacing = TRUE)
  }
)
dev.off() 
