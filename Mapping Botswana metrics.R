# CODE for mapping Botswana's district-level HIV prevalence and migration metrics
# For eLife publication 'The role of migration networks in the development of Botswana's generalized HIV epidemic'
# Author: Justin T. Okano
# Last updated: 15 August 2023

library(readxl)
library(rgdal) 
library(dplyr) 
library(sf) 
library(nngeo)
library(RColorBrewer)
library(extrafont)
library(raster)
library(fields)

# Read in previously calculated HIV prevalence & migration metrics
setwd("/Users/jokano/Desktop/JO_Work/Finished projects/Botswana (eLife)/Data in process")
dist_prev<-read.csv("dist_prev.csv",header=F)[,1]
metrics81<-read.csv("metrics_1981.csv",header=F) ; metrics91<-read.csv("metrics_1991.csv",header=F)
metrics01<-read.csv("metrics_2001.csv",header=F) ; metrics11<-read.csv("metrics_2011.csv",header=F)
net81<-metrics81[,2] ; wd81<-metrics81[,3] ; to81<-metrics81[,4]
net91<-metrics91[,2] ; wd91<-metrics91[,3] ; to91<-metrics91[,4]
net01<-metrics01[,2] ; wd01<-metrics01[,3] ; to01<-metrics01[,4]
net11<-metrics11[,2] ; wd11<-metrics11[,3] ; to11<-metrics11[,4]

# Read in Origin-Destination (OD) matrices (for calculating migratory flows)
setwd('/Users/jokano/Box/Blower Lab/Botswana/Data/IPUMS (1981-1991-2001)/Migration/Migration Matrices/')
OD_1981<-read_excel('ipums1981_1yr_migration.xlsx')
OD_1991<-read_excel('ipums1991_1yr_migration.xlsx')
OD_2001<-read_excel('ipums2001_1yr_migration.xlsx')
OD_2011<-read_excel('../../../2011 Census/Migration/Migration Matrices/district_level_migration_1yr_v2.xlsx')

# Harmonized Botswana districts (shapefile) for 'as consistent as possible' geographic units over time
setwd("/Users/jokano/Desktop/JO_Work/Botswana/botswana mapping/Settlement/")
districts2 <- readOGR(dsn=".",layer="Botswana-Census_Districts")
bw13<-spTransform(districts2, CRS("+proj=longlat +datum=WGS84"))
bw13<-bw13[order(as.numeric(bw13$Dist_Code)),]  # Reorder by district codes
# Editing several district name labels:
bw13$District_N[c(4,12:13,15:17,21,22)]<-c("Selebi Phikwe","Kweneng\n    East","Kweneng\nWest","Central\nSerowe","Central\nMahalapye","Central\nBobonong","Ngamiland East","Ngamiland\nWest")

# Compared with the IPUMS data, the Botswana districts shapefile has excess polygons (for example, the Delta and the game reserve). 
# Here, we combine several of these (as advised by our Botswana colleagues) into the appropriate district divisions to match the IPUMS data.
mdist1<-subset(bw13,Dist_Code %in% c('71','73')) %>%  # Ngamiland North + Okavango Delta
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist2<-subset(bw13,Dist_Code %in% c('80','81')) %>%  # Ghanzi + CKGR
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist3<-subset(bw13,Dist_Code %in% c('02','60')) %>%  # Francistown + North East (for map coloring)
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist4<-subset(bw13,Dist_Code %in% c('30','31')) %>%  # Kweneng East + Kweneng West
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist5<-subset(bw13,Dist_Code %in% c('12','10','06')) %>%  # Ngwaketse West + Southern + Jwaneng (for 1981 and 1991)
 st_as_sf() %>% st_make_valid() %>% st_union()
 mdist5.2<-subset(bw13,Dist_Code %in% c('10','06')) %>%  # Southern + Jwaneng (for 2001 and 2011)
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist6o<-subset(bw13,Dist_Code %in% c('54','7')) %>%  # Sowa + Central Tutume
 st_as_sf() %>% st_make_valid() %>% st_union()
 mdist6<-st_remove_holes(mdist6o)
mdist7<-subset(bw13,Dist_Code %in% c('01','20','03')) %>% # Gaborone + South East + Lobatse
 st_as_sf() %>% st_make_valid() %>% st_union()
mdist8<-subset(bw13,Dist_Code %in% c('04','52')) %>% # Selebi Phikwe + Central Bononong
 st_as_sf() %>% st_make_valid() %>% st_union()

# Adds district-level HIV prevalence & migration metrics to shapefile, while accounting for misaligned or different districts over time
bw13@data$prev<-rep(NA,28) ;  bw13@data$prev[-c(24,26)]<-dist_prev

bw13@data$to1<-rep(NA,28) ; bw13@data$to1[-c(7,10,12,13,26)]<-to81[-c(10,17)]
bw13@data$to2<-rep(NA,28) ; bw13@data$to2[-c(10,24,26)]<-to91
bw13@data$to3<-rep(NA,28) ; bw13@data$to3[-c(24,26)]<-to01
bw13@data$to4<-rep(NA,28) ; bw13@data$to4<-c(to11[1:12],to11[12:27])

bw13@data$wd1<-rep(NA,28) ; bw13@data$wd1[-c(7,10,12,13,26)]<-wd81[-c(10,17)]
bw13@data$wd2<-rep(NA,28) ; bw13@data$wd2[-c(10,24,26)]<-wd91
bw13@data$wd3<-rep(NA,28) ; bw13@data$wd3[-c(24,26)]<-wd01
bw13@data$wd4<-rep(NA,28) ; bw13@data$wd4<-c(wd11[1:12],wd11[12:27])

bw13@data$n1<-rep(NA,28) ; bw13@data$n1[-c(7,10,12,13,26)]<-net81[-c(10,17)]
bw13@data$n2<-rep(NA,28) ; bw13@data$n2[-c(10,24,26)]<-net91
bw13@data$n3<-rep(NA,28) ; bw13@data$n3[-c(24,26)]<-net01
bw13@data$n4<-rep(NA,28) ; bw13@data$n4<-c(net11[1:12],net11[12:27])

# Centroids for circles to be drawn for cities/towns: Gaborone, Francistown, Lobatse, Selebi Phikwe, Orapa, Jwaneng, Sowa 
xp<-c(25.92747,27.50707,25.68761,27.83441,25.38387,24.72383,26.20939) 
yp<-c(-24.62381,-21.16779,-25.20292,-22.00321,-21.31454,-24.60061,-20.57568) 
POINTS<-function(cex0,vec) {
	n<-length(vec) 
	for (i in 1:n) { 
		points(xp[i],yp[i],pch=21,bg=pall[vec[i]],cex=cex0)
		}	
}

# Adjusting polygon centroids to improve positioning of district name labels
preset1<-function(bw) {  #  For plotting large (1x1) maps
	bw$District_N[12]<-"Kweneng\n    East"
	bw@polygons[[1]]@labpt<-c(26.72,-25.03)  # Gaborone
	bw@polygons[[2]]@labpt<-c(28.8,-20.9)  # Francistown
	bw@polygons[[3]]@labpt<-c(26.5,-25.65)  # Lobatse	
	bw@polygons[[4]]@labpt<-c(28.9,-21.27)  # Selebi Phikwe   
	bw@polygons[[5]]@labpt<-c(25.36,-21.68)  # Orapa 
	bw@polygons[[6]]@labpt<-c(24.87,-24.27)  # Jwaneng 
	bw@polygons[[8]]@labpt<-c(25.04,-25.08) # Southern		     
	bw@polygons[[9]]@labpt<-c(24.98,-25.54) # Barolong 
	bw@polygons[[11]]@labpt<-c(26.72,-25.35)  # South East
	bw@polygons[[12]]@labpt<-c(25.49,-23.83)  # Kweneng East 
	bw@polygons[[13]]@labpt<-c(24.1,-23.83)  # Kweneng West
	bw@polygons[[14]]@labpt<-c(26.54,-24.15)  # Kgatleng  
	bw@polygons[[15]]@labpt<-c(26.5,-22.22)  # Central Serowe
	bw@polygons[[16]]@labpt<-c(26.62,-23.21)  # Central Mahalapye
	bw@polygons[[17]]@labpt<-c(28.45,-22.05)  # Central Bobonong
	bw@polygons[[18]]@labpt<-c(25.01,-20.99)  # Central Boteti
	bw@polygons[[19]]@labpt<-c(26.21,-20.25)  # Central Tutume	 
	bw@polygons[[20]]@labpt<-c(28.6,-20.5)  # Northeast		
	bw@polygons[[21]]@labpt<-c(22.48,-20.4)   # Ngamiland East 
	bw@polygons[[22]]@labpt<-c(22.2,-18.84)  # Ngamiland West 
	bw@polygons[[25]]@labpt<-c(22.65,-22.276)  # Ghanzi
	bw@polygons[[27]]@labpt<-c(21.78,-25.4)   # Kgalagadi South
	return(bw)
}
bw13<-preset1(bw13)

preset2<-function(bw) {  #  For plotting smaller (2x2) maps
	bw$District_N[12]<-"Kweneng\n East"
	bw@polygons[[1]]@labpt<-c(26.8,-24.95)  # Gaborone 
	bw@polygons[[4]]@labpt<-c(28.95,-21.27)  # Selebi Phikwe
	bw@polygons[[8]]@labpt<-c(24.87,-24.98)  # Southern	
	bw@polygons[[11]]@labpt<-c(26.82,-25.3)  # South East	
	bw@polygons[[12]]@labpt<-c(25.42,-23.83)  # Kweneng East
	bw@polygons[[14]]@labpt<-c(26.6,-24.15)  # Kgatleng
	return(bw)
}

preset3<-function(bw) {  #  Minor adjustment to preset2 for 2001 & 2011
	bw@polygons[[8]]@labpt<-c(24.94,-25.13)  # Southern	
	return(bw)
}

preset4<-function(bw) {  #  For positioning 'centroid' points (rather than district text labels)
	for (i in 1:7) { 
		bw@polygons[[i]]@labpt<-c(xp[i],yp[i])  # Cities and towns
	}
	bw@polygons[[8]]@labpt<-c(24.94,-25.03)  # Southern	
	bw@polygons[[9]]@labpt<-c(25.08,-25.54)  # Barolong 
	bw@polygons[[11]]@labpt<-c(25.76,-24.93)  # South East
	bw@polygons[[14]]@labpt<-c(26.36,-24.15)  # Kgatleng
	bw@polygons[[18]]@labpt<-c(25.11,-21.15)  # Central Boteti
	bw@polygons[[20]]@labpt<-c(27.60,-20.87)  # Northeast	
	bw@polygons[[21]]@labpt<-c(22.97,-20.3)   # Ngamiland East 
	bw@polygons[[25]]@labpt<-c(21.77,-22.276)  # Ghanzi
	bw@polygons[[27]]@labpt<-c(21.99,-25.6)   # Kgalagadi South
	return(bw)
}

# Function to add in line segments for district labels (when no room to label at district center)
LINES<-function(x) {
	lines(c(27.87,27.55),c(-20.5,-20.77))  # North East
	lines(c(27.96,27.55),c(-20.9,-21.15))  # Francistown
	lines(c(28.08,27.83),c(-21.3,-21.95))  # Selebi Phikwe
	lines(c(25.3,25.38),c(-21.53,-21.35))  # Orapa
	lines(c(24.86,24.71),c(-24.40,-24.55))  # Jwaneng
	lines(c(26.1,25.94),c(-24.92,-24.65))  # Gaborone
	lines(c(25.94,25.73),c(-25.57,-25.21))  # Lobatse
	lines(c(26.02,25.74),c(-25.27,-24.92))  # South East 
	if (x==2) { lines(c(26.25,26.47),c(-20.57,-20.69)) }  # Sowa, if year: 1991, 2001, 2011
}  

# Function to plot data for merged districts (mdistX) defined earlier 
# Variable to plot, var: 1 (prev), 2:5 (turnover 1981:2011), 6:9 (wdmi 1981:2011), 10:13 (netflow 1981:2011)
plotmd<-function(mdist,var,code) {	
	plot(mdist,col=pall[cut(bw13@data[,var+5][bw13$Dist_Code==code],breaks=BRK)],add=T) 
}

# A few additional functions are needed to plot migratory flows:

# Function takes in a shapefile & returns a table of (x,y) centers for each district/polygon
centers<-function(bw) {
BW<-bw
c01x<-c01y<-c01<-NULL 
for (i in 1:dim(BW)[1]) {
	c01x<-c(c01x,BW@polygons[[i]]@labpt[1]) ; c01y<-c(c01y,BW@polygons[[i]]@labpt[2]) ; c01<-cbind(c01,BW@polygons[[i]]@labpt) }
out<-cbind(c01x,c01y)
return(out)
}

# Computes Origin-Destination (OD) matrix of movements between districts
# This is done by inputting the four OD matrices & removing within-district migrations (by setting diagonal to 0)
 OD1<-as.data.frame(OD_1981[-c(18,23),-c(1,18,23)])
 diag(OD1)<-0
  OD2<-as.data.frame(OD_1991[,-1])
  diag(OD2)<-0
  OD3<-as.data.frame(OD_2001[,-1])
  diag(OD3)<-0
    OD4<-as.data.frame(OD_2011[-24,-c(1,25)])
    OD4[,12]<-OD4[,12]+OD4[,13]
     OD4<-OD4[,-13] 
  diag(OD4)<-0

# Function to draw line segments between districts with thickness indicating magnitude of net-flow and color indicating flow direction
connect<-function(od,bw) {
	ODX<-od ; BW<-bw
	c01x<-centers(BW)[,1] ; c01y<-centers(BW)[,2]
 	OD1net<-matrix(0,dim(ODX)[1],dim(ODX)[2])
 	
 	for (i in 1:dim(ODX)[1]) {
 		for (j in i:dim(ODX)[2]) {
 			OD1net[i,j]<-ODX[i,j]-ODX[j,i]  # Creates an upper triangular matrix of net-flow between district pairs
 		}
 	}
 	for (i in 1:dim(ODX)[1]) {
 		for (j in i:dim(ODX)[2]) {
 			COL<-"#0CB702"  # Sets default line segment color to green
 			if (j!=i) {
 				# Changes line segment color to pink if movement is eastward. This happens when:
 				# 1) Net-flow is negative and longitude/x decreases from district i to j, or 
 				# 2) Net-flow is positive and longitude/x increases from district i to j
 				if ((OD1net[i,j]<0 & c01x[i]>c01x[j]) | (OD1net[i,j]>0 & c01x[i]<c01x[j]) ) { COL<-"#ff61cc" }
 				lines(c(c01x[i],c01x[j]),c(c01y[i],c01y[j]),lwd=abs(OD1net[i,j]/150),col=COL)  # Thickness determined by net-flow
 			}
 		}
 	}
}

# HIV PREVALENCE MAP 
# Detailed commenting provided for this map - all other maps are created in a similar manner
pall<-c(rev(c(brewer.pal(n = 9, name = "Spectral")))[-1],"#A50F15")  # Color spectrum to use
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
par(family=fonts()[9]) ; bw13<-preset1(bw13)
BRK<-seq(.125,.35,.025)  # Prevalence breaks for color classes
plot(bw13['prev'],col=pall[cut(bw13$prev,breaks=BRK)])  # Plot prevalence for districts that don't require merging
plotmd(mdist1,1,'71') ; plotmd(mdist2,1,'80') ; plotmd(mdist3,1,'60')  # Overlay polygons for merged districts (colored by prevalence)
plotmd(mdist5.2,1,'10') ; plotmd(mdist6,1,'54') ; plotmd(mdist7,1,'20') ; plotmd(mdist8,1,'52') 
# District labels for non-standard districts (ones that don't exist at all time points, or require special positioning):
text(23.79,-24.61,"NGWAKETSE\nWEST",cex=.9) ;  text(26.7,-20.87,"SOWA",cex=.9,col="white") 
# District labels for all other districts 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.9,col=c(rep(1,12),"white",1,1,1,"white",1,1,1,"white",1,1)) 
par(family=fonts()[40]) ; LINES(2)  # Adds line segments for district labels
POINTS(cex0=1.7,vec=match(cut(bw13$prev[1:7],breaks=BRK),levels(cut(bw13$prev,breaks=BRK))))  # Plots prevalence-colored circles for cities/towns

# TURNOVER MAP
pall<-c(brewer.pal(n = 9, name = "RdYlBu")) 
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0))  # 2x2 grid to show all 4 years
par(family=fonts()[9]) ; bw13<-preset2(bw13)
BRK<-c(-5,seq(-1.5,1.5,.5),5,46)
plot(bw13['to1'],col=pall[cut(bw13$to1,breaks=BRK)])
plotmd(mdist1,2,'71') ; plotmd(mdist2,2,'80') ; plotmd(mdist3,2,'60') ; plot(st_remove_holes(mdist4),col=pall[4],add=T) ;
plotmd(mdist5,2,'10') ; plotmd(mdist6,2,'54') ; plotmd(mdist7,2,'20') ; plotmd(mdist8,2,'52') 
text(24.55,-23.85,"KWENENG",cex=.6) 
text(bw13[-c(7,10,12:13,24,26),],labels=toupper(bw13$District_N[-c(7,10,12:13,24,26)]),font=1,cex=.6,col=c(rep(1,7),"white",rep(1,4),"white",rep(1,4),"white",1,1,1,1)) 
par(family=fonts()[40]) ; LINES(1) 
POINTS(1,match(cut(bw13$to1[1:6],breaks=BRK),levels(cut(bw13$to1,breaks=BRK)))) 
text(20,-18,"A",font=2,cex=1) ; text(27.3,-19,"1981",font=2)  
par(family=fonts()[9])
plot(bw13['to2'],col=pall[cut(bw13$to2,breaks=BRK)])
plotmd(mdist1,3,'71') ; plotmd(mdist2,3,'80') ; plotmd(mdist3,3,'60') 
plotmd(mdist5,3,'10') ; plotmd(mdist6,3,'54') ; plotmd(mdist7,3,'20') ; plotmd(mdist8,3,'52') 
text(26.7,-20.87,"SOWA",cex=.6,col="white")
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=c(rep(1,7),"white",rep(1,5),"white","white",1,"white",1,"white","white",1,1,1,1)) 
par(family=fonts()[40]) ; LINES(2) 
POINTS(1,match(cut(bw13$to2[1:7],breaks=BRK),levels(cut(bw13$to2,breaks=BRK)))) 
text(20,-18,"B",font=2,cex=1) ; text(27.3,-19,"1991",font=2)  
par(family=fonts()[9]) ; bw13<-preset3(bw13)
plot(bw13['to3'],col=pall[cut(bw13$to3,breaks=BRK)])
plotmd(mdist1,4,'71') ; plotmd(mdist2,4,'80') ; plotmd(mdist3,4,'60')
plotmd(mdist5.2,4,'10') ; plotmd(mdist6,4,'54') ; plotmd(mdist7,4,'20') ; plotmd(mdist8,4,'52') 
text(23.78,-24.61,"NGWAKETSE\nWEST",cex=.6) ;  text(26.7,-20.87,"SOWA",cex=.6) 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=rep(1,24)) 
par(family=fonts()[40]) ; LINES(2)
POINTS(1,match(cut(bw13$to3[1:7],breaks=BRK),levels(cut(bw13$to3,breaks=BRK)))) 
text(20,-18,"C",font=2,cex=1) ; text(27.3,-19,"2001",font=2)  
par(family=fonts()[9])
plot(bw13['to4'],col=pall[cut(bw13$to4,breaks=BRK)])
plotmd(mdist1,5,'71') ; plotmd(mdist2,5,'80') ; plotmd(mdist3,5,'60')
plotmd(mdist5.2,5,'10') ; plotmd(mdist6,5,'54') ; plotmd(mdist7,5,'20') ; plotmd(mdist8,5,'52') 
text(23.78,-24.61,"NGWAKETSE\nWEST",cex=.6) ; text(26.7,-20.87,"SOWA",cex=.6) 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=c(rep(1,18),"white",rep(1,5)))
par(family=fonts()[40]) ; LINES(2)
POINTS(1,match(cut(bw13$to4[1:7],breaks=BRK),levels(cut(bw13$to4,breaks=BRK)))) 
text(20,-18,"D",font=2,cex=1) ; text(27.3,-19,"2011",font=2)  

# WDMI MAP
pall<-rev(c(brewer.pal(n = 9, name = "PuOr")))
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0))
par(family=fonts()[9]) ; bw13<-preset2(bw13)
BRK<-c(seq(0,8,1),10)
plot(bw13['wd1'],col=pall[cut(bw13$wd1,breaks=BRK,include.lowest=T)])
plotmd(mdist1,6,'71') ; plotmd(mdist2,6,'80') ; plotmd(mdist3,6,'60') ; plot(st_remove_holes(mdist4),col=pall[6],add=T) ;
plotmd(mdist5,6,'10') ; plotmd(mdist6,6,'54') ; plotmd(mdist7,6,'20') ; plotmd(mdist8,6,'52') 
text(24.55,-23.85,"KWENENG",cex=.6) 
text(bw13[-c(7,10,12:13,24,26),],labels=toupper(bw13$District_N[-c(7,10,12:13,24,26)]),font=1,cex=.6,col=c(rep(1,7),"white",rep(1,9),"white",1,"white",1,"white"))
par(family=fonts()[40]) ; LINES(1) 
POINTS(1,match(cut(bw13$wd1[1:6],breaks=BRK,include.lowest=T),levels(cut(bw13$wd1,breaks=BRK,include.lowest=T)))) 
text(20,-18,"A",font=2,cex=1) ; text(27.3,-19,"1981",font=2) 
par(family=fonts()[9])
plot(bw13['wd2'],col=pall[cut(bw13$wd2,breaks=BRK)])
plotmd(mdist1,7,'71') ; plotmd(mdist2,7,'80') ; plotmd(mdist3,7,'60') 
plotmd(mdist5,7,'10') ; plotmd(mdist6,7,'54') ; plotmd(mdist7,7,'20') ; plotmd(mdist8,7,'52') 
text(26.7,-20.87,"SOWA",cex=.6) 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=c(rep(1,19),"white",1,1,1,1))
par(family=fonts()[40]) ; LINES(2)
POINTS(1,match(cut(bw13$wd2[1:7],breaks=BRK,include.lowest=T),levels(cut(bw13$wd2,breaks=BRK,include.lowest=T)))) 
text(20,-18,"B",font=2,cex=1) ; text(27.3,-19,"1991",font=2) 
par(family=fonts()[9]) ; bw13<-preset3(bw13)
plot(bw13['wd3'],col=pall[cut(bw13$wd3,breaks=BRK)])
plotmd(mdist1,8,'71') ; plotmd(mdist2,8,'80') ; plotmd(mdist3,8,'60')
plotmd(mdist5.2,8,'10') ; plotmd(mdist6,8,'54') ; plotmd(mdist7,8,'20') ; plotmd(mdist8,8,'52') 
text(23.78,-24.61,"NGWAKETSE\nWEST",cex=.6) ;  text(26.7,-20.87,"SOWA",cex=.6) 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=c(rep(1,4),"white",rep(1,10),"white",rep(1,5),"white",1,1))
par(family=fonts()[40]) ; LINES(2)
POINTS(1,match(cut(bw13$wd3[1:7],breaks=BRK,include.lowest=T),levels(cut(bw13$wd3,breaks=BRK,include.lowest=T)))) 
text(20,-18,"C",font=2,cex=1) ; text(27.3,-19,"2001",font=2)  
par(family=fonts()[9])
plot(bw13['wd4'],col=pall[cut(bw13$wd4,breaks=BRK)])
plotmd(mdist1,9,'71') ; plotmd(mdist2,9,'80') ; plotmd(mdist3,9,'60')
plotmd(mdist5.2,9,'10') ; plotmd(mdist6,9,'54') ; plotmd(mdist7,9,'20') ; plotmd(mdist8,9,'52') 
text(23.78,-24.61,"NGWAKETSE\nWEST",cex=.6) ; text(26.7,-20.87,"SOWA",cex=.6) 
text(bw13[-c(7,10,24,26),],labels=toupper(bw13$District_N[-c(7,10,24,26)]),font=1,cex=.6,col=c(rep(1,21),"white",1,1))
par(family=fonts()[40]) ; LINES(2)
POINTS(1,match(cut(bw13$wd4[1:7],breaks=BRK,include.lowest=T),levels(cut(bw13$wd4,breaks=BRK,include.lowest=T)))) 
text(20,-18,"D",font=2,cex=1) ; text(27.3,-19,"2011",font=2) 

# NETFLOW MAP
# Each district is color-coded to show net-flow, with pink/green connecting lines used to show pairwise flows between districts
# Districts to calculate pairwise flows for, by census year:
bw13<-preset4(bw13)
bw1<-subset(bw13,!Dist_Code %in% c('07','12','30','73','81'))  #1981
bw2<-subset(bw13,!Dist_Code %in% c('12','73','81'))  # 1991
bw3<-subset(bw13,!Dist_Code %in% c('73','81'))  # 2001
bw4<-subset(bw13,!Dist_Code %in% c('73','81'))  #2011

pall<-c(brewer.pal(n = 8, name = "Greys"))
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(0,0,0,0))
par(family=fonts()[9])
BRK<-c(-7500,-2000,-500,-250,0,250,500,2000,6000)
plot(bw13['n1'],col=pall[cut(bw13$n1,breaks=BRK)])
plotmd(mdist1,10,'71') ; plotmd(mdist2,10,'80') ; plotmd(mdist3,10,'60') ; plot(st_remove_holes(mdist4),col=pall[3],add=T) ;
plotmd(mdist5,10,'10') ; plotmd(mdist6,10,'54') ; plotmd(mdist7,10,'20') ; plotmd(mdist8,10,'52') 
par(family=fonts()[40]) ; text(20,-18,"A",font=2,cex=1) ; text(27.3,-19,"1981",font=2) ; par(family=fonts()[9])
connect(OD1,bw1)
points(x=centers(bw1)[,1],y=centers(bw1)[,2],pch=20,col=1,cex=.3)
POINTS(1.7,match(cut(bw13$n1[1:6],breaks=BRK),levels(cut(bw13$n1,breaks=BRK))))
plot(bw13['n2'],col=pall[cut(bw13$n2,breaks=BRK)])
plotmd(mdist1,11,'71') ; plotmd(mdist2,11,'80') ; plotmd(mdist3,11,'60') 
plotmd(mdist5,11,'10') ; plotmd(mdist6,11,'54') ; plotmd(mdist7,11,'20') ; plotmd(mdist8,11,'52') 
par(family=fonts()[40]) ; text(20,-18,"B",font=2,cex=1) ; text(27.3,-19,"1991",font=2) ; par(family=fonts()[9])
connect(OD2,bw2)
points(x=centers(bw2)[,1],y=centers(bw2)[,2],pch=20,col=1,cex=.3)
POINTS(1.7,match(cut(bw13$n2[1:7],breaks=BRK),levels(cut(bw13$n2,breaks=BRK))))
plot(bw13['n3'],col=pall[cut(bw13$n3,breaks=BRK)])
plotmd(mdist1,12,'71') ; plotmd(mdist2,12,'80') ; plotmd(mdist3,12,'60')
plotmd(mdist5.2,12,'10') ; plotmd(mdist6,12,'54') ; plotmd(mdist7,12,'20') ; plotmd(mdist8,12,'52') 
par(family=fonts()[40]) ; text(20,-18,"C",font=2,cex=1) ; text(27.3,-19,"2001",font=2) ; par(family=fonts()[9])
connect(OD3,bw3)
points(x=centers(bw3)[,1],y=centers(bw3)[,2],pch=20,col=1,cex=.3)
POINTS(1.7,match(cut(bw13$n3[1:7],breaks=BRK),levels(cut(bw13$n3,breaks=BRK))))
plot(bw13['n4'],col=pall[cut(bw13$n4,breaks=BRK)])
plotmd(mdist1,13,'71') ; plotmd(mdist2,13,'80') ; plotmd(mdist3,13,'60')
plotmd(mdist5.2,13,'10') ; plotmd(mdist6,13,'54') ; plotmd(mdist7,13,'20') ; plotmd(mdist8,13,'52') 
par(family=fonts()[40]) ; text(20,-18,"D",font=2,cex=1) ; text(27.3,-19,"2011",font=2) ; par(family=fonts()[9])
connect(OD4,bw4)
points(x=centers(bw4)[,1],y=centers(bw4)[,2],pch=20,col=1,cex=.3)
POINTS(1.7,match(cut(bw13$n4[1:7],breaks=BRK),levels(cut(bw13$n4,breaks=BRK))))

# Constructing a legend (for example, for Turnover)
set.seed(seed = 1)
rot_mat<-matrix(sample(1:9,25,replace=T),5,5)
image.plot(rot_mat,legend.only=T,col = brewer.pal(n = 9, name = "RdYlBu"),axis.args=list(at=seq(0.5,9.5,1),labels=sprintf("%.1f",c(-5.0,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,5.0,45.0))),legend.args=list(text="          Turnover",side=3,font=2,line=.8))