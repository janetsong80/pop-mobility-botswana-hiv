# CODE for estimating HIV prevalence, computing all migration metrics, and plotting age pyramids
# For eLife publication 'The role of migration networks in the development of Botswana's generalized HIV epidemic'
# Author: Justin T. Okano
# Last updated: 09 August 2023

library(foreign)
library(plyr)
library(ggplot2)
library(cowplot)
library(scales)
library(readxl)

# HIV PREVALENCE
# Estimating HIV prevalence from BAIS III (2008) in 15-69 year olds: national, by gender, by district
setwd('/Users/jokano/Box/Blower Lab/Botswana/Data/BAIS III')
bais3<-read.spss("BAIS_III_merged_HP_IND_08Dec09.sav",to.data.frame=TRUE)
bais3a<-subset(bais3,as.numeric(bais3$P04_AGE_YEARS) %in% 16:70 & bais3$HDBS_RSLU %in% c("Positive","Negative")) # For ages 15-69 w/ HIV test

bais3a$HIVp<-as.numeric(as.character(factor(bais3a$HDBS_RSLU,levels=c("Positive","Negative"),labels=c("1","0"))))
bais3a$WT<-bais3a$PEA_WGHT  # Survey individual weights
weighted.mean(bais3a$HIVp,bais3a$WT)  # HIV prevalence, 15-69
weighted.mean(bais3a$HIVp[bais3a$P03_SEX=="Female"],bais3a$WT[bais3a$P03_SEX=="Female"])  # HIV prevalence, Females 15-69
weighted.mean(bais3a$HIVp[bais3a$P03_SEX=="Male"],bais3a$WT[bais3a$P03_SEX=="Male"])  # HIV prevalence, Males 15-69

dist_prev<-unlist(lapply(1:26,prevcalc<-function(i) {  # District prevalence
	weighted.mean(bais3a$HIVp[bais3a$ID_DISTRICT==levels(bais3a$ID_DISTRICT)[i]],bais3a$WT[bais3a$ID_DISTRICT==levels(bais3a$ID_DISTRICT)[i]]) }))
# write.table(dist_prev,'dist_prev.csv',row.names=F,col.names=F,sep=",")

# MIGRATION METRICS
# IMPORTS 4 files for each year:
# (i) District-level Migration matrices - 'OD_Year_0'
# (ii) Summary stat tables - 'dis_Year'
# (iii) Population age structure (5-yr groups) by gender - 'agestr_Year'
# (iv) Population totals by age (5-yr groups) and gender - 'agetot_Year'
setwd('/Users/jokano/Box/Blower Lab/Botswana/Data/IPUMS (1981-1991-2001)/Migration/Migration Matrices/')
# 1981:
OD_1981<-read_excel('ipums1981_1yr_migration.xlsx')
dis_1981<-read.csv('../district_size_growth_churn_1yr_1981.csv')[-26,]
agestr_1981<-read_excel('../age_structure_IPUMS_1981.xlsx',sheet='migrant_1yr_grouped_1981')[-19,]
agetot_1981<-read_excel('../age_structure_IPUMS_1981.xlsx',sheet='ages_population_grouped_1981')
# 1991:
OD_1991<-read_excel('ipums1991_1yr_migration.xlsx')
dis_1991<-read.csv('../district_size_growth_churn_1yr_1991.csv')
agestr_1991<-read_excel('../age_structures_IPUMS_1991.xlsx',sheet='migrant_1yr_grouped_1991')
agetot_1991<-read_excel('../age_structures_IPUMS_1991.xlsx',sheet='ages_total_grouped_1991')
# 2001:
OD_2001<-read_excel('ipums2001_1yr_migration.xlsx')
dis_2001<-read.csv('../district_size_growth_churn_1yr_2001.csv')
agestr_2001<-read_excel('../age_structure_IPUMS_2001.xlsx',sheet='migrant_1yr_grouped_2001')[-19,]
agetot_2001<-read_excel('../age_structure_IPUMS_2001.xlsx',sheet='ages_total_grouped_2001')
# 2011:
OD_2011<-read_excel('../../../2011 Census/Migration/Migration Matrices/district_level_migration_1yr_v2.xlsx')
dis_2011<-read.csv('../../../2011 Census/Migration/district_size_growth_churn_1yr_2011.csv')
agestr_2011<-read_excel('../../../2011 Census/Migration/age_structures_2011_census.xlsx',sheet='migrant_1yr_grouped')
agetot_2011<-read_excel('../../../2011 Census/Migration/age_structures_2011_census.xlsx',sheet='ages_total_population_grouped')

Pop<-c(sum(agetot_1981$total),sum(agetot_1991$total),sum(agetot_2001$total),sum(agetot_2011$total)) # Population total by census

# Function to calculate migration metrics for census year of interest
mig_metrics<-function(year) { 
	
	OD<-as.matrix(eval(parse(text=paste("OD_",year,"[,-1]",sep=""))))
	dis<-eval(parse(text=paste("dis_",year,sep="")))

	if (year==2011) {  # Only run this for 2011. Combines Kweneng & Kweneng East into 1 district (as 1-yr-movement data doesn't differentiate)
		OD[,12]<-OD[,12]+OD[,13]
		OD<-OD[,-13] 
		dis[12,3:6]<-dis[12,3:6]+dis[13,3:6]
		dis<-dis[-13,]
	} 
	
	P<-Pop[(year-1981)/10+1]  # Population
	Di<-apply(OD,2,sum)  # in-migrants for district i
	Oi<-apply(OD,1,sum)  # out-migrants for district i
	M<-sum(Di)
	# Country-level metrics:
	CMI<-100*M/P  # Metric: Crude Migration Intensity (CMI) 
	MEI<-100*.5*sum(abs(Di-Oi))/M  # Metric: Migration Effectiveness Index (MEI)
	ANMR<-CMI*MEI/100  # Metric: Aggregate Net Migration Rate (ANMR)
	# District-level metrics:
	net<-Di-Oi
	pop0<-dis$population-net  # Population estimate for beginning of year (accounts for 1-yr movements)
	propenLD<-round(Oi/diag(OD)-1,2)  # Metric: Propensity to leave current district
	WDMI<-round(100*diag(OD)/pop0,2)  # Metric: Within District Migration Intensity (WDMI)
	turnover<-round(100*(net)/pop0,2)  # Metric: Turnover
	return(cbind(colnames(OD),net,format(WDMI,nsmall=2),format(turnover,nsmall=2)))
	#return(c(CMI,MEI,ANMR))
}

mig_metrics(1981) # mig_metrics(1991) ; mig_metrics(2001) ; mig_metrics(2011)
write.table(mig_metrics(1981),'metrics_1981.csv',row.names=F,col.names=F,sep=',')  # Save table of district-level metrics (run for each year)

# MIGRATION AGE PYRAMIDS
agelab<-c("[0,5]"="0-5","(5,10]"="6-10","(10,15]"="11-15","(15,20]"="16-20","(20,25]"="21-25","(25,30]"="26-30","(30,35]"="31-35","(35,40]"="36-40","(40,45]"="41-45","(45,50]"="46-50","(50,55]"="51-55","(55,60]"="56-60","(60,65]"="61-65","(65,70]"="66-70","(70,75]"="71-75","(75,80]"="76-80","(80,85]"="81-85","(85,90]"="> 85")

# To display the same age groups for each census year, we aggregate 2011 census age groups (85,90], (90,95], and (95,100] into "> 85"
sum85<-apply(agestr_2011[18:20,2:4],2,sum)
agestr_2011[18,]<-data.frame(age="(85,90]",male=sum85[1],female=sum85[2],total=sum85[3]) 
agestr_2011<-agestr_2011[-c(19:20),]

agestr_1981<-transform(agestr_1981,age=revalue(age,agelab))
agestr_1991<-transform(agestr_1991,age=revalue(age,agelab))
agestr_2001<-transform(agestr_2001,age=revalue(age,agelab))
agestr_2011<-transform(agestr_2011,age=revalue(age,agelab))

z<-1:18 
age1981<-data.frame(z,agestr_1981) ; age1991<-data.frame(z,agestr_1991) ; age2001<-data.frame(z,agestr_2001)  ;  age2011<-data.frame(z,agestr_2011) 

migpyr<-function(year) {  # Function to plot migration age pyramid using census data from a specific year
ggplot(year) + geom_bar(aes(x=reorder(age,z),y=male),fill="#E66551",stat="identity",colour=1,size=.2,data=year) + geom_bar(aes(x=age,y=-female),fill="#59ABE3",stat="identity",colour=1,linewidth=.2,data=year)+ coord_flip() + theme(axis.title.y=element_blank(),axis.text=element_text(size=9)) + scale_y_continuous(limits=c(-20700,20700),breaks=seq(-20000,20000,10000),labels=comma(abs(seq(-20000,20000,10000))))  + labs(y="     Male                              Female")
}

# Create 2x2 figure of all age pyramids
migration_agepyr<-ggdraw() + draw_plot(migpyr(age1981),x=0.01,y=.49,width=.49,height=.49) + draw_plot(migpyr(age1991),x=0.51,y=.49,width=0.49,height=0.49) + draw_plot(migpyr(age2001),x=0.01,y=0,width=0.49,height=0.49) + draw_plot(migpyr(age2011),x=0.51,y=0,width=0.49,height=0.49) + draw_plot_label(label=c("1981","1991","2001","2011"),size=12,x=c(0,.5,0,.5)+.06,y=c(1,1,0.51,0.51)-.035) + draw_plot_label(label=c("A","B","C","D"),size=12,x=c(0,.5,0,.5),y=c(1,1,0.51,0.51))

pdf("Migration age pyramid.pdf",7,7)
migration_agepyr
dev.off()