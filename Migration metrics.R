# CODE for computing all migration metrics used in 'Population mobility and the development of Botswana's generalized HIV epidemic: a network analysis'
# Author: Justin T. Okano

library(plyr)
library(ggplot2)
library(cowplot)
library(scales)
library(readxl)

# IMPORTS 4 files for each year:
# (i) District-level Migration matrices - 'OD_Year_0'
# (ii) Summary stat tables - 'dis_Year'
# (iii) Population age structure (5-yr groups) by gender - 'agestr_Year'
# (iv) Population totals by age (5-yr groups) and gender - 'agetot_Year'
setwd('/Users/jokano/Dropbox (UCLA Mednet)/Blower lab (NEW)/Botswana/Data/Janet/IPUMS (1981-1991-2001)/Migration/Migration Matrices/')
BW_nc<-read_excel('../../District name evolution by census/BW District name evolution 2011 names.xls')
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

if (year==2011) { # Only run this for 2011. Combines Kweneng & Kweneng East into 1 district (as 1-yr-ago data doesn't differentiate)
OD[,12]<-OD[,12]+OD[,13]
OD<-OD[,-13] 
dis[12,3:6]<-dis[12,3:6]+dis[13,3:6]
dis<-dis[-13,]
} 

P<-Pop[(year-1981)/10+1] # Population
Di<-apply(OD,2,sum) # in-migrants for district i
Oi<-apply(OD,1,sum) # out-migrants for district i
M<-sum(Di)
CMI<-100*M/P # 9.045,10.448,10.733,10.325
pop1ago<-dis$population-(apply(OD,2,sum)-apply(OD,1,sum))
propenLD<-round(apply(OD,1,sum)/diag(OD)-1,2)
WDMI<-round(100*diag(OD)/pop1ago,2)
turnover<-round(100*(apply(OD,2,sum)-apply(OD,1,sum))/pop1ago,2)

return(cbind(round(CMI,3),format(WDMI,nsmall=2),format(turnover,nsmall=2)))
}
mig_metrics(1981) # mig_metrics(1991) ; mig_metrics(2001) ; mig_metrics(2011)
write.csv(mig_metrics(1981),"Metrics_1981.csv")

# Migration age pyramids
agelab<-c("[0,5]"="0-5","(5,10]"="6-10","(10,15]"="11-15","(15,20]"="16-20","(20,25]"="21-25","(25,30]"="26-30","(30,35]"="31-35","(35,40]"="36-40","(40,45]"="41-45","(45,50]"="46-50","(50,55]"="51-55","(55,60]"="56-60","(60,65]"="61-65","(65,70]"="66-70","(70,75]"="71-75","(75,80]"="76-80","(80,85]"="81-85","(85,90]"="> 85")

agestr_1981<-transform(agestr_1981,age=revalue(age,agelab))
agestr_1991<-transform(agestr_1991,age=revalue(age,agelab))
agestr_2001<-transform(agestr_2001,age=revalue(age,agelab))
agestr_2011<-transform(agestr_2011,age=revalue(age,agelab))

z<-1:18 ; z2<-1:20
age1981<-data.frame(z,agestr_1981) ; age1991<-data.frame(z,agestr_1991) ; age2001<-data.frame(z,agestr_2001)  ;  age2011<-data.frame(z2,agestr_2011) 

migpyr1<-ggplot(age1981) + geom_bar(aes(x=reorder(age,z),y=male),fill="#E66551",stat="identity",colour=1,size=.2) + geom_bar(aes(x=age,y=-female),fill="#59ABE3",stat="identity",colour=1,size=.2)+ coord_flip() + theme(axis.title.y=element_blank(),axis.text=element_text(size=9)) + scale_y_continuous(limits=c(-20700,20700),breaks=seq(-20000,20000,10000),labels=comma(abs(seq(-20000,20000,10000))))  + labs(y="     Male                              Female") # limits=c(-8200,8000),breaks=seq(-8000,8000,4000)

migpyr2<-ggplot(age1991) + geom_bar(aes(x=reorder(age,z),y=male),fill="#E66551",stat="identity",colour=1,size=.2) + geom_bar(aes(x=age,y=-female),fill="#59ABE3",stat="identity",colour=1,size=.2)+ coord_flip() + theme(axis.title.y=element_blank(),axis.text=element_text(size=9)) + scale_y_continuous(limits=c(-20700,20700),breaks=seq(-20000,20000,10000),labels=comma(abs(seq(-20000,20000,10000)))) + labs(y="     Male                              Female") # limits=c(-14500,14500),breaks=seq(-12000,12000,4000)

migpyr3<-ggplot(age2001) + geom_bar(aes(x=reorder(age,z),y=male),fill="#E66551",stat="identity",colour=1,size=.2) + geom_bar(aes(x=age,y=-female),fill="#59ABE3",stat="identity",colour=1,size=.2)+ coord_flip() + theme(axis.title.y=element_blank(),axis.text=element_text(size=9)) + scale_y_continuous(limits=c(-20700,20700),breaks=seq(-20000,20000,10000),labels=comma(abs(seq(-20000,20000,10000))))  + labs(y="     Male                              Female") #limits=c(-17400,17400),breaks=seq(-16000,16000,4000)

migpyr4<-ggplot(age2011) + geom_bar(aes(x=reorder(age,z2),y=male),fill="#E66551",stat="identity",colour=1,size=.2) + geom_bar(aes(x=age,y=-female),fill="#59ABE3",stat="identity",colour=1,size=.2)+ coord_flip() + theme(axis.title.y=element_blank(),axis.text=element_text(size=9)) + scale_y_continuous(limits=c(-20700,20700),breaks=seq(-20000,20000,10000),labels=comma(abs(seq(-20000,20000,10000))))  + labs(y="     Male                              Female")

migfig<-ggdraw() + draw_plot(migpyr1,x=0.01,y=.49,width=.49,height=.49) + draw_plot(migpyr2,x=0.51,y=.49,width=0.49,height=0.49) + draw_plot(migpyr3,x=0.01,y=0,width=0.49,height=0.49) + draw_plot(migpyr4,x=0.51,y=0,width=0.49,height=0.49) + draw_plot_label(label=c("1981","1991","2001","2011"),size=12,x=c(0,.5,0,.5)+.06,y=c(1,1,0.51,0.51)-.035) + draw_plot_label(label=c("A","B","C","D"),size=12,x=c(0,.5,0,.5),y=c(1,1,0.51,0.51))

pdf("Migration age pyramid.pdf",7,7)
migfig
dev.off()