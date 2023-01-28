# IPUMS DATA

library(haven)
library(dplyr)
library(tidyr)

setwd("/Users/janetsong/Desktop/Botswana")

data <- read_sav("Data/1991-2001 Census/ipumsi_00006.sav")
bw <- data
settlements_list <- read.csv("2011 Analysis/Settlement_Coordinates_Checked_R.csv",stringsAsFactors = FALSE)

district_index <- sort(unique(settlements_list$dist_no)) # the district numbers after 7 do not increment by 1, so this vector is used as index in the for loop
head(data)



migration_district <- function(data, year,district_past, district_now, residence_past, residence_now){
  # formatting usual residence bw
  
  databw <- subset(data, data$YEAR == year)
  databw <- subset(data, data$SEX != "9")
  databw_filter <- subset(databw,databw[[district_past]] > 0 & databw[[district_past]] <= 91) # removing 95, 98, 99 for foreign country, unknown, and not in universe
  databw_filter <- subset(databw_filter,databw_filter[[district_now]] >0 & databw_filter[[district_now]] <= 91) # removing 95, 98, 99 for foreign country, unknown, and not in universe
  databw_filter$res <- databw_filter[[residence_now]] # creating a separate column for residence
  databw_filter$res1yr <- databw_filter[[residence_past]]
  databw_filter$district <- factor(databw_filter[[district_now]], levels = names(table(databw_filter[[district_now]]))) #creating a factor column for district now
  databw_filter$dist1yr <- factor(databw_filter[[district_past]], levels = names(table(databw_filter[[district_past]]))) #creating a factor column for district 1 year ago
  c1 <- sort(unique(databw_filter$dist1yr))
  c2 <- sort(unique(databw_filter$district))
  # 1981 Census
  g1 <- databw_filter[databw_filter$res %in% c(1,2),] #75478 people in group 1 now
  g2 <- databw_filter[databw_filter$res == 3,] # groups 2 and 3 combined for easier computing: 12515
  g4 <- databw_filter[databw_filter$res == 4,] # 503
  g5 <- databw_filter[databw_filter$res == 8 | databw_filter$res == 9,] #4212
  
  # Group 1 now 
  g1a <- subset(g1, g1$res1yr ==3) # people that moved
  
  l1 <- length(c1)
  l2 <- length(c2)
  move1<-matrix(0,l1,l2,dimnames=list(c1,c2)) # Empty 28x28 matrix
  
  for (i in 1:l1) {
    ind <- c1[i]
    move1[i,]<-table(factor(g1a$district[g1a$dist1yr==ind],levels=levels(c2)))  }
  # checks:
  check1 <- sum(move1) == nrow(g1a)
  check2 <- all(rowSums(move1) == table(g1a$dist1yr))
  print("check group 1")
  cat("all people accounted for in move:", check1, "\n")
  cat("all rows the same:")
  print(rowSums(move1) == table(g1a$dist1yr))
  

  # Group 2 now
  # quick check: table(g2$res1yr)
  # people that actually moved from group 2 now:people that reported to be elsewhere in BW now, but are in this locality 1 year ago (2557) 
  # and people that are elsewhere in BW now and not in the same district elsewhere in BW 1 year ago (427)
  # total: 2984
  g2a <- subset(g2, g2$res1yr != 3 | g2$res1yr == 3 & as.character(g2$dist1yr) != as.character(g2$district)) # dataframe with only people who definitely moved
  
  move2<-matrix(0,l1,l2,dimnames=list(c1,c2)) # Empty 28x28 matrix
  
  for (i in 1:l1) {
    ind <- c1[i]
    move2[i,]<-table(factor(g2a$district[g2a$dist1yr==ind],levels= levels(c2)))  }
  # checks:
  check1 <- sum(move2) == nrow(g2a)
  check2 <- all(rowSums(move2) == table(g2a$dist1yr))
  print("check group 2")
  cat("all people accounted for in move:", sum(move2) == nrow(g2a), "\n")
  cat("all rows the same:")
  print(rowSums(move2) == table(g2a$dist1yr))
  
  
  # Total
  # adding move1 table and move2 table
  move <- move1 + move2
  # checks:
  check1 <- sum(move) == nrow(g1a) + nrow(g2a)
  check2 <- all(sum(move) == nrow(g1a) + nrow(g2a))
  print("check total")
  cat("all people accounted for in move:", check1, "\n")
  cat("all rows the same:")
  print(rowSums(move) == table(g1a$dist1yr) + table(g2a$dist1yr))
  return(move)
}

# bw is original data
# stratifying by sex:
# female:
bw_f <- subset(bw, bw$SEX==2)
bw_m <- subset(bw, bw$SEX==1)

# 1981
m1981 <- migration_district(data = bw, year = 1981, district_past = "BW1981A_DIST1YR", district_now ="BW1981A_RESDIST", residence_past =  "BW1981A_PLACE1YR", residence_now = "BW1981A_RESPLACE") # 1 year move for people in 1981
m1981 <- m1981 *10 # multiply by factor of 10 to account for 10% sample
write.csv(m1981, "ipums1981_1yr_migration.csv")
m1981 <- read.csv("IPUMS 1981-2001/ipums1981_1yr_migration.csv")

m_1981_f <- migration_district(data =bw_f, year = 1981, district_past = "BW1981A_DIST1YR", district_now ="BW1981A_RESDIST", residence_past =  "BW1981A_PLACE1YR", residence_now = "BW1981A_RESPLACE") # 1 year move for people in 1981
m_1981_f <- m_1981_f *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_1981_f, "ipums_female_1981_1yr_migration.csv")

m_1981_m <- migration_district(data =bw_m, year = 1981, district_past = "BW1981A_DIST1YR", district_now ="BW1981A_RESDIST", residence_past =  "BW1981A_PLACE1YR", residence_now = "BW1981A_RESPLACE") # 1 year move for people in 1981
m_1981_m <- m_1981_m *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_1981_m, "ipums_male_1981_1yr_migration.csv")

# 1991
m1991 <- migration_district(data = bw, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991
m1991 <- m1991 *10
write.csv(m1991, "ipums1991_1yr_migration.csv")
m1991 <- read.csv("ipums1991_1yr_migration.csv")

m_1991_f <- migration_district(data =bw_f, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991
m_1991_f <- m_1991_f *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_1991_f, "ipums_female_1991_1yr_migration.csv")

m_1991_m <- migration_district(data =bw_m, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991
m_1991_m <- m_1991_m *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_1991_m, "ipums_male_1991_1yr_migration.csv")

# 2001
m2001 <- migration_district(data = bw, year = 2001, district_past = "BW2001A_MIG1DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG1PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
m2001 <- m2001 * 10
write.csv(m2001, "ipums2001_1yr_migration.csv")
m2001 <- read.csv("ipums2001_1yr_migration.csv")

m_2001_f <- migration_district(data = bw_f, year = 2001, district_past = "BW2001A_MIG1DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG1PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
m_2001_f <- m_2001_f *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_2001_f, "ipums_female_2001_1yr_migration.csv")

m_2001_m <- migration_district(data = bw_m, year = 2001, district_past = "BW2001A_MIG1DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG1PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
m_2001_m <- m_2001_m *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_2001_m, "ipums_male_2001_1yr_migration.csv")

m2001_5yr <- migration_district(data = bw, year = 2001, district_past = "BW2001A_MIG5DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG5PLC", residence_now = "BW2001A_USUPLACE") # 5 year move for people in 2001
m2001_5yr <- m2001_5yr * 10
write.csv(m2001_5yr, "ipums2001_5yrs_migration.csv")
m2001_5yr <- read.csv("ipums2001_5yrs_migration.csv")

m_2001_5yr_f <- migration_district(data = bw_f, year = 2001, district_past = "BW2001A_MIG5DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG5PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
m_2001_5yr_f <- m_2001_5yr_f *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_2001_5yr_f, "ipums_female_2001_5yr_migration.csv")

m_2001_5yr_m <- migration_district(data = bw_m, year = 2001, district_past = "BW2001A_MIG5DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG5PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
m_2001_5yr_m <- m_2001_5yr_m *10 # multiply by factor of 10 to account for 10% sample
write.csv(m_2001_5yr_m, "ipums_male_2001_5yr_migration.csv")


# 1. urban to urban
# 2. urban to rural
# 3. rural to urban
# 4. rural to rural
bw1991 <- subset(bw, bw$YEAR == 1991)

# 1. urban to urban 1 year migration
# creating data frame that only has urban to urban migrants
bw1991u_u <- subset(bw1991, bw1991$BW1991A_ZONECODE == 1 & bw1991$BW1991A_ZONECODE2 == 1)
u_u1991 <- migration_district(data = bw1991u_u, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991

# 2. Urban to rural 1 year migration
# creating a data frame that has people living in urban areas 1 year ago and rural areas now
bw1991u_r <- subset(bw1991, bw1991$BW1991A_ZONECODE == 2 & bw1991$BW1991A_ZONECODE2 == 1)
u_r1991 <- migration_district(data = bw1991u_r, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991

# 3. rural to urban 1 year migration
# creating a data frame that has people living in rural areas 1 year ago and urban areas now
bw1991r_u <- subset(bw1991, bw1991$BW1991A_ZONECODE == 1 & bw1991$BW1991A_ZONECODE2 == 2)
r_u1991 <- migration_district(data = bw1991r_u, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991

# 4. rural to rural 1 year migration
# creating data frame that only has rural to rural migrants
bw1991r_r <- subset(bw1991, bw1991$BW1991A_ZONECODE == 2 & bw1991$BW1991A_ZONECODE2 == 2)
r_r1991 <- migration_district(data = bw1991r_r, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991

dim(u_u1991) # 19x19
dim(u_r1991) # 19x18
dim(r_u1991) # 18x19
dim(r_r1991) # 18x18
# adding "_u" or "_r" to column names so it is easy to distinguish between urban and rural areas
colnames(u_u1991) <- paste(colnames(u_u1991), "_u", sep = "")
rownames(u_u1991) <- paste(rownames(u_u1991), "_u", sep = "")

colnames(u_r1991) <- paste(colnames(u_r1991), "_r", sep = "")
rownames(u_r1991) <- paste(rownames(u_r1991), "_u", sep = "")

colnames(r_u1991) <- paste(colnames(r_u1991), "_u", sep = "")
rownames(r_u1991) <- paste(rownames(r_u1991), "_r", sep = "")

colnames(r_r1991) <- paste(colnames(r_r1991), "_r", sep = "")
rownames(r_r1991) <- paste(rownames(r_r1991), "_r", sep = "")

# combining tables
a1 <- cbind(u_u1991, u_r1991)
a2 <- cbind(r_u1991, r_r1991)
urban_1991 <- rbind(a1,a2)
urban_1991 <- urban_1991*10
write.csv(urban_1991, "urban_rural_migration_1yr_1991.csv")

urban_1991 <- read.csv("urban_rural_migration_1yr_1991.csv")

# Age Structure Analysis:

age_structure_all<- function(df, age_var, sex_var, grouped = FALSE){
  df_age<-df
  df_age$p06 <- df_age[[age_var]]
  df_age$p05 <- df_age[[sex_var]]
  head(df_age)
  if(grouped){
    df_age$age_by_5 <- cut(df_age$p06, breaks = seq(0,100,by=5), include.lowest = TRUE)
    head(df_age$age_by_5)
    df_tot <- df_age %>% group_by(p05,age_by_5) %>% summarise(count = n()) %>% spread(key = "p05", value ="count")
  } else{
    df_tot <- df_age %>% group_by(p05,p06) %>% summarise(count = n()) %>% spread(key = "p05", value ="count")
  }
  colnames(df_tot) <- c('age', 'male','female')
  df_tot[2:3] <- df_tot[2:3]*10
  df_tot[2:3][is.na(df_tot[2:3])] <-0
  df_tot <- df_tot %>% mutate(total = male+female)
  return(df_tot)
}

# Who moved
moved_group <- function(data, year,district_past, district_now, residence_past, residence_now){
  databw <- subset(data, data$YEAR == year)
  databw_filter <- subset(databw,databw[[district_past]] > 0 & databw[[district_past]] <= 91) # removing 95, 98, 99 for foreign country, unknown, and not in universe
  databw_filter <- subset(databw_filter,databw_filter[[district_now]] >0 & databw_filter[[district_now]] <= 91) # removing 95, 98, 99 for foreign country, unknown, and not in universe
  databw_filter$res <- databw_filter[[residence_now]] # creating a separate column for residence
  databw_filter$res1yr <- databw_filter[[residence_past]]
  databw_filter$district <- factor(databw_filter[[district_now]], levels = names(table(databw_filter[[district_now]]))) #creating a factor column for district now
  databw_filter$dist1yr <- factor(databw_filter[[district_past]], levels = names(table(databw_filter[[district_past]]))) #creating a factor column for district 1 year ago
  # 1981 Census
  g1 <- databw_filter[databw_filter$res %in% c(1,2),] #75478 people in group 1 now
  g2 <- databw_filter[databw_filter$res == 3,] # groups 2 and 3 combined for easier computing: 12515

  g1a <- subset(g1, !g1$res1yr %in% c(1,2)) # people that moved
  g2a <- subset(g2, g2$res1yr != 3 | g2$res1yr == 3 & as.character(g2$dist1yr) != as.character(g2$district)) # dataframe with only people who definitely moved
  moved <- rbind(g2a, g1a)
  return(moved)
}


## 1981:
### Total Population:
bw1981 <- subset(bw, bw$YEAR == 1981 & bw$BW1981A_SEX %in% c(1,2)& bw$BW1981A_AGE < 99)
bw1981 <- zap_label(bw1981)
# all ages by one year (0-90+)
age_tot1981 <- age_structure_all(bw1981, age_var = "AGE", sex_var = "SEX")
write.csv(age_tot1981, "ages_total_population_all_1981.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
age_grouped1981 <- age_structure_all(bw1981, age_var = "BW1981A_AGE", sex_var = "BW1981A_SEX", grouped = TRUE)
write.csv(age_grouped1981, "ages_total_population_grouped_1981.csv", row.names = FALSE)

### 1 Year Migrant Population
move1981 <- moved_group(data = bw, year = 1981, district_past = "BW1981A_DIST1YR", district_now ="BW1981A_RESDIST", residence_past =  "BW1981A_PLACE1YR", residence_now = "BW1981A_RESPLACE") # 1 year move for people in 1981
move1981 <- zap_labels(move1981)
# all ages by one year (0-90+)

### 1 Year Migrant Population
move_age_tot1981 <- age_structure_all(move1981, age_var = "AGE", sex_var = "SEX")
write.csv(move_age_tot1981,"ages_moved_all_1981.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
move_age_grouped1981 <- age_structure_all(move1981, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(move_age_grouped1981,"ages_moved_grouped_1981.csv", row.names = FALSE)

### Churn tables
churn <- function(m){
  sum1 = colSums(m)
  sum2 = rowSums(m)
  c1 = rownames_to_column(as.data.frame(sum1), "dist_no")
  colnames(c1)[2] = "net_in"
  c2 = rownames_to_column(as.data.frame(sum2), "dist_no")
  colnames(c2)[2] = "net_out"
  res = merge(c1,c2, all = TRUE)
  res$dist_no = as.integer(res$dist_no)
  res[is.na(res)] = 0
  print(res)
  res$net_change = res$net_in + res$net_out
  return(res)
}

tot81 <- migration_district(data = bw, year = 1981, district_past = "BW1981A_DIST1YR", district_now ="BW1981A_RESDIST", residence_past =  "BW1981A_PLACE1YR", residence_now = "BW1981A_RESPLACE")*10 # 1 year move for people in 1981
#0) size of district
dist_size81 <- bw1981 %>% group_by(BW1981A_RESDIST) %>% summarise(n()*10) %>% as.data.frame() %>% filter(BW1981A_RESDIST<=91)
colnames(dist_size81) <- c("dist_no", "population")
dist_size81 <- zap_labels(dist_size81)

#i) District-level Net-growth = (in-migrants - out-migrants) I think in-migrants would be the column-sums & out-migrants would be the row-sums in your migration tables.  
growth81 <- as.data.frame(colSums(tot81) - rowSums(tot81) )
growth81 <- rownames_to_column(growth81, "dist_no")
growth81$dist_no <- as.integer(growth81$dist_no)
colnames(growth81)[2] <- "net_growth"

#ii) District-level Churn =  (Net-growth)/district-size.
churn81 <- left_join(dist_size81, growth81) %>% merge(churn(tot81), all = TRUE)
churn81$churn <- churn81$net_change/churn81$population
write.csv(churn81, "district_size_growth_churn_1yr_1981.csv")


## 1991:
### Total Population
bw1991 <- subset(bw, bw$YEAR == 1991 & bw$SEX %in% c(1,2))
bw1991 <- zap_label(bw1991)
# all ages by one year (0-90+)
age_tot1991 <- age_structure_all(bw1991, age_var = "AGE", sex_var = "SEX")
write.csv(age_tot1991, "ages_total_population_all_1991.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
age_grouped1991 <- age_structure_all(bw1991, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(age_grouped1991, "ages_total_population_grouped_1991.csv", row.names = FALSE)

### 1 Year Migrant Population
move1991 <- moved_group(data = bw, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES") # 1 year move for people in 1991
move1991 <- zap_labels(move1991)
# all ages by one year (0-90+)
move_age_tot1991 <- age_structure_all(move1991, age_var = "AGE", sex_var = "SEX")
write.csv(move_age_tot1991, "ages_moved_all_1991.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
move_age_grouped1991 <- age_structure_all(move1991, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(move_age_grouped1991, "ages_moved_grouped_1991.csv", row.names = FALSE)

tot91 <- migration_district(data = bw, year = 1991, district_past = "BW1991A_DISTYEAR", district_now ="BW1991A_DISTNOW", residence_past =  "BW1991A_RES1YR", residence_now = "BW1991A_USURES")*10 # 1 year move for people in 1991
#0) size of district
dist_size91 <- bw1991 %>% group_by(BW1991A_DISTNOW) %>% summarise(n()*10) %>% as.data.frame() %>% filter(BW1991A_DISTNOW<=91)
colnames(dist_size91) <- c("dist_no", "population")
dist_size91 <- zap_labels(dist_size91)

#i) District-level Net-growth = (in-migrants - out-migrants) I think in-migrants would be the column-sums & out-migrants would be the row-sums in your migration tables.  
growth91 <- as.data.frame(colSums(tot91) - rowSums(tot91) )
growth91 <- rownames_to_column(growth91, "dist_no")
growth91$dist_no <- as.integer(growth91$dist_no)
colnames(growth91)[2] <- "net_growth"

#ii) District-level Churn =  (Net-growth)/district-size.
churn91 <- left_join(dist_size91, growth91)  %>% merge(churn(tot91), all = TRUE)
churn91$churn <- churn91$net_change/churn91$population
write.csv(churn91, "district_size_growth_churn_1yr_1991.csv")

## 2001:
### Total Population
bw2001 <- subset(bw, bw$YEAR == 2001 & bw$SEX %in% c(1,2))
bw2001 <- zap_label(bw2001)
# all ages by one year (0-90+)
age_tot2001 <- age_structure_all(bw2001, age_var = "AGE", sex_var = "SEX")
write.csv(age_tot2001, "ages_total_population_all_2001.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
age_grouped2001 <- age_structure_all(bw2001, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(age_grouped2001, "ages_total_population_grouped_2001.csv", row.names = FALSE)

### 1 Year Migrant Population
move2001 <- moved_group(data = bw, year = 2001, district_past = "BW2001A_MIG1DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG1PLC", residence_now = "BW2001A_USUPLACE") # 1 year move for people in 2001
move2001 <- zap_labels(move2001)
# all ages by one year (0-90+)
move_age_tot2001 <- age_structure_all(move2001, age_var = "AGE", sex_var = "SEX")
write.csv(move_age_tot2001, "ages_moved_all_2001.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
move_age_grouped2001 <- age_structure_all(move2001, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(move_age_grouped2001, "ages_moved_grouped_2001.csv", row.names = FALSE)

tot01 <- migration_district(data = bw,  year = 2001, district_past = "BW2001A_MIG1DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG1PLC", residence_now = "BW2001A_USUPLACE")*10 # 1 year move for people in 2001
#0) size of district
dist_size01 <- bw2001 %>% group_by(BW2001A_USUALDIST) %>% summarise(n()*10) %>% as.data.frame() %>% filter(BW2001A_USUALDIST<=91)
colnames(dist_size01) <- c("dist_no", "population")
dist_size01 <- zap_labels(dist_size01)

#i) District-level Net-growth = (in-migrants - out-migrants) I think in-migrants would be the column-sums & out-migrants would be the row-sums in your migration tables.  
growth01 <- as.data.frame(colSums(tot01) - rowSums(tot01) )
growth01 <- rownames_to_column(growth01, "dist_no")
growth01$dist_no <- as.integer(growth01$dist_no)
colnames(growth01)[2] <- "net_growth"

#ii) District-level Churn =  (Net-growth)/district-size.
churn01 <- left_join(dist_size01, growth01) %>% left_join(churn(tot01))
churn01$churn <- churn01$net_change/churn01$population
write.csv(churn01, "district_size_growth_churn_1yr_2001.csv")

### 5 Year Migrant Population
move2001_5yr <- moved_group(data = bw, year = 2001, district_past = "BW2001A_MIG5DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG5PLC", residence_now = "BW2001A_USUPLACE") # 5 year move for people in 2001
move2001_5yr <- zap_labels(move2001_5yr)
# all ages by one year (0-90+)
move_age_tot2001_5yr <- age_structure_all(move2001_5yr, age_var = "AGE", sex_var = "SEX")
write.csv(move_age_tot2001_5yr, "ages_moved_all_2001_5yr.csv", row.names = FALSE)

# organizing age structure by groups of five so it is easier to read
move_age_grouped2001_5yr <- age_structure_all(move2001_5yr, age_var = "AGE", sex_var = "SEX", grouped = TRUE)
write.csv(move_age_grouped2001_5yr, "ages_moved_grouped_2001_5yr.csv", row.names = FALSE)

tot01_5yr <- migration_district(data = bw,  year = 2001, district_past = "BW2001A_MIG5DIST", district_now ="BW2001A_USUALDIST", residence_past =  "BW2001A_MIG5PLC", residence_now = "BW2001A_USUPLACE")*10 # 1 year move for people in 2001_5yr
#0) size of district
dist_size01_5yr <- bw2001 %>% group_by(BW2001A_USUALDIST) %>% summarise(n()*10) %>% as.data.frame() %>% filter(BW2001A_USUALDIST<=91)
colnames(dist_size01_5yr) <- c("dist_no", "population")
dist_size01_5yr <- zap_labels(dist_size01_5yr)

#i) District-level Net-growth = (in-migrants - out-migrants) I think in-migrants would be the column-sums & out-migrants would be the row-sums in your migration tables.  
growth01_5yr <- as.data.frame(colSums(tot01_5yr) - rowSums(tot01_5yr) )
growth01_5yr <- rownames_to_column(growth01_5yr, "dist_no")
growth01_5yr$dist_no <- as.integer(growth01_5yr$dist_no)
colnames(growth01_5yr)[2] <- "net_growth"

#ii) District-level Churn =  (Net-growth)/district-size.
churn01_5yr <- left_join(dist_size01_5yr, growth01_5yr) %>% merge(churn(tot01_5yr), all = TRUE)
churn01_5yr$churn <- churn01_5yr$net_change/churn01_5yr$population
write.csv(churn01_5yr, "district_size_growth_churn_5yr_2001.csv")






