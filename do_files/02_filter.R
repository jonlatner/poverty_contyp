# Top commands ----
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# Adapt this pathway!
setwd("~/GitHub/poverty_contyp/")

support_files = "support_files/"
data_files = "data_files/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(countrycode)
library(data.table)
library(car) # recode
library(readxl)
library(beepr)

# Load EU-SILC data ----

df_eu_silc_0 <- readRDS(file = paste0(data_files,"01_df_eu_silc_clean_empst.rds")) 

# Load poverty data ----

# df_pov_line comes from Eurostat - Mean and median equivalized income by household type - EU-SILC and ECHP surveys (ilc_di04)
df_pov_line <- read_xlsx(paste0(support_files,"eurostat/median_income.xlsx"), sheet = "Sheet1")

df_pov_line <- df_pov_line %>%
        rename(country_name=country)

df_pov_line$country <- countrycode(df_pov_line$country_name, 'country.name', 'genc2c')
df_pov_line <- df_pov_line %>%
        mutate(country=ifelse(country == "GB", yes = "UK", no = country))

# Merge poverty data
df_eu_silc_0 <- merge(data.table(df_eu_silc_0), data.table(df_pov_line), 
                      by = c("country","year"),
                      all.x = TRUE) %>%
        mutate(pov = ifelse(eq_hh_income<median_inc*.6, yes = 1, no = 0)) %>%
        arrange(country, pid, year)
df_eu_silc_0 <- droplevels(df_eu_silc_0)

# https://newbedev.com/r-keep-first-observation-per-group-identified-by-multiple-variables-stata-equivalent-bys-var1-var2-keep-if-n-1
step_0 <- df_eu_silc_0 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_0 <- data.table(step_0, key = "country,panel,pid")
step_0 <- step_0[, head(.SD, 1), by = key(step_0)]
# step_0 <- step_0[, .I[1], by = key(step_0)] # this also works and is slightly faster
step_0 <- data.table(step_0, key = "country,panel")
step_0 <- step_0[, .(count = .N), by = key(step_0)]
step_0$step <- 0

# step 1: Clean data ----

df_eu_silc_1 <- df_eu_silc_0

# Clean panel - make sure each panel has only 4 years

# with(df_eu_silc_1,table(year,panel,useNA = "ifany"))
df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(test = ifelse(panel == 2007 & year < 2004, yes = 1, 
                             ifelse(panel == 2008 & year < 2005, yes = 1,
                                    ifelse(panel == 2009 & year < 2006, yes = 1, 
                                           ifelse(panel == 2010 & year < 2007, yes = 1, no = 0))))) %>%
        filter(test == 0) %>%
        select(-test)
# with(df_eu_silc_1,table(year,panel,useNA = "ifany"))

# for some reason, in some country, years all observations have weight_personal_base==0
# In NL, if panel>=2016, first year has weight_personal_base==0
# In NO, if panel>=2010, only last observation in panel period has weight_personal_base>0
# df_base_weight <- df_eu_silc_1 %>%
#         select(country,pid,year,panel,weight_personal_base) %>%
#         group_by(country,panel,year) %>%
#         summarise(mean = mean(weight_personal_base)) %>%
#         ungroup()
# df_base_weight %>% filter(mean==0) %>% arrange(country,panel) %>% print(n=40)

# therefore, to keep a 4 year panel, we weight these to 1
df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(age = year - birthy) %>%
        mutate(weight_personal_base=ifelse(country == "NL" & weight_personal_base == 0 & (panel==2016|panel==2017|panel==2018|panel==2019), yes = 1, no = weight_personal_base)) %>%
        mutate(weight_personal_base=ifelse(country == "NO" & weight_personal_base == 0 & (panel>=2010), yes = 1, no = weight_personal_base)) %>%
        select(country, pid, year, weight_long_4, everything()) %>%
        filter(weight_personal_base>0) %>% # remove duplicates (country,panel,pid,year)
        arrange(country, pid, year)

# create data set to merge spousal data from
df_eu_silc_spouse_0 <- df_eu_silc_1 %>%
        select(country, panel, hid, year, pid, empst, age)

# Code full time
with(df_eu_silc_1,table(empst, useNA = "ifany"))
df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(ftime = ifelse(empst==1, yes = 1,
                              ifelse(empst==2, yes = 0, no = NA)))

# Recode employment status
# 0 = unemployed
# 1 = employed (full-time or part-time)
# NA = NILF
df_eu_silc_1$empst <- recode(df_eu_silc_1$empst, "1:2=1; 0=0; else=NA") 

# Clean contract type
# There are lots of examples where individuals are listed as unemployed (empst_1 == 3 | empst_2 == 5), but have a contract
# How do we treat these?  
# The answer is contract type variable (PL140) refers to current or last situation.
# If currently employed (PL031 = 1,2,3,4 or PL030 = 1,2), then current contract type
# If currently unemployed (PL031 > 4 or PL030 > 2), then previous situation
# Therefore, if you are unemployed, then you do not have contract type
df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(contyp = ifelse(empst==0, yes = NA, no = contyp))

step_1 <- df_eu_silc_1 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_1 <- data.table(step_1, key = "country,panel,pid")
step_1 <- step_1[, head(.SD, 1), by = key(step_1)]
step_1 <- data.table(step_1, key = "country,panel")
step_1 <- step_1[, .(count = .N), by = key(step_1)]
step_1$step <- 1

# step 2: Individual-level filters ----

df_eu_silc_2 <- df_eu_silc_1

# if weight_personal_base==0, we drop observations 
# the reason, these appear to be duplicates, albeit not identical.
# df_dups <- df_eu_silc_2 %>%
#         select(country, panel, pid, year, empst, matches("gov_inc_hh"), eq_hh_income, pov, weight_personal_base, sex, marstat, age)
# df_dups$dups <- duplicated(df_dups)
# df_dups %>% filter(dups==TRUE)

df_eu_silc_2 <- df_eu_silc_2 %>%
        filter(age >= 20 & age <= 60) %>%
        filter(!is.na(eq_hh_income) & eq_hh_income>0) %>% # must have non missing income 
        filter(empst == 0 | empst == 1) %>% # employed or unemployed, drop self-employed (empst == 3 | empst == 4)
        select(country, pid, year, weight_long_4, everything()) %>%
        arrange(country, pid, year)

# drop if contract information is missing and employed 
df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(missing = ifelse(empst == 1 & is.na(contyp), yes = 1, no = 0)) %>%
        filter(missing == 0) %>%
        select(-missing)

step_2 <- df_eu_silc_2 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_2 <- data.table(step_2, key = "country,panel,pid")
step_2 <- step_2[, head(.SD, 1), by = key(step_2)]
# step_2 <- step_2[, .I[1], by = key(step_2)] # this also works and is slightly faster
step_2 <- data.table(step_2, key = "country,panel")
step_2 <- step_2[, .(count = .N), by = key(step_2)]
step_2$step <- 2

# step 3-5: Treatment sample ----
# Poor and employed with temporary contract in first year of 4-year study period

df_eu_silc_3 <- df_eu_silc_2

# Select first observation from each unique country, panel, pid
df_first <- df_eu_silc_3 %>%
        select(country, panel, year, pid, empst, contyp, pov) %>%
        mutate(count = 1) 
df_first <- data.table(df_first, key = "country,panel,pid")
df_first <- df_first[, head(.SD, 1), by = key(df_first)]

# Step 3 - In first observation period: poor
df_trtmnt <- df_first %>%
        mutate(trtmnt = ifelse(pov == 1, yes = 1 , no = 0)) %>%
        filter(trtmnt == 1) %>%
        select(country,panel,pid,trtmnt)
nrow(df_trtmnt)

df_eu_silc_3 <- merge(data.table(df_eu_silc_3), data.table(df_trtmnt), by = c("country", "panel", "pid"), all.x = TRUE)

df_eu_silc_3 <- df_eu_silc_3 %>%
        arrange(country, panel, pid, year) %>%
        filter(trtmnt == 1) %>%
        select(-trtmnt)

step_3 <- df_eu_silc_3 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_3 <- data.table(step_3, key = "country,panel,pid")
step_3 <- step_3[, head(.SD, 1), by = key(step_3)]
step_3 <- data.table(step_3, key = "country,panel")
step_3 <- step_3[, .(count = .N), by = key(step_3)]
step_3$step <- 3

# Step 4 - In first observation period: poor + employed
df_trtmnt <- df_first %>%
        mutate(trtmnt = ifelse(pov == 1 & empst == 1, yes = 1 , no = 0)) %>%
        filter(trtmnt == 1) %>%
        select(country,panel,pid,trtmnt)
nrow(df_trtmnt)

df_eu_silc_4 <- merge(data.table(df_eu_silc_3), data.table(df_trtmnt), by = c("country", "panel", "pid"), all.x = TRUE)

df_eu_silc_4 <- df_eu_silc_4 %>%
        arrange(country, panel, pid, year) %>%
        filter(trtmnt == 1) %>%
        select(-trtmnt)

step_4 <- df_eu_silc_4 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_4 <- data.table(step_4, key = "country,panel,pid")
step_4 <- step_4[, head(.SD, 1), by = key(step_4)]
step_4 <- data.table(step_4, key = "country,panel")
step_4 <- step_4[, .(count = .N), by = key(step_4)]
step_4$step <- 4

# Step 5 - In first observation period: poor + employed + temporary contract
df_trtmnt <- df_first %>%
        mutate(trtmnt = ifelse(pov == 1 & empst == 1 & contyp == 2, yes = 1 , no = 0)) %>%
        filter(trtmnt == 1) %>%
        select(country,panel,pid,trtmnt)
nrow(df_trtmnt)

df_eu_silc_5 <- merge(data.table(df_eu_silc_4), data.table(df_trtmnt), by = c("country", "panel", "pid"), all.x = TRUE)

df_eu_silc_5 <- df_eu_silc_5 %>%
        arrange(country, panel, pid, year) %>%
        filter(trtmnt == 1) %>%
        select(-trtmnt)

step_5 <- df_eu_silc_5 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_5 <- data.table(step_5, key = "country,panel,pid")
step_5 <- step_5[, head(.SD, 1), by = key(step_5)]
step_5 <- data.table(step_5, key = "country,panel")
step_5 <- step_5[, .(count = .N), by = key(step_5)]
step_5$step <- 5

# step 6: Partner employment status ----
# drop observations who are married with partner id, but missing partner employment information

# rename variables
df_eu_silc_spouse_1 <- df_eu_silc_spouse_0 %>%
        rename(spouse_empst = empst,
               spouse_age = age)

# Recode spouse employment status
# unemployed/NLIF = 0, 
# employed = 1, 
# else = NA
df_eu_silc_spouse_1$spouse_empst <- recode(df_eu_silc_spouse_1$spouse_empst,
                                           "1:2=1;
                                           3=0;
                                           0=0;
                                           else = NA")

df_eu_silc_spouse_1 <- df_eu_silc_spouse_1 %>%
        select(country,panel,hid,year,pid,spouse_empst,spouse_age)

# merge in partner data
df_eu_silc_6 <- merge(data.table(df_eu_silc_5), data.table(df_eu_silc_spouse_1), by.x = c("country","panel","hid","year","partner_id"), by.y = c("country","panel","hid","year","pid"), all.x = TRUE)

# partner status
df_eu_silc_6 <- df_eu_silc_6 %>%
        mutate(partnerstat = ifelse(!is.na(partner_id), yes = 1, no = 0))

# if partner is present and married is missing, then "married"
# if no partner is present and married is missing, then single
df_eu_silc_6 <- df_eu_silc_6 %>%
        mutate(marstat=ifelse(partnerstat == 1 & is.na(marstat), yes = 2, no = marstat)) %>% 
        mutate(marstat=ifelse(partnerstat == 0 & is.na(marstat), yes = 1, no = marstat)) %>% 
        mutate(married = ifelse(marstat == 2, yes = 1, no = 0))

df_eu_silc_6 <- df_eu_silc_6 %>%
        mutate(missing = ifelse(partnerstat == 1 & married == 1 & is.na(spouse_empst), yes = 1, no = 0))
table(df_eu_silc_6$missing)

# if mising, then drop
df_eu_silc_6 <- df_eu_silc_6 %>%
        filter(missing == 0) %>%
        select(-missing)

# there are missings for number of kids/children, but only in Malta
summary(df_eu_silc_6$num_kids)
with(df_eu_silc_6,table(country,num_kids,useNA = "ifany"))
beep()

df_eu_silc_6 <- df_eu_silc_6 %>%
        filter(!is.na(num_kids))

step_6 <- df_eu_silc_6 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_6 <- data.table(step_6, key = "country,panel,pid")
step_6 <- step_6[, head(.SD, 1), by = key(step_6)]
step_6 <- data.table(step_6, key = "country,panel")
step_6 <- step_6[, .(count = .N), by = key(step_6)]
step_6$step <- 6

# step 7: keep only if observation is present in first two sequential years in a given four year panel period ----
df_eu_silc_7 <- df_eu_silc_6

df_eu_silc_7 <- df_eu_silc_7 %>%
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        mutate(number = ifelse(year == lag(year,1)+1, yes = 1, no = 0),
               number = ifelse(row_number()==1, yes = 1, no = number),
               count = cumsum(number),
               row = row_number(),
               max_count = max(count)) %>%
        ungroup()

# drop observations situations like this
with(df_eu_silc_7,table(row,count,useNA = "ifany"))
df_eu_silc_7 %>% filter(row==2 & count!=2) %>% select(country, panel, pid, year,number,count,row) 
df_eu_silc_7 %>% filter(pid==97590001) %>% select(country, panel, pid, year,number,count,row,max_count) 

df_eu_silc_7 <- df_eu_silc_7 %>%
        filter(count==row & max_count>=2)

# check
with(df_eu_silc_7,table(row,count,useNA = "ifany"))
df_eu_silc_7 %>% filter(row==2 & count!=2) %>% select(country, panel, pid, year,number,count,row) 
df_eu_silc_7 %>% filter(pid==97590001) %>% select(country, panel, pid, year,number,count,row,max_count) 

df_eu_silc_7 <- df_eu_silc_7 %>%
        select(-number,-count,-row,-max_count)

step_7 <- df_eu_silc_7 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_7 <- data.table(step_7, key = "country,panel,pid")
step_7 <- step_7[, head(.SD, 1), by = key(step_7)]
step_7 <- data.table(step_7, key = "country,panel")
step_7 <- step_7[, .(count = .N), by = key(step_7)]
step_7$step <- 7

# step 8: keep only if observation is present in all four years in a given four year panel period ----
df_eu_silc_8 <- df_eu_silc_6

df_eu_silc_8 <- df_eu_silc_8 %>%
        group_by(country, panel, pid) %>%
        arrange(country, panel, pid, year) %>%
        mutate(number = row_number(),
               count = max(number)) %>%
        ungroup() %>%
        filter(count >= 4 & number <= 4) %>%
        select(-number,-count)

step_8 <- df_eu_silc_8 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_8 <- data.table(step_8, key = "country,panel,pid")
step_8 <- step_8[, head(.SD, 1), by = key(step_8)]
step_8 <- data.table(step_8, key = "country,panel")
step_8 <- step_8[, .(count = .N), by = key(step_8)]
step_8$step <- 8

# Save ----

saveRDS(df_eu_silc_7, file = paste0(data_files, "02_df_eu_silc_trt_sample_2year.rds"))
saveRDS(df_eu_silc_8, file = paste0(data_files, "02_df_eu_silc_trt_sample_4year.rds"))

df_filter_steps <- rbind(step_0,step_1,step_2,step_3,step_4,step_5,step_6,step_7,step_8)
saveRDS(df_filter_steps, file = paste0(data_files, "df_eu_silc_filter_steps.rds"))

beep()
