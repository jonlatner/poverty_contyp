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

# FOLDERS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/")

support_files = "support_files/world_bank/"
data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(janitor) #clean_names
library(data.table)

# Load EU-SILC data ----

df_eu_silc <- readRDS(file = paste0(data_files,"02_df_eu_silc_trt_sample_2year.rds"))

# Load macro variables from eurostat -----------------------------------------

# Unemployment - from the world bank
df_unmp <- read.csv(paste0(support_files,"world_bank_unemployment.csv"))
df_unmp <- clean_names(df_unmp) # janitor package
df_unmp <- pivot_longer(df_unmp,
                        cols = starts_with("x"),
                        names_to = "year", 
                        values_to = "macro_unmp_rate")
df_unmp$year <- as.numeric(gsub("^x", "", df_unmp$year))
df_unmp <- df_unmp %>%
        arrange(country_name,year)

# Add macro variables from eurostat (unemployment rate) 
df_eu_silc <- merge(x = df_eu_silc, y = df_unmp, by = c("country_name", "year"), all.x = TRUE) %>%
        arrange(country_name,panel,pid,year)

# Houshold head employment status ----

df_eu_silc <- df_eu_silc %>%
        mutate(ftc = contyp-1)

# empst_v1: 1=unemployed, 2=employed (temp), 3=employed (perm)
df_eu_silc <- df_eu_silc %>%
        mutate(empst_v1 = ifelse(empst==0, yes = 1, #unemployed
                                 ifelse(empst==1 & ftc==1, yes = 2, #employed, with a temporary contract
                                        ifelse(empst==1 & ftc==0, yes = 3, #employed, with a permanent contract
                                               ifelse(empst==2, yes = 4, #inactive (neither employed or unemployed)
                                               no = NA)))))

table(df_eu_silc$empst_v1, useNA = "ifany")

# empst_v2: 1=unemployed, 2=employed (temp + ptime), 3=employed (temp + ftime), 4=employed (perm + ptime), 5=employed (perm + ftime)
df_eu_silc <- df_eu_silc %>%
        mutate(empst_v2 = ifelse(empst == 0, yes = 1, #unemployed
                                 ifelse(empst == 1 & ftc == 1 & ftime == 0, yes = 2, #temporary contract + ptime
                                        ifelse(empst == 1 & ftc == 1 & ftime == 1, yes = 3, #temporary contract + ftime
                                               ifelse(empst == 1 & ftc == 0 & ftime == 0, yes = 4, #permanent contract + ptime
                                                      ifelse(empst == 1 & ftc == 0 & ftime == 1, yes = 5, #permanent contract + ftime
                                                             ifelse(empst==2, yes = 6, #inactive (neither employed or unemployed)
                                                             no = NA)))))))

table(df_eu_silc$empst_v2, useNA = "ifany")

# Spousal employment status ----

# with(df_eu_silc,table(marstat,partnerstat, useNA = "ifany"))
# with(subset(df_eu_silc,partnerstat==0),table(married,spouse_empst, useNA = "ifany"))
# with(df_eu_silc,table(married,spouse_empst, useNA = "ifany"))
# View(select(df_eu_silc,country,panel,pid,year,spouse_empst,marstat,married,matches("partner")) %>% filter(is.na(marstat)))
# View(select(df_eu_silc,country,panel,pid,year,spouse_empst,marstat,married,matches("partner")) %>% filter(pid==146250001))

# partner_v1: 1=single, 2=married (with unemployed spouse), 3=married (with employed spouse)
df_eu_silc <- df_eu_silc %>%
        mutate(partner_v1 = ifelse((married==0 | (married==1 & partnerstat == 0)), yes = 1,
                                   ifelse(partnerstat == 1 & married == 1 & (spouse_empst != 1 | is.na(spouse_empst)), yes = 2,
                                          ifelse((partnerstat == 1 & married == 1) & spouse_empst == 1, yes = 3,
                                                 no = NA))))

table(df_eu_silc$partner_v1, useNA = "ifany")

# Household children status ----

table(df_eu_silc$num_kids, useNA = "ifany")

df_eu_silc <- df_eu_silc %>%
        filter(!is.na(num_kids)) %>%
        mutate(kids_in_hh = ifelse(num_kids > 0, yes = 1, no = 0))

# Dependent variables ----

# transitory exit 
df_pov_t <- df_eu_silc %>%
        select(country, panel, pid, year, pov) %>%
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        filter(row_number()<3) %>%
        ungroup() %>%
        mutate(exit_pov_t = ifelse(pov == 0, yes = 1, no = 0)) %>%
        select(-pov)

df_eu_silc <- merge(df_eu_silc,df_pov_t,all.x = TRUE, by = c("country","panel","pid","year"))

df_eu_silc <- df_eu_silc %>%
        arrange(country,panel,pid,year)

# permanent exit is exit poverty in 3 consecutive time periods
# df_pov_p <- df_eu_silc %>%
#         select(country, panel, pid, year, pov) %>%
#         arrange(country, panel, pid, year) %>%
#         group_by(country, panel, pid) %>%
#         mutate(exit_pov_p = ifelse(pov == 0 & lead(pov,1) == 0 & lead(pov,2) == 0, yes = 1, no = 0)) %>%        
#         mutate(exit_pov_p = ifelse(row_number()>2, yes = NA, no = exit_pov_p)) %>%
#         ungroup() %>%
#         select(-pov)
# 
# df_eu_silc <- merge(df_eu_silc,df_pov_p,all.x = TRUE, by = c("country","panel","pid","year"))
# df_eu_silc <- df_eu_silc %>%
#         arrange(country,panel,pid,year)

# Generate unique id across country and panel periods ----

any(table(df_eu_silc$pid, df_eu_silc$year)>1)

# df_test <- select(df_eu_silc, country, panel, hid, year)
# df_test$dups <- duplicated(df_test)
# with(df_test,table(country,dups))

# df_eu_silc_2 <- merge(df_eu_silc,df_test)
# with(df_eu_silc_2,table(country,dups))

# View(filter(df_eu_silc_2,dups==TRUE))
# View(filter(df_eu_silc_2,(pid==85660001|pid==85660002) & country == "FR"))

# t <- table(df_eu_silc$pid, df_eu_silc$year)>1

df_eu_silc_pid <- df_eu_silc %>%
        select(country, panel, pid) %>%
        group_by(country, panel, pid) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(id = row_number())

df_eu_silc <- merge(x = data.table(df_eu_silc), y = data.table(df_eu_silc_pid), by = c("country","panel","pid"))

any(table(df_eu_silc$id, df_eu_silc$year)>1)

# Select variables ----

df_eu_silc <- df_eu_silc %>%
        mutate(male = ifelse(sex == 1, yes = 1, 
                             ifelse(sex == 2, yes = 0, no = NA))) %>%
        rename(weight_long=weight_long_2) %>%
        select(country, country_name, panel, pid, id, year, male, age, pov, matches("exit_pov"), empst, ftc, ftime, empst_v1, empst_v2, partner_v1, matches("macro"), kids_in_hh, num_kids, weight_long) %>%
        arrange(country, id, year)
summary(df_eu_silc)

# Country code data ----

country_name <- c("Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", "Germany",
                  "United Kingdom", "Ireland", 
                  "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
                  "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Serbia", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia",
                  "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- data.frame(cbind(country_name, region))
rm(country_name, region)

country_name <- c("Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", "Germany",
                  "United Kingdom", "Ireland", 
                  "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
                  "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia",
                  "Sweden", "Denmark", "Norway", "Finland", "Iceland")
country_name_region <- c("Switzerland (C)", "Luxembourg (C)", "Belgium (C)", "Austria (C)", "Netherlands (C)", "France (C)", "Germany (C)",
                         "United Kingdom (A)", "Ireland (A)", 
                         "Malta (S)", "Greece (S)", "Italy (S)", "Cyprus (S)", "Portugal (S)", "Spain (S)", 
                         "Romania (E)", "Poland (E)", "Croatia (E)", "Hungary (E)", "Czechia (E)", "Bulgaria (E)", "Slovenia (E)", "Slovakia (E)", "Lithuania (E)", "Estonia (E)", "Latvia (E)", "Serbia (E)",
                         "Sweden (N)", "Denmark (N)", "Norway (N)", "Finland (N)", "Iceland (N)")
geography_2 <- data.frame(cbind(country_name, country_name_region))
rm(country_name, country_name_region)
df_eu_silc <- merge(df_eu_silc,geography, by = c("country_name"), all.x = TRUE)
df_eu_silc <- merge(df_eu_silc,geography_2, by = c("country_name"), all.x = TRUE)

# Create dummy variables for employment and partner status  ----

# empst_v1: 1=unemployed, 2=employed (temp), 3=employed (perm)
table(df_eu_silc$empst_v1, useNA = "ifany")

df_eu_silc <- df_eu_silc %>%
        mutate(empst_1 = ifelse(empst_v1==1 | empst_v1 == 4, yes = 1, no = 0), #unemployed
               empst_2 = ifelse(empst_v1==2, yes = 1, no = 0), #employed, with a temporary contract
               empst_3 = ifelse(empst_v1==3, yes = 1, no = 0)) #employed, with a permanent contract

# partner_v1: 1=single, 2=married (with unemployed spouse), 3=married (with employed spouse)
table(df_eu_silc$partner_v1, useNA = "ifany")

df_eu_silc <- df_eu_silc %>%
        mutate(partner_1 = ifelse(partner_v1==1, yes = 1, no = 0),#single
               partner_2 = ifelse(partner_v1==2, yes = 1, no = 0), #married, with unemployed spouse
               partner_3 = ifelse(partner_v1==3, yes = 1, no = 0) #married, with employed spouse
        ) 

# Save ----

saveRDS(df_eu_silc, file = paste0(data_files, "03_df_eu_silc_trt_sample_clean_2year.rds"))
