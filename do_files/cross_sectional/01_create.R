# Top commands --------------------------------------------------------------
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

orig_data_files = "raw_data/EU_SILC_2019/Cross_2004_19/data_files/"
project_data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(data.table)
library(countrycode)

# Variables data -----------------------------------------

# personal
# label(data_p$PB010) <- "Year of the survey" 
# label(data_p$PB020) <- "Country alphanumeric" 
# label(data_p$PB030) <- "Personal ID" 
# label(data_p$PB040) <- "Personal cross-sectional weight" 
# label(data_p$PB140) <- "Year of birth (DE: Age group pertubation & bottom code; MT: 5 yr groups)" 
# label(data_p$PL140) <- "Type of contract" 
# label(data_p$PL030) <- "Self-defined current economic status" (2004-2011)
# label(data_p$PL031) <- "Self-defined current economic status" (2011-) 
# label(data_p$PX030) <- "Household ID" 

# Household
# label(data_h$HB010) <- "Year of the survey" 
# label(data_h$HB020) <- "Country alphanumeric" 
# label(data_h$HB030) <- "Household ID" ;
# label(data_h$HX090) <- "Equivalised disposable income  (SI: Adjustments)"

# Household register
# label(data_d$DB010) <- "Year of the Survey" 
# label(data_d$DB020) <- "Country alphanumeric" 
# label(data_d$DB030) <-  "Household ID" 
# label(data_d$DB090)  <- "Household cross-sectional weight" 

# Variables -----------------------------------------

vars_personal_1 <- c("PB010", "PB020", "PB030", "PB040", "PB140", "PL140", "PL030", "PX030")
vars_personal_2 <- c("PB010", "PB020", "PB030", "PB040", "PB140", "PL140", "PL031", "PX030")
vars_household_1 <- c("HB010", "HB020", "HB030", "HX090")
vars_household_reg_1 <- c("DB010", "DB020", "DB030", "DB090")

# Load personal data -----------------------------------------

test <- data.frame()
year <- seq(2004,2008,1)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- mutate(df, PB010 = as.numeric(as.character(PB010)))
        df <- select(df, all_of(vars_personal_1))
        df <- rename(df, year = PB010)
        df <- rename(df, country = PB020)
        df <- rename(df, hhid = PX030)
        test <- rbind(test,df)
}

personal_1 <- test

test <- data.frame()
year <- seq(2009,2019,1)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- mutate(df, PB010 = as.numeric(as.character(PB010)))
        df <- select(df, all_of(vars_personal_2))
        df <- rename(df, year = PB010)
        df <- rename(df, country = PB020)
        df <- rename(df, hhid = PX030)
        test <- rbind(test,df)
}

personal_2 <- test

df_personal <- bind_rows(personal_1,personal_2)
rm(personal_1,personal_2)

# Load household data -----------------------------------------

test <- data.frame()
year <- seq(2004,2019,1)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"hhld_data_",year[yr],".rds"))
        df <- mutate(df, HB010 = as.numeric(as.character(HB010)))
        df <- mutate(df, HB020 = as.factor(as.character(HB020)))
        df <- select(df, all_of(vars_household_1))
        df <- rename(df, year = HB010)
        df <- rename(df, country = HB020)
        df <- rename(df, hhid = HB030)
        test <- rbind(test,df)
}

df_household <- test


# Load household register data -----------------------------------------

test <- data.frame()
year <- seq(2004,2019,1)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"hhld_reg_data_",year[yr],".rds"))
        df <- mutate(df, DB010 = as.numeric(as.character(DB010)))
        df <- mutate(df, DB020 = as.factor(as.character(DB020)))
        df <- select(df, all_of(vars_household_reg_1))
        df <- rename(df, year = DB010)
        df <- rename(df, country = DB020)
        df <- rename(df, hhid = DB030)
        df <- rename(df, weight_hh = DB090)
        test <- rbind(test,df)
}

df_household_reg <- test

# Merge -----------------------------------------

df_eu_silc_xs <- merge(data.table(df_personal), data.table(df_household), by = c("year","country","hhid"))

df_eu_silc_xs <- merge(data.table(df_eu_silc_xs), data.table(df_household_reg), by = c("year","country","hhid"))

# Save -----------------------------------------

saveRDS(df_eu_silc_xs, file = paste0(project_data_files, "df_eu_silc_xs.rds"))
