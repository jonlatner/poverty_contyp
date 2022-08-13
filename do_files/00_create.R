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

orig_data_files = "raw_data/EU_SILC_2019/Long_2005_19/data_files/"
project_data_files = "data_files/"

# LIBRARY
library(dplyr)
library(data.table)
library(beepr)

# Variables data ----

# Personal data
# PB010 year
# PB020 country
# PB030 pid
# PX030 household id
# PB050 personal base weight
# PB140 birthy
# PB150 sex
# PE040 isced
# PL030 empst
# PL031 empst
# PL040 
# PL140 contyp
# PY010G income
# PB180 partner_id
# PB190 marstat
# PL160 Change of job since last year

# Personal register
# RB010 year
# RB020 country
# RB030 pid
# RB040 household id
# RB060 weight_personal
# RB064 weight_long_4_yr_dur
# RB080 Year of birth
# RX010 age at date of interview

# Household data
# HB010 year of the survey
# HB020 country
# HB030 household ID
# HY010 TOTAL HOUSEHOLD GROSS INCOME
# HY020 TOTAL DISPOSABLE HOUSEHOLD INCOME
# HX090 Equivalised disposable income
# HX040 Household size

# Load personal data ----
year <- c(2007,2008)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL030, PL030_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)),
                       PL031 = NA,
                       PL031_F = NA)
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2009,2010,2011,2012)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL030, PL030_F, PL031, PL031_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL031, PL031_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)),
                       PL030 = NA,
                       PL030_F = NA)
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

# Append personal data ----

year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_eu_silc_pers = data.frame()
for (yr in seq_along(year)) {
        df <- get(paste0("df_pers_data_yr_", year[yr]))
        df_eu_silc_pers <- rbind(df_eu_silc_pers,df)
}

rm(list=ls(pattern="df_pers_data")) # remove

# Load register data ----

# Personal register
# RB010 year
# RB020 country
# RB030 pid
# RB040 household id
# RB060 weight_personal
# RB064 weight_long_4_yr_dur
# RB080 Year of birth
# RX010 age at date of interview

year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_reg_data_",year[yr],".rds"))
        df <- df %>%
                select(RB010, RB020, RB030, RB040, RB060, RB062, RB063, RB064, RB080, RX010) %>%
                rename(PB010 = RB010,
                       PB020 = RB020,
                       PB030 = RB030,
                       PX030 = RB040) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_reg_data_yr_",year[yr]), df)
}

year <- c(2007)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_reg_data_",year[yr],".rds"))
        df <- df %>%
                select(RB010, RB020, RB030, RB040, RB060, RB062, RB063, RB064, RB080, RX010) %>%
                rename(PB010 = RB010,
                       PB020 = RB020,
                       PB030 = RB030,
                       PX030 = RB040) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_reg_data_yr_",year[yr]), df)
}

# Append register data

df_eu_silc_pers_reg = data.frame()
year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
for (yr in seq_along(year)) {
        df <- get(paste0("df_pers_reg_data_yr_", year[yr]))
        df_eu_silc_pers_reg <- rbind(df_eu_silc_pers_reg,df)
}

rm(list=ls(pattern="df_pers_reg_data")) # remove

# Household children status ----

# Generate a dummy variable identifying children under 18***
# RB080 is birth year
# RX010 is age at interview
# We note that there are 2 age variables (RB080 and RX010, RB080 has fewer missings, therefore we use RB080)
df_eu_silc_pers_reg <- df_eu_silc_pers_reg %>%
        mutate(age = PB010 - RB080, # there are some cases where age<0, but RX010 suggests that half of these are child less than 1, rest not sure (i.e. RX010 is missing)
               child = ifelse(age<18, yes = 1, no = 0),
               ) %>%
        select(panel,PB010,PB020,PX030,PB030,everything()) %>% # panel, year, country, hid, pid
        arrange(panel,PB010,PB020,PX030,PB030) %>% # panel, year, country, hid, pid
        group_by(panel,PB010,PB020,PX030) %>% # panel, year, country, hid
        mutate(num_kids = sum(child)) %>%
        ungroup()

# Load household data ----

# Household data
# HB010 year of the survey
# HB020 country
# HB030 household ID
# HY010 TOTAL HOUSEHOLD GROSS INCOME
# HY020 TOTAL DISPOSABLE HOUSEHOLD INCOME
# HX090 Equivalised disposable income

year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"hhld_data_",year[yr],".rds"))
        df <- data.frame(df)
        df <- df %>%
                select(HB010, HB020, HB030, HY010, HY020, HX090, HX040) %>%
                rename(PB010 = HB010,
                       PB020 = HB020,
                       PX030 = HB030) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_hhid_data_yr_",year[yr]), df)
}

# Append household data
df_eu_silc_hhid = data.frame()
for (yr in seq_along(year)) {
        df <- get(paste0("df_hhid_data_yr_", year[yr]))
        df_eu_silc_hhid <- rbind(df_eu_silc_hhid,df)
}

rm(list=ls(pattern="df_hhid_data")) # remove

beep()

# Merge and clean data environment ----

df_eu_silc <- merge(data.table(df_eu_silc_pers_reg),data.table(df_eu_silc_pers),by = c("PB010","PB020","PB030","panel"))
df_eu_silc_2 <- merge(data.table(df_eu_silc),data.table(df_eu_silc_hhid),by = c("PB010","PB020","PX030","panel")) # merge household data to personal data

# rm(df,df_eu_silc_pers_reg,df_eu_silc_pers,df_eu_silc_hhid)

# Rename vars ----

df_eu_silc_2 <- df_eu_silc_2 %>%
        rename(year = PB010,
               country = PB020,
               pid = PB030,
               hid = PX030,
               hh_size=HX040,
               age_interview=RX010,
               weight_xc_base = PB050,
               weight_personal_base = RB060,
               weight_long_2 = RB062,
               weight_long_3 = RB063,
               weight_long_4 = RB064, 
               birthy = PB140,
               sex = PB150,
               job_change = PL160,
               isced = PE040,
               empst_1 = PL030,
               empst_1_f = PL030_F,
               empst_2 = PL031,
               empst_2_f = PL031_F,
               contyp = PL140,
               income = PY010G,
               partner_id = PB180,
               marstat = PB190,
               hid = PX030,
               pre_gov_inc_hh = HY010,
               pst_gov_inc_hh = HY020,
               eq_hh_income = HX090
        )

# Save ----

saveRDS(df_eu_silc_2, file = paste0(project_data_files, "00_df_eu_silc.rds"))

beep()
