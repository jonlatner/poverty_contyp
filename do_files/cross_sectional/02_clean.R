# Top commands -------------------------
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

data_files = "data_files/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(data.table)
library(Hmisc)
library(countrycode)
library(readxl)

# Load ----

df_eu_silc_0 <- readRDS(file = paste0(data_files, "df_eu_silc_xs.rds"))

# df_eu_silc_0 <- df_eu_silc_0 %>%
#         group_by(country, year) %>%
#         sample_frac(.1) %>%
#         ungroup()

# Clean ----

df_eu_silc_1 <- df_eu_silc_0 %>%
        mutate(unemp = ifelse(year < 2009 & PL030 == 3, yes = 1,
                            ifelse(year > 2008 & PL031 == "Unemployed", yes = 1, no = 0)),
               emp = ifelse(year < 2009 & PL030 < 3, yes = 1,
                            ifelse(year > 2008 & (PL031 == "Employee working full-time" | PL031 == "Employee working part-time"), yes = 1, 
                                   ifelse(unemp == 1, yes = 0, no = NA))),
               ftime = ifelse(year < 2009 & PL030 == 1, yes = 1, # full time
                              ifelse(year < 2009 & PL030 == 2, yes = 0, # part time
                                     ifelse(year > 2008 & PL031 == "Employee working full-time", yes = 1, 
                                            ifelse(year > 2008 & PL031 == "Employee working part-time", yes = 0, no = NA)))),
               lfp = ifelse(is.na(emp) == 1 | unemp == 1, yes = 1, no = 0),
               temp = ifelse(PL140==1, yes = 0,
                             ifelse(PL140 == 2, yes = 1, no = NA)),
               eq_hh_income = HX090,
               age = year - PB140,
               id = PB030,
               weight_xc = PB040) %>%
        select(-PL030,-PL031,-PL140,-HX090,-PB040,-PB140,-PB030) %>%
        filter(eq_hh_income>100)

df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(country = as.factor(country)) %>%
        filter(age >= 25 | age < 55)

levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="EL"] <- "GR"
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="UK"] <- "GB"

df_eu_silc_1$country_name <- countrycode(df_eu_silc_1$country, 'genc2c', 'country.name')
table(df_eu_silc_1$country_name,useNA = "ifany")

table(df_eu_silc_1$country,useNA = "ifany")


# Calculate poverty line (post tax, post-transfer, hh size adjusted) ----

# df_pov_line comes from Eurostat - Mean and median equivalized income by household type - EU-SILC and ECHP surveys (ilc_di04)
df_pov_line <- read_xlsx(paste0(support_files,"eurostat/median_income.xlsx"))

df_pov_line <- df_pov_line %>%
        rename(country_name=country) %>%
        arrange(country_name,year)

df_eu_silc_2 <- merge(data.table(df_eu_silc_1), data.table(df_pov_line)) %>%
        mutate(pov = ifelse(eq_hh_income<median_inc*.6, yes = 1, no = 0))

# Save ----

saveRDS(df_eu_silc_2, file = paste0(data_files, "df_eu_silc_xs_clean.rds"))

        