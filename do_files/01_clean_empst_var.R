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
library(data.table)
library(car) # recode
library(beepr)

# Load EU-SILC data ----

df_eu_silc_0 <- readRDS(file = paste0(data_files,"00_df_eu_silc.rds")) 

# step 1: Clean data ----

df_eu_silc_1 <- df_eu_silc_0

df_eu_silc_1$country <- as.factor(as.character(df_eu_silc_1$country))
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="EL"] <- "GR"

# Clean employment status
# PL030
# 0 - unemployed
# 1 - employed, full-time
# 2 - employed, part-time
# 3 - not in labour force (NILF)
df_eu_silc_1$empst_1_new <- recode(df_eu_silc_1$empst_1, "1=1; 2=2; 3=0; 4:hi=3; else = NA")  

# PL031
# 0 - unemployed
# 1 - employed, full-time (including self-employed)
# 2 - employed, part-time (including self-employed)
# 3 - NILF
df_eu_silc_1$empst_2_new <- recode(df_eu_silc_1$empst_2, "c(1,3)=1;c(2,4)=2; 5=0; 6:hi = 3;else = NA")

# Clean employment status
# Between 2009 and 2012, SILC data transitioned from one definition of employment status to another.
# Moving from PL030 to PL031.  The difference is that PL031 accounts for self-employed
# To maintain consistency, we include self-employed as employed, conditional being employed with an observable work contract
# Therefore, three distinct definitions of employment status: 1) panel == 2008; 2) 2008 < panel < 2013; 3) panel >= 2013
# It is crucial to use the indicator variable
# Recode 1 = employed FT; 2 = employed PT; 0 = unemployed; 3 = not in labor force (NILF) 

df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(empst_1_new = ifelse(empst_1_f==-5, yes = empst_2_new, # if old indicator says to do so, then replace old with new variable 
                                    ifelse(empst_1_f == 1, yes = empst_1_new, no = NA)), # else use old variable
               empst_2_new = ifelse(empst_2_f==-5, yes = empst_1_new, #  if new indicator says to do so, then replace new with old variable
                                    ifelse(empst_2_f == 1, yes = empst_2_new, no = NA)), # else use new variable
        )

# this should solve the problem, but it doesn't.  there are still some problems where match is not correct
with(subset(df_eu_silc_1,panel > 2008 & panel < 2013),table(empst_1_new,empst_2_new, useNA = "ifany"))
with(subset(df_eu_silc_1,panel > 2008 & panel < 2013),table(empst_1_f,empst_2_f, useNA = "ifany"))

# Issue 1: PL is the only country where empst_1_f == 0, which is not nice because there is no value label for this code in that or any year
with(subset(df_eu_silc_1,panel > 2008 & panel < 2013),table(country,empst_1_f, useNA = "ifany"))
with(subset(df_eu_silc_1,panel > 2008 & panel < 2013 & country == "PL"),table(year,empst_1_f, useNA = "ifany"))
# Issue 2: empst_1_f == -1 (i.e. missing) and empst_2_f == 1 (i.e. filled)
# Issue 3: empst_1_f == NA (i.e. missing) and empst_2_f == 1 (i.e. filled)
# Issue 4: empst_2_f == -1 (i.e. missing) and empst_1_f == 1 (i.e. filled)

# Therefore, replace missing values with filled values
df_eu_silc_2 <- df_eu_silc_1 %>%
        mutate(empst_1_new = ifelse(empst_1_f == 0 & empst_2_f == 1, yes = empst_2_new, no = empst_1_new),
               empst_1_new = ifelse(empst_1_f == -1 & empst_2_f == 1, yes = empst_2_new, no = empst_1_new),
               empst_1_new = ifelse(is.na(empst_1_f) & empst_2_f == 1, yes = empst_2_new, no = empst_1_new),
               empst_2_new = ifelse(empst_2_f == -1 & empst_1_f == 1, yes = empst_1_new, no = empst_2_new),
        )

# match is now correct
with(subset(df_eu_silc_2,panel > 2008 & panel < 2013),table(empst_1_new,empst_2_new, useNA = "ifany"))

# create single, continuous variable for employment status

df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(empst = ifelse(panel < 2009, yes = empst_1_new,
                              ifelse(panel > 2008, yes = empst_2_new, no = NA)))

# Save ----

saveRDS(df_eu_silc_2, file = paste0(data_files, "01_df_eu_silc_clean_empst.rds"))

beep()
