# Top commands -----------------------------------------
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
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

support_files = "SECCOPA/projects/poverty_contyp/support_files/"
data_files = "SECCOPA/projects/poverty_contyp/data_files/"
graphs = "SECCOPA/projects/poverty_contyp/graphs/"

# LIBRARY
library(tidyverse)
library(countrycode)
library(readxl)
library(writexl)

# Load poverty data -----------------------------------------

df_pov_line <- read_xls(paste0(support_files,"eurostat/ilc_di04.xls"),sheet = "data", col_types = "text")

# df_pov_line comes from Eurostat - Mean and median equivalized income by household type - EU-SILC and ECHP surveys (ilc_di04)
df_pov_line <- read_xls(paste0(support_files,"eurostat/ilc_di04.xls"),sheet = "data", col_types = "text")
df_pov_line <- df_pov_line %>%
        rename(country = "GEO/TIME")

df_pov_line <- pivot_longer(df_pov_line, !country, names_to = "year", values_to = "median_inc")
df_pov_line <- df_pov_line %>%
        mutate(year = as.numeric(as.character(year)),
               median_inc = as.numeric(as.character(median_inc)),
               )

write_xlsx(df_pov_line, paste0(support_files,"eurostat/median_income.xlsx"))
