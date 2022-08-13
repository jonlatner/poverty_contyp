# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
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

data_files = "support_files/"

# LIBRARY
library(tidyverse)
library(readxl)
library(plm)
library(texreg)

options(scipen = 9999) # disable scientific notation

# load data -----

df_pov_exit <- read_xlsx(paste0(data_files, "simulation.xlsx"), 
                         sheet = paste0("single_panel"))
 
df_pov_exit

# Independent variables --------------------------------------------------------------


iv_1 = "perm"
iv_2 = "perm + year"

# Transitory exit ----

model_1 <- plm(as.formula(paste0("exit_pov_t ~ ",iv_1)), index = c("id","year"),
                data = df_pov_exit)

model_2 <- plm(as.formula(paste0("exit_pov_t ~ ",iv_2)), index = c("id","year"),
               data = df_pov_exit)

screenreg(list(model_1, model_2),
          # include.groups=TRUE,
          # omit.coef = ("year"),
)

t <- with(subset(df_pov_exit,year==2),table(exit_pov_t))
prop.table(t)

t <- with(df_pov_exit,table(exit_pov_t,perm))
prop.table(t,1)
