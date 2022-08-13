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

setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/")

data_files = "data_files/"
results = "results/"

# LIBRARY
library(tidyverse)
library(broom) #tidy
library(plm)
library(countrycode)
library(beepr)

# Load data ----

df_eu_silc <- readRDS(paste0(data_files,"03_df_eu_silc_trt_sample_clean_4year.rds"))

# Clean data ----

df_pov_exit <- df_eu_silc
df_pov_exit <- droplevels(df_pov_exit)

df_filter <- df_pov_exit %>%
        group_by(region, country_name_region, id) %>%
        slice() %>%
        group_by(region, country_name_region) %>%
        tally() %>%
        ungroup() %>%
        mutate(n=n/4) %>%
        arrange(region, -n) %>%
        group_by(region) %>%
        mutate(filter = ifelse(n>50, yes = 1, no = 0)) %>%
        ungroup() %>%
        select(country_name_region,filter)

df_pov_exit <- merge(df_pov_exit,df_filter,all.x = TRUE,by = c("country_name_region"))

df_country_filter <- df_pov_exit %>%
        filter(filter==1) %>%
        select(country_name_region)
df_country_filter <- droplevels(df_country_filter)


df_pov_exit <- df_pov_exit %>%
        rename(empst_age=age) %>%
        group_by(country_name, id) %>%
        filter(row_number()<3) %>%
        ungroup() %>%
        rename(partner_4 = num_kids) %>% # this just makes labelling the variables easier in the graphs
        select(region,country_name_region,id,year,matches("exit_pov_"),matches("empst_"),matches("partner_"),matches("unmp"))

# Create interaction variables
df_pov_exit <- df_pov_exit %>%
        mutate(intunmp_empst_1 = macro_unmp_rate*empst_1,
               intunmp_empst_2 = macro_unmp_rate*empst_2,
               intunmp_empst_3 = macro_unmp_rate*empst_3
        )

df_pov_exit_eu <- df_pov_exit

df_pov_exit <- df_pov_exit_eu %>%
        filter(region != "Anglophone") %>%
        ungroup()
df_pov_exit <- droplevels(df_pov_exit)

df_pov_exit$year <- relevel(as.factor(df_pov_exit$year), ref = "2016")

# Independent variables ----

iv = "empst_1 + empst_3 + 
partner_1 + partner_3 + partner_4 + 
macro_unmp_rate + 
intunmp_empst_1 + intunmp_empst_3 +
year"

# Transitory exit ----

# Regression model
model <- plm(as.formula(paste0("exit_pov_t ~ ",iv)), index = c("id","year"), data = df_pov_exit_eu)

# create table
mfx <- tidy(model, conf.int = TRUE)
mfx$model <- 1
mfx$country <- "Europe"
mfx$region <- "Europe"

# assign output
# assign(paste("m1","EU",sep = "_"), model)

df_mfx_exit_t_eu <- mfx

# Regression model within each region
df_mfx_exit_t <- data.frame()
region <- unique(df_pov_exit$region)
for(c in region) {
        # filter
        df_country <- filter(df_pov_exit, region == c)
        df_country <- droplevels(df_country)
        
        # model
        model <- plm(as.formula(paste0("exit_pov_t ~ ",iv)), index = c("id","year"), data = df_country)

        # create table
        mfx <- tidy(model, conf.int = TRUE)
        mfx$model <- 1
        mfx$country <- c
        mfx$region <- unique(df_country$region)
        df_mfx_exit_t <- rbind(df_mfx_exit_t,mfx)
        
        # assign output
        # assign(paste("m1",c,sep = "_"), model)
}

df_mfx_exit_t_region <- df_mfx_exit_t

# Regression model within each country
df_mfx_exit_t <- data.frame()
country <- unique(df_country_filter$country_name_region)
for(c in country) {
        # filter
        df_country <- filter(df_pov_exit, country_name_region == c)
        df_country <- droplevels(df_country)
        
        # model
        model <- plm(as.formula(paste0("exit_pov_t ~ ",iv)), index = c("id","year"), data = df_country)

        # create table
        mfx <- tidy(model, conf.int = TRUE)
        mfx$model <- 1
        mfx$country <- c
        mfx$region <- unique(df_country$region)
        df_mfx_exit_t <- rbind(df_mfx_exit_t,mfx)
        
        # assign output
        # assign(paste("m1",c,sep = "_"), model)
}

df_mfx_exit_t_country <- df_mfx_exit_t
rm(df_mfx_exit_t)
df_mfx_exit_t <- rbind(df_mfx_exit_t_eu,df_mfx_exit_t_region,df_mfx_exit_t_country)
rm(df_mfx_exit_t_eu,df_mfx_exit_t_region,df_mfx_exit_t_country,model,mfx,df_country)

# Permanent exit ----

# Regression model
model <- plm(as.formula(paste0("exit_pov_p ~ ",iv)), index = c("id","year"), data = df_pov_exit_eu)

# create table
mfx <- tidy(model, conf.int = TRUE)
mfx$model <- 3
mfx$country <- "Europe"
mfx$region <- "Europe"

# assign output
# assign(paste("m3","EU",sep = "_"), model)

df_mfx_exit_p_eu <- mfx

# Regression model within each region 
df_mfx_exit_p <- data.frame()
region <- unique(df_pov_exit$region)
for(c in region) {
        # filter
        df_country <- filter(df_pov_exit, region == c)
        df_country <- droplevels(df_country)
        
        # model
        model <- plm(as.formula(paste0("exit_pov_p ~ ",iv)), index = c("id","year"), data = df_country)
        
        # create table
        mfx <- tidy(model, conf.int = TRUE)
        mfx$model <- 3
        mfx$country <- c
        mfx$region <- unique(df_country$region)
        df_mfx_exit_p <- rbind(df_mfx_exit_p,mfx)
        
        # assign output
        # assign(paste("m3",c,sep = "_"), model)
}

df_mfx_exit_p_region <- df_mfx_exit_p

# Regression model within each country 
df_mfx_exit_p <- data.frame()
country <- unique(df_country_filter$country_name_region)
for(c in country) {
        # filter
        df_country <- filter(df_pov_exit, country_name_region == c)
        df_country <- droplevels(df_country)
        
        # model
        model <- plm(as.formula(paste0("exit_pov_p ~ ",iv)), index = c("id","year"), data = df_country)

        # create table
        mfx <- tidy(model, conf.int = TRUE)
        mfx$model <- 3
        mfx$country <- c
        mfx$region <- unique(df_country$region)
        df_mfx_exit_p <- rbind(df_mfx_exit_p,mfx)
        
        # assign output
        # assign(paste("m3",c,sep = "_"), model)
}

df_mfx_exit_p_country <- df_mfx_exit_p
rm(df_mfx_exit_p)
df_mfx_exit_p <- rbind(df_mfx_exit_p_eu,df_mfx_exit_p_region,df_mfx_exit_p_country)
rm(df_mfx_exit_p_eu,df_mfx_exit_p_region,df_mfx_exit_p_country,model,mfx,df_country)

# Save ----

df_output <- rbind(df_mfx_exit_t,df_mfx_exit_p) %>%
        mutate(country_name_region = country)

rm(df_mfx_exit_t,df_mfx_exit_p)

saveRDS(df_output, file = paste0(results, "results_c_micro_macro_int.rds"))

beep()

