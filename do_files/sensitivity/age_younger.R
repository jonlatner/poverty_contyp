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
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(stringr) #str_detect
library(forcats) #fct_rev
library(ggh4x) # facet_nested
library(broom) #tidy
library(plm)

options(scipen = 999) # disable scientific notation

# Load data ----

df_eu_silc <- readRDS(paste0(data_files,"03_df_eu_silc_trt_sample_clean_4year.rds"))

# Clean data ----

df_pov_exit <- df_eu_silc %>%
        filter(age < 35)
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
saveRDS(df_output, file = paste0(results, "results_sensitivity_micro_macro_int_male.rds"))

# Main effects graph ----

df_graph <- df_output

df_graph$country <- fct_reorder(df_graph$country, as.numeric(as.factor(df_graph$region)), .desc = FALSE) # forcats

df_graph$country <- fct_relevel(df_graph$country, "Southern", after = 0) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Nordic", after = 0) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Eastern", after = 0) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Continental", after = 0) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Anglophone", after = 0) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Europe", after = 0) # forcats

table(df_graph$country)

df_graph <- df_graph %>%
        filter(!str_detect(term, 'year')) %>%
        filter(!str_detect(term, '(Intercept)')) %>%
        mutate(sig = ifelse(p.value<.05, yes = "Yes", no = "No"))

df_graph$group <- gsub( "_.*$", "", df_graph$term)
df_graph$group <- factor(df_graph$group,
                         levels = c("intunmp", "macro", "partner", "empst"),
                         labels = c("Interaction w/ Unmp rate","Macro-level",  "Household", "Head"))

# reference is empst_3 (employed, temp + ftime) & partner_3 (married, with employed spouse - temp)
df_graph$term <- factor(df_graph$term,
                        levels = c(
                                "macro_unmp_rate",
                                "intunmp_empst_1", "intunmp_empst_3",  
                                "partner_1", "partner_3", "partner_4",
                                "empst_3", "empst_1"
                        ),
                        labels = c(
                                "Unmp rate (%)",
                                "Unemployed", "Permanent", 
                                "Single", "Employed spouse", "Children", 
                                "Permanent contract", "Unemployed"
                        ))

df_graph$model <- factor(as.factor(df_graph$model),
                         levels = c("1", "3"),
                         labels = c("Short-term exit", "Long-term exit"))

table(df_graph$term)

# df_graph$term <- fct_rev(df_graph$term)
df_graph$group <- fct_rev(df_graph$group)
df_graph$country <- fct_rev(df_graph$country)

table(df_graph$term)

# graph
library(grid)
library(gtable)
library(plyr)

dummy <- df_graph %>%
        select(country,country_name_region,estimate,sig,model,group,term) %>%
        filter(country_name_region == "Poland (E)" | country_name_region == "France (C)") %>%
        mutate(estimate=ifelse(group == "Head" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Head" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Interaction w/ Unmp rate" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Interaction w/ Unmp rate" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Household" & country_name_region == "Poland (E)", yes = -4,
                               ifelse(group == "Household" & country_name_region == "France (C)", yes = 4, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Macro-level" & country_name_region == "Poland (E)", yes = -5,
                               ifelse(group == "Macro-level" & country_name_region == "France (C)", yes = 5, no = estimate)))

p <- ggplot(df_graph, aes(x = country, y = estimate, color = sig, shape = sig)) +
        coord_flip() +
        facet_nested(model  ~ group + term) +
        geom_point(size = 2) +
        scale_color_manual(values=c("black", "black")) +
        scale_shape_manual(values=c(1, 16)) +
        labs(color  = "95% significant", shape = "95% significant") +
        geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.2) +
        geom_hline(yintercept = 0) +
        theme_bw() +
        # scale_y_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5,5)) +
        geom_text(aes(label=ifelse(p.value<.05,sprintf(estimate, fmt = '%#.3f'),"")),
                  hjust=ifelse(df_graph$estimate<0, yes = +1.25, no = -.25),
                  show.legend = FALSE,
                  size=3) +
        ylab("Average marginal effect") +
        # geom_blank(data=dummy) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "none",
              axis.text = element_text(size = 9),
              strip.text = element_text(size = 9),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot = p, filename = paste0(graphs,"graph_sensitivity_micro_macro_int_younger.pdf"), height = 9, width = 10.5)
