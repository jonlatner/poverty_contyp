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

data_files = "data_files/"
tables = "tables/"

# LIBRARY
library(dplyr)
library(ggplot2)
library(stargazer)
library(reshape2)
library(forcats)
library(xtable)
library(countrycode)

options(scipen = 999) # disable scientific notation


# Load data --------------------------------------------------------------

df_eu_silc <- readRDS(paste0(data_files,"03_df_eu_silc_trt_sample_clean_4year.rds"))

df_pov_exit <- df_eu_silc %>%
  group_by(country_name, id) %>%
  filter(row_number()<3) %>%
  ungroup()

df_pov_exit <- droplevels(df_pov_exit)

# Prepare table --------------------------------------------------------------

df_pov_exit <- df_pov_exit %>%
  select(region, country_name, id, exit_pov_t, exit_pov_p, empst_1, empst_2, empst_3, partner_1, partner_2, partner_3, num_kids, macro_unmp_rate)

# Table across all region/country --------------------------------------------------------------

df_test <- df_pov_exit %>%
  group_by(country_name, id) %>%
  filter(row_number()==2) %>%
  ungroup()
t <- with(df_test,table(exit_pov_t,exit_pov_p))
prop.table(t,1)

df_europe <- df_pov_exit %>%
  select(-country_name,-region) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(list(mean =~ mean(.,na.rm = TRUE))) %>%
  mutate(geography = "Europe")

df_count <- df_pov_exit %>%
  select(-country_name,-region) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  tally() %>%
  mutate(geography = "Europe")

df_europe <- merge(df_europe,df_count)
rm(df_count)


# Table within region --------------------------------------------------------------

df_region <- df_pov_exit %>%
  select(-country_name) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(region) %>%
  summarise_all(list(mean =~ mean(.,na.rm = TRUE))) %>%
  ungroup() %>%
  rename(geography = region)

df_count <- df_pov_exit %>%
  select(-country_name) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(region) %>%
  tally() %>%
  ungroup() %>%
  rename(geography = region)

df_region <- merge(df_region,df_count)
rm(df_count)


# Table within countries --------------------------------------------------------------

df_country <- df_pov_exit %>%
  select(-region) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(country_name) %>%
  summarise_all(list(mean =~ mean(.,na.rm = TRUE))) %>%
  ungroup() %>%
  rename(geography = country_name)

df_count <- df_pov_exit %>%
  select(-region) %>%
  group_by(id) %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(country_name) %>%
  tally() %>%
  ungroup() %>%
  rename(geography = country_name)

df_country <- merge(df_country,df_count)
rm(df_count)

# Combine --------------------------------------------------------------

df_table <- rbind(df_europe,df_region,df_country)
df_table <- df_table %>%
  select(geography,n,everything())

df_table$geography <- factor(df_table$geography,
                        levels = c(
                          "Europe", 
                          "Anglophone", "Ireland", "United Kingdom",
                          "Continental", "Austria", "Belgium", "France", "Germany", "Luxembourg", "Netherlands", "Switzerland", 
                          "Eastern", "Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Serbia", "Slovakia", "Slovenia", 
                          "Nordic", "Denmark", "Finland", "Iceland", "Norway", "Sweden",
                          "Southern", "Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain"
                          ))

df_table <- df_table %>%
  select(geography,n,
         exit_pov_t_mean,exit_pov_p_mean,
         empst_1_mean,empst_2_mean,empst_3_mean,
         partner_1_mean,partner_2_mean,partner_3_mean,
         num_kids_mean,
         macro_unmp_rate_mean) %>%
  arrange(geography) 

# Descriptive table --------------------------------------------------------------

table <- df_table
table[1] <- NULL
# there is no serbia

# VARIABLE LABLES
rownames(table) <- c(
  "\\\\[-1.8ex]
\\hspace{5mm} Europe",
  "\\multicolumn{10}{l}{\\phantom{empty}} \\\\
        \\hspace{5mm} Anglophone",
  "\\hspace{10mm} Ireland",
  "\\hspace{10mm} United Kingdom",

  "\\multicolumn{10}{l}{\\phantom{empty}} \\\\
        \\hspace{5mm} Continental",
  "\\hspace{10mm} Austria",
  "\\hspace{10mm} Belgium",
  "\\hspace{10mm} France",
  "\\hspace{10mm} Germany",
  "\\hspace{10mm} Luxembourg",
  "\\hspace{10mm} Netherlands",
  "\\hspace{10mm} Switzerland",

  "\\multicolumn{10}{l}{\\phantom{empty}} \\\\
        \\hspace{5mm} Eastern",
  "\\hspace{10mm} Bulgaria",
  "\\hspace{10mm} Croatia",
  "\\hspace{10mm} Czechia",
  "\\hspace{10mm} Estonia",
  "\\hspace{10mm} Hungary",
  "\\hspace{10mm} Latvia",
  "\\hspace{10mm} Lithuania",
  "\\hspace{10mm} Poland",
  "\\hspace{10mm} Romania",
  "\\hspace{10mm} Serbia",
  "\\hspace{10mm} Slovakia",
  "\\hspace{10mm} Slovenia",
  
  "\\multicolumn{10}{l}{\\phantom{empty}} \\\\
        \\hspace{5mm} Nordic",
  "\\hspace{10mm} Demark",
  "\\hspace{10mm} Finland",
  "\\hspace{10mm} Iceland",
  "\\hspace{10mm} Norway",
  "\\hspace{10mm} Sweden",
  
  "\\multicolumn{10}{l}{\\phantom{empty}} \\\\
        \\hspace{5mm} Southern",
  "\\hspace{10mm} Cyprus",
  "\\hspace{10mm} Greence",
  "\\hspace{10mm} Italy",
  "\\hspace{10mm} Malta",
  "\\hspace{10mm} Portugal",
  "\\hspace{10mm} Spain"
)

# Descriptive Statistics: Means

header <- c("Country & 
                         n & 
                         Short-term & Long-term & 
                         Unemployed & 
                         Temporary & 
                         Permanent & 
                         Single & Non-employed & Employed & Children &
                         Unemployment rate \\\\ \n")
top_header <- c("[-1.8ex] &  & \\multicolumn{2}{l}{Poverty exit} & 
                          \\multicolumn{3}{l}{Employment characteristics of head} & 
                          \\multicolumn{3}{l}{Employment characteristics of partner} & 
                          \\multicolumn{1}{l}{Household} &
                          \\multicolumn{1}{l}{Macro-level characteristics} \\\\ \n")

hline_top <- c("\\\\[-1.8ex]\\hline\\hline \\\\ \n")

hline_bot <- c("\\hline\\hline \\\\[-1.8ex] \n") 

midrule <- c("
            \\cmidrule(lr){3-4} 
            \\cmidrule(lr){5-7}
            \\cmidrule(lr){8-10}
            \\cmidrule(lr){11-11}
            \\cmidrule(lr){12-12}
             ")

notes <- paste0("\\emph{Notes:} &", 
                "\\multicolumn{8}{l}{Average across individuals in last observation in panel period.}",
                "\\\\ \n" )

t <- xtable(table, digits = 3)
align(t) <- c("l", 
              "r", "r",
              "r", "r", "r",
              "r", "r", "r",
              "r", "r", "r")

print(t, 
      file = paste0(tables,"descriptives_table.tex"),
      include.rownames = TRUE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      hline.after = FALSE,
      add.to.row = list(
        pos = list(0,0,0,0,38,38),
        command = c(hline_top,
                    top_header,
                    midrule,
                    header,
                    hline_bot,
                    notes)),
      comment = FALSE
)


