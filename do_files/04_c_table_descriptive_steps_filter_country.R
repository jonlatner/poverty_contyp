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
# setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "data_files/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(countrycode)

options(scipen=999)


# Load data ----

df_filter <- readRDS(file = paste0(data_files,"df_eu_silc_filter_steps.rds"))

# Country code data ----

df_filter$country_name <- countrycode(df_filter$country, 'genc2c', 'country.name')
df_filter <- df_filter %>%
  mutate(country_name=ifelse(country == "UK", yes = "United Kingdom", no = country_name))

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

df_filter <- merge(df_filter,geography)
rm(country_name, region, geography)

# Summarize data ----

df_filter <- df_filter %>%
  group_by(country,panel,step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(country_name,panel,step) %>%
  select(region,country_name,panel,step,total)

df_filter %>% filter(country_name=="Austria")

# Reshape data ----

df_filter_wide <- df_filter %>%
  pivot_wider(names_from = panel, values_from = total)

df_filter_wide$total <- rowSums(df_filter_wide[,4:16], na.rm = TRUE)

# Clean data ----

df_filter_wide <- df_filter_wide %>%
  group_by(country_name) %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
  ) %>%
  ungroup()
df_filter_wide

# Table -----------------------------------------

df_table <- df_filter_wide
df_table <- df_table %>%
  arrange(region,country_name,step) %>%
  group_by(country_name) %>%
  mutate(region = ifelse(row_number()>1, yes = "", no = region),
         country_name = ifelse(row_number()>1, yes = "", no = country_name),
  ) %>%
  ungroup()
df_table

# VARIABLE LABLES

header_first <- c("
\\label{descriptives_table_steps_country_panel}
\\\\ \\hline \\\\ 
 [-1.8ex]
\\rowcolor{white} Region & 
Country & 
Step & 
\\multicolumn{13}{l}{Unique observations by panel wave} &
Total & 
\\% change 
\\\\  \n 
\\cmidrule(lr){4-16}   \n 
\\rowcolor{white} & & & 2007 & 2008 & 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & 2019 & &
\\\\ 
\\hline
\\endfirsthead
\n"
)


header_repeat <- c("
\\rowcolor{white}\\multicolumn{6}{l}{\\ldots Table \\ref{descriptives_table_steps_country_panel} continued} \\\\
\\hline
\\rowcolor{white} Region & 
Country & 
Step & 
\\multicolumn{13}{l}{Unique observations by panel wave} &
Total & 
\\% change 
\\\\  \n 
\\cmidrule(lr){4-16}   \n 
\\rowcolor{white} & & & 2007 & 2008 & 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & 2019 & &
\\\\ 
\\hline
\\endhead % all the lines above this will be repeated on every page
\\hline
\\rowcolor{white}\\multicolumn{18}{r@{}}{Table \\ref{descriptives_table_steps_country_panel} continued \\ldots}\\\\
\\endfoot
\\hline
\\endlastfoot
\n
")

t <- xtable(df_table, 
            digits = 0, 
            caption = "Sample selection detailed by country, panel wave")

align(t) <- c("l", #first
              "l", "l", "l", "l", "l", "l", 
              "l", "l", "l", "l", "l", "l", 
              "l", "l", "l", "l", "l", "l"
) 

print(t, 
      tabular.environment = "longtable", 
      caption.placement = "top",
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country_panel.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      hline.after = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row = list(
        pos = list(0,0),
        command = c(header_first,
                    header_repeat
                    )),
      comment = FALSE
)
