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

data_files = "data_files/"
tables = "tables/"

# LIBRARY
library(dplyr)
library(xtable)

options(scipen=999)


# Load data ----

df_filter <- readRDS(file = paste0(data_files,"df_eu_silc_filter_steps.rds"))

# Summarize data ----

df_filter <- df_filter %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  select(step,total)

df_filter

# Clean data ----

df_filter <- df_filter %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
       obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
       )
df_filter


df_filter <- df_filter %>%
  mutate(notes = ifelse(step == 0, yes = "Raw data",
                        ifelse(step == 1, yes = "Panel filters: Each panels is four years long",
                               ifelse(step == 2, yes = "Individual filters: Age (20 - 60), active labor market participation (employed or unemployed), personal weight $>$ 0",
                                      ifelse(step == 3, yes = "In first period: poor",
                                             ifelse(step == 4, yes = "In first period: poor + employed",
                                                    ifelse(step == 5, yes = "In first period: poor + employed + temp contract",
                                                           ifelse(step == 6, yes = "Spouse, if present, must have observable employment status",
                                                                  ifelse(step == 7, yes = "Present in first 2 years of a 4-year study period",
                                                                         ifelse(step == 8, yes = "Present in all 4 years of a 4-year study period",
                                                           no = NA))))))))))
df_filter

# Table -----------------------------------------

df_table <- df_filter %>%
  select(step,total,obs_diff,notes)


# VARIABLE LABLES

columns_header <- c("[-1.8ex]
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{l}{Unique observations} &
\\multicolumn{1}{l}{Percent change} & 
\\multicolumn{1}{l}{Notes} 
\\\\  \n 
")

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              "l", #unique obs
              "l", #diff. unique
              ">{\\raggedright\\arraybackslash}p{4in}" # notes
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = FALSE,
      add.to.row = list(
              pos = list(0,0,9),
              command = c(hline_top,
                          columns_header,
                          hline_bot)),
      comment = FALSE
)

t

