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


oldw <- getOption("warn")
options(warn = -1) # turn off warnings

source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/age_older.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/age_younger.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/edu_higher.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/edu_lower.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/gender_female.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/gender_male.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/sensitivity/part_time_full_time.R", echo = FALSE)


