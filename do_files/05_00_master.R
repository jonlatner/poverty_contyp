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

source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/05_a_model_micro.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/05_b_model_micro_macro.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/05_c_model_micro_macro_int.R", echo = FALSE)
source("/Users/jonathanlatner/OneDrive/SECCOPA/projects/poverty_contyp/do_files/06_a_graph.R", echo = FALSE)

oldw <- getOption("warn")
options(warn = oldw) # turn on warnings

