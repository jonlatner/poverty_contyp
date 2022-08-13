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
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(data.table)
library(Hmisc) #wtd.mean
library(forcats)
library(countrycode)

## The function to get overlapping strip labels
OverlappingStripLabels = function(plot) {
        
        # Get the ggplot grob
        pg = ggplotGrob(plot)
        
        ### Collect some information about the strips from the plot
        # Get a list of strips
        stripr = lapply(grep("strip-r", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        stript = lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        # Number of strips
        NumberOfStripsr = sum(grepl(pattern = "strip-r", pg$layout$name))
        NumberOfStripst = sum(grepl(pattern = "strip-t", pg$layout$name))
        
        # Number of columns
        NumberOfCols = length(stripr[[1]])
        NumberOfRows = length(stript[[1]])
        
        # Panel spacing
        plot_theme <- function(p) {
                plyr::defaults(p$theme, theme_get())
        }
        PanelSpacing = plot_theme(plot)$panel.spacing
        
        # Map the boundaries of the new strips
        Nlabelr = vector("list", NumberOfCols)
        mapr = vector("list", NumberOfCols)
        for(i in 1:NumberOfCols) {
                
                for(j in 1:NumberOfStripsr) {
                        Nlabelr[[i]][j] = getGrob(grid.force(stripr[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapr[[i]][1] = TRUE
                for(j in 2:NumberOfStripsr) {
                        mapr[[i]][j] = as.character(Nlabelr[[i]][j]) != as.character(Nlabelr[[i]][j-1])#Nlabelr[[i]][j] != Nlabelr[[i]][j-1]
                }
        }
        
        # Map the boundaries of the new strips
        Nlabelt = vector("list", NumberOfRows)
        mapt = vector("list", NumberOfRows)
        for(i in 1:NumberOfRows) {
                
                for(j in 1:NumberOfStripst) {
                        Nlabelt[[i]][j] = getGrob(grid.force(stript[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapt[[i]][1] = TRUE
                for(j in 2:NumberOfStripst) {
                        mapt[[i]][j] = as.character(Nlabelt[[i]][j]) != as.character(Nlabelt[[i]][j-1])#Nlabelt[[i]][j] != Nlabelt[[i]][j-1]
                }
        }
        
        
        ## Construct gtable to contain the new strip
        newStripr  = gtable(heights = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripsr-1), unit(1, "null")), 
                            widths = stripr[[1]]$widths)
        ## Populate the gtable  
        seqTop = list()
        for(i in NumberOfCols:1) {  
                Top = which(mapr[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfCols) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripsr-1))
                newStripr = gtable_add_grob(newStripr, lapply(stripr[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
        }
        
        mapt <- mapt[NumberOfRows:1]
        Nlabelt <- Nlabelt[NumberOfRows:1]
        ## Do the same for top facets
        newStript  = gtable(heights = stript[[1]]$heights,
                            widths = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripst-1), unit(1, "null")))
        seqTop = list()
        for(i in NumberOfRows:1) {  
                Top = which(mapt[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfRows) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripst-1))
                # newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
                newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[(NumberOfRows:1)[i]]]), t = (NumberOfRows:1)[i], l = seqTop[[i]], r = seqBottom)
        }
        
        ## Put the strip into the plot
        # Get the locations of the original strips
        posr = subset(pg$layout, grepl("strip-r", pg$layout$name), t:r)
        post = subset(pg$layout, grepl("strip-t", pg$layout$name), t:r)
        
        ## Use these to position the new strip
        pgNew = gtable_add_grob(pg, newStripr, t = min(posr$t), l = unique(posr$l), b = max(posr$b))
        pgNew = gtable_add_grob(pgNew, newStript, l = min(post$l), r = max(post$r), t=unique(post$t))
        grid.draw(pgNew)
        
        return(pgNew)
}

# Load -----------------------------------------

df_eu_silc <- readRDS(file = paste0(data_files, "df_eu_silc_xs_clean.rds"))
df_eu_silc <- data.frame(df_eu_silc)

df_eu_silc <- droplevels(df_eu_silc)

country <- c("Europe",
             "Anglophone", "Continental", "Eastern", "Nordic", "Southern",
             "DE", "CH", "LU", "BE", "AT", "NL", "FR", 
             "GB", "IE", 
             "MT", "GR", "IT", "CY", "PT", "ES", "RO", "PL", 
             "HR", "HU", "CZ", "BG", "SI", "SK", "LT", "EE", "LV", "RS", 
             "SE", "DK", "NO", "FI", "IS")
region <- c("Europe",
            "Anglophone", "Continental", "Eastern", "Nordic", "Southern",
            "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", 
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country, region)
rm(country)

df_eu_silc <- merge(data.table(df_eu_silc),data.table(geography), by = c("country"), all.x = TRUE)
summary(df_eu_silc)
        
# Poverty, by contract type -----------------------------------------

df_graph_countries <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(region, country,year, temp) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(region, year,temp) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country = region)

df_graph_eu <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(year,temp) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(country = "Europe", region = "Europe", level = "Region")

df_pov_contyp <- rbind(df_graph_eu,df_graph_countries,df_graph_regions)
rm(df_graph_countries,df_graph_regions,df_graph_eu)

df_pov_contyp$temp <- factor(df_pov_contyp$temp,
                        levels = c("0", "1"),
                        labels = c("Permanent", "Temporary"))
table(df_pov_contyp$temp)

# Poverty, in-work -----------------------------------------

df_graph_countries <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(region, country,year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country = region)

df_graph_eu <- df_eu_silc %>%
        filter(!is.na(temp)) %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(country = "Europe", region = "Europe", level = "Region")

df_pov_inwork <- rbind(df_graph_eu,df_graph_countries,df_graph_regions)
rm(df_graph_countries,df_graph_regions,df_graph_eu)

df_pov_inwork$temp <- "In-work"

table(df_pov_inwork$temp)

# Poverty, percent -----------------------------------------

df_graph_countries <- df_eu_silc %>%
        group_by(region, country,year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_silc %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country = region)

df_graph_eu <- df_eu_silc %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(pov,weight_xc)) %>%
        ungroup() %>%
        mutate(country = "Europe", region = "Europe", level = "Region")

df_pov <- rbind(df_graph_eu,df_graph_countries,df_graph_regions)
rm(df_graph_countries,df_graph_regions,df_graph_eu)

df_pov$temp <- "Poverty"

# Graph -----------------------------------------

df_graph <- rbind(df_pov,df_pov_contyp,df_pov_inwork)
df_graph$avg <- df_graph$avg*100

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$temp <- factor(df_graph$temp,
                        levels = c("Poverty", "In-work", "Permanent", "Temporary"),
                        labels = c("Poverty", "In-work", "Permanent", "Temporary"))

df_graph_data <- df_graph %>%
        select(country,year,avg,temp) %>%
        arrange(temp,country,year)

df_graph_data <- unique(df_graph_data)

df_graph$country <- fct_relevel(df_graph$country, "Southern", after = Inf) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Nordic", after = Inf) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Eastern", after = Inf) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Continental", after = Inf) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Anglophone", after = Inf) # forcats
df_graph$country <- fct_relevel(df_graph$country, "Europe", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats

df_graph$label_region <- "Country clusters with similar welfare state regime"

df_graph_region <- df_graph %>%
        filter(level != "Countries")

library(grid)
library(gtable)
library(plyr)


p <- ggplot(df_graph, aes(x = year, y = avg, group = country, color = level, size = level)) +
        facet_grid(temp ~ label_region + region) +
        scale_size_manual(values = c(1,.5)) +
        scale_color_manual(values = c("black", "gray")) +
        geom_line() +
        # scale_y_continuous(breaks = c(seq(0, .4, by = .1)), limits = c(0, .45)) +
        scale_x_continuous(breaks = c(2004, 2011, 2018), limits = c(2002, 2021)) +
        ylab("100x Poverty rate (ages 25-54)") +
        xlab("Year") + 
        geom_text(data = df_graph_region,show.legend = FALSE,
                  size = 3, 
                  aes(x = year, y = ifelse(year %in% c(2004,2011,2019), yes = avg, no = NA),
                      vjust=-2,
                      label=sprintf(avg, fmt = '%#.1f'))) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(angle = 90),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"inwork_pov_rate_contyp.pdf"), height = 9.5, width = 7)
grid.draw(OverlappingStripLabels(p))
dev.off()
