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
results = "results/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(stringr) #str_detect
library(forcats) #fct_rev
library(ggh4x) # facet_nested package replaces function

options(scipen = 999) # disable scientific notation

# Load data ----

df_micro <- readRDS(paste0(results,"results_a_micro.rds"))
df_macro <- readRDS(paste0(results,"results_b_micro_macro.rds"))
df_int <- readRDS(paste0(results,"results_c_micro_macro_int.rds"))

# Clean data ----

df_micro$type <- "micro"
df_macro$type <- "macro"
df_int$type <- "int"
        
df_results <- rbind(df_micro,df_macro,df_int)
rm(df_micro,df_macro,df_int)
# df_results <- df_results %>%
#         mutate(region = ifelse(is.na(region), yes = "A", no = region))
df_results$country <- fct_reorder(df_results$country, as.numeric(as.factor(df_results$region)), .desc = FALSE) # forcats

df_results$country <- fct_relevel(df_results$country, "Southern", after = 0) # forcats
df_results$country <- fct_relevel(df_results$country, "Nordic", after = 0) # forcats
df_results$country <- fct_relevel(df_results$country, "Eastern", after = 0) # forcats
df_results$country <- fct_relevel(df_results$country, "Continental", after = 0) # forcats
df_results$country <- fct_relevel(df_results$country, "Anglophone", after = 0) # forcats
df_results$country <- fct_relevel(df_results$country, "Europe", after = 0) # forcats

# Prepare graph labels ----

df_results <- df_results %>%
        filter(!str_detect(term, 'year')) %>%
        filter(!str_detect(term, '(Intercept)')) %>%
        mutate(sig = ifelse(p.value<.05, yes = "Yes", no = "No"))

df_results$group <- gsub( "_.*$", "", df_results$term)
df_results$group <- factor(df_results$group,
                         levels = c("intunmp", "macro", "partner", "empst"),
                         labels = c("Interaction w/ Unmp rate","Macro-level", "Household", "Head"))

# reference is empst_3 (employed, temp + ftime) & partner_3 (married, with employed spouse - temp)
df_results$term <- factor(df_results$term,
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
                                "Permanent", "Unemployed"
                        ))

table(df_results$term)

df_results$model <- factor(as.factor(df_results$model),
                         levels = c("1", "3"),
                         labels = c("Short-term exit", "Long-term exit"))


df_results$group <- fct_rev(df_results$group)
df_results$country <- fct_rev(df_results$country)

# Graph interaction ----

df_graph <- df_results %>%
        filter(type == "int")

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


# graph
p <- ggplot(df_graph, aes(x = country, y = estimate, color = sig, shape = sig)) +
        coord_flip() +
        facet_nested(model  ~ group + term, scales = "free") +
        geom_point(size = 2) +
        scale_color_manual(values=c("black", "black")) +
        scale_shape_manual(values=c(1, 16)) +
        # labs(color  = "95% significant", shape = "95% significant") +
        geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.2) +
        geom_hline(yintercept = 0) +
        theme_bw() +
        # scale_y_continuous(breaks = seq(-4, 4, by = 2), limits = c(-5.4,5.4)) +
        # scale_y_continuous(breaks = seq(-10, 10, by = 5), limits = c(-10,10)) +
        geom_text(aes(label=ifelse(p.value<.05,sprintf(estimate, fmt = '%#.3f'),"")),
                  hjust=ifelse(df_graph$estimate<0, yes = +1.25, no = -.25),
                  show.legend = FALSE,
                  size=3) +
        ylab("Average marginal effect") +
        geom_blank(data=dummy) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "none",
              axis.text = element_text(size = 9),
              strip.text = element_text(size = 9),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot = p, filename = paste0(graphs,"graph_c_micro_macro_int.pdf"), height = 9, width = 10.5)

# Graph macro ----

df_graph <- df_results %>%
        filter(type == "macro")

dummy <- df_graph %>%
        select(country,country_name_region,estimate,sig,model,group,term) %>%
        filter(country_name_region == "Poland (E)" | country_name_region == "France (C)") %>%
        mutate(estimate=ifelse(group == "Head" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Head" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Household" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Household" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Macro-level" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Macro-level" & country_name_region == "France (C)", yes = 2, no = estimate)))

# graph
p <- ggplot(df_graph, aes(x = country, y = estimate, color = sig, shape = sig)) +
        coord_flip() +
        facet_nested(model  ~ group + term, scales = "free") +
        geom_point(size = 2) +
        scale_color_manual(values=c("black", "black")) +
        scale_shape_manual(values=c(1, 16)) +
        # labs(color  = "95% significant", shape = "95% significant") +
        geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.2) +
        geom_hline(yintercept = 0) +
        theme_bw() +
        # scale_y_continuous(breaks = seq(-4, 4, by = 2), limits = c(-5.4,5.4)) +
        # scale_y_continuous(breaks = seq(-10, 10, by = 5), limits = c(-10,10)) +
        geom_text(aes(label=ifelse(p.value<.05,sprintf(estimate, fmt = '%#.3f'),"")),
                  hjust=ifelse(df_graph$estimate<0, yes = +1.5, no = -.5),
                  show.legend = FALSE,
                  size=3) +
        ylab("Average marginal effect") +
        geom_blank(data=dummy) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "none",
              axis.text = element_text(size = 9),
              strip.text = element_text(size = 9),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot = p, filename = paste0(graphs,"graph_b_micro_macro.pdf"), height = 9, width = 10.5)

# Graph micro ----

df_graph <- df_results %>%
        filter(type == "micro")
df_graph <- droplevels(df_graph)

table(df_graph$term)

dummy <- df_graph %>%
        select(country,country_name_region,estimate,sig,model,group,term)%>%
        filter(country_name_region == "Poland (E)" | country_name_region == "France (C)") %>%
        mutate(estimate=ifelse(group == "Head" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Head" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Household" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Household" & country_name_region == "France (C)", yes = 2, no = estimate))) %>%
        mutate(estimate=ifelse(group == "Macro-level" & country_name_region == "Poland (E)", yes = -2,
                               ifelse(group == "Macro-level" & country_name_region == "France (C)", yes = 2, no = estimate)))

# graph
p <- ggplot(df_graph, aes(x = country, y = estimate, color = sig, shape = sig)) +
        coord_flip() +
        facet_nested(model  ~ group + term, scales = "free") +
        geom_point(size = 2) +
        scale_color_manual(values=c("black", "black")) +
        scale_shape_manual(values=c(1, 16)) +
        # labs(color  = "95% significant", shape = "95% significant") +
        geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.2) +
        geom_hline(yintercept = 0) +
        theme_bw() +
        # scale_y_continuous(breaks = seq(-4, 4, by = 2), limits = c(-5.4,5.4)) +
        # scale_y_continuous(breaks = seq(-10, 10, by = 5), limits = c(-10,10)) +
        geom_text(aes(label=ifelse(p.value<.05,sprintf(estimate, fmt = '%#.3f'),"")),
                  hjust=ifelse(df_graph$estimate<0, yes = +1.5, no = -1),
                  show.legend = FALSE,
                  size=3) +
        ylab("Average marginal effect") +
        geom_blank(data=dummy) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "none",
              axis.text = element_text(size = 9),
              strip.text = element_text(size = 9),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot = p, filename = paste0(graphs,"graph_a_micro.pdf"), height = 9, width = 10.5)
