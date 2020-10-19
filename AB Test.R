library(tidyverse)
library(readr)
library(forcats)

# A library page has a three main buttons available to customers, 'FIND', 'REQUEST', and 'INTERACT'. There's a problem where interact is only used by customers 2% of the time. To identify a better alternative to interact, an AB test is done with four different variants: Connect, Learn, Help, and Services. 


# -- Data Load -------

setwd("~/Kaggle Competitions/Automating Boring Stuff/AB Test")


# -- EDA (Exploratory Data Analysis) --------
# Three Questions to solve
  # 0. create a df with summarizing all variants together
  # 1. How does each variant compare to 'Interact' in terms of clicks --> boxplot 
  # 2. Relative rank amongst its own dataset 
  # 3. Turn clicks to click through rate --> need total clicks

# summary = df_interact %>%
#   filter(grepl("^interact", Name, ignore.case =  TRUE)) %>%
#   select(`No. clicks`) %>%
#   mutate(Name = "Interact") 
  
v_names = c('INTERACT', 'CONNECT', 'HELP','LEARN', 'SERVICES')



# https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
 
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
names(myfiles) <- gsub(".*[-]([^.]+)[,].*", "\\1", temp) %>%
  gsub("[[:space:]]", "",.)


# Functions: lower and upper confidence intervals of clickthrough rates to be passed to Summary 
lowCI = function(click_through_rate, total_clicks) {
  click_through_rate - (qt(0.975, df = total_clicks - 1) * sd(click_through_rate)/ sqrt(total_clicks))
}

upperCI = function(click_through_rate, total_clicks) {
  click_through_rate + (qt(0.975, df = total_clicks - 1) * sd(click_through_rate)/ sqrt(total_clicks))
}

# -- Summary --------
# Summary of all variations from the list of dfs 
# Columns include: Version, Clicks, Total Clicks for the Test, Low CI, Upper CI, and Method (Control/Variation)
summary = myfiles %>%
  lapply(., function(x) x[x[,grep("Name", names(x))]%in% v_names,] ) %>%
  bind_rows(., .id = "version") %>%
  select(., version, clicks = `No..clicks`) %>%
  mutate(total_clicks = lapply(myfiles, function(x) sum(x$`No..clicks`))) %>%
  mutate(total_clicks = as.numeric(total_clicks)) %>%
  mutate(.,click_through_rate = (clicks/total_clicks)) %>%
  mutate(., low_ci = lowCI(click_through_rate, total_clicks)) %>%
  mutate(., upper_ci =  upperCI(click_through_rate, total_clicks)) %>%
  mutate( method = ifelse(version == 'Interact', 'Control', 'Variation'))




# -- Pot the Summary -----
# fct_reorder makes it so that the factors are ordered by click_through_rate
ggplot(data = summary, aes(fct_reorder(version, click_through_rate), click_through_rate, color = method)) +
  geom_point() +
  geom_errorbar(aes(ymin = low_ci, ymax = upper_ci)) + 
  xlab("Tested Version") +
  ylab("Click through rate") +
  ggtitle("Control vs Variation tests") +
  theme_minimal()


## -- Analysis -----

ab_test = summary %>%
  filter(version != "Interact") %>%
  rowwise() %>%
  #Basically have Interact's clicks/ total clicks inputted into prop.test for all variations of the AB test
  mutate(pval = prop.test(x = c(summary$clicks[1], clicks),
                          n = c(summary$total_clicks[1], total_clicks)
                          )$p.value) %>%
  ungroup() %>%
  mutate(adj_pval = p.adjust(pval, method = "bonferroni")) %>%
  as.data.frame()
  
  
# Filter Adjusted p-values by 0.05
top_variants = ab_test %>%
  select(version, click_through_rate, pval, adj_pval) %>%
  filter(adj_pval <= 0.05)

# -- Plot Top Variants -----
# ggplot(data = top_variants, aes(x = adj_pval, y = click_through_rate, color = version)) +
#   geom_point()
  
