### CONSTITUTIONS

# Campbell McNolty - May 2022

# Loading packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(corrplot)) install.packages('corrplot')
if (!require(plm)) install.packages('plm')
if (!require(stargazer)) install.packages('stargazer')
if (!require(estimatr)) install.packages('estimatr')
if (!require(zoo)) install.packages('zoo')
library(tidyverse)
library(readxl)
library(corrplot)
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(estimatr)
library(zoo)

# Import and filter data

mdp.df <- read_xlsx("mpd2020.xlsx", sheet = "GDP pc") %>%
  pivot_longer(
    cols = Afghanistan:Zimbabwe,
    names_to = "country",
    values_to = "GDP"
  )

ccp.df <- read_csv("ccpcnc_v3.csv") %>% 
  select(country, year,  market, specleg_4, forinves, bank, provwork, 
         provhlth, expcond_1, expcond_2, proprght,
        intrght_1, medmark_1, econplan)

p5.df <- read_excel("~/Study/ECF5060 - Applied Research/Data Analysis/p5v2018_1946-2018.xls") 
p5.df <- p5.df %>% select(country, year, polity2)

# Cleaning and merging data sets into one panel

panel.df <- merge(ccp.df, mdp.df, by=c("country", "year"))
panel.df <- merge(panel.df, p5.df, by=c("country", "year"))
panel.df$provhlth <- as.numeric(panel.df$provhlth)
panel.df$expcond_1 <- as.numeric(panel.df$expcond_1)
panel.df$proprght <- as.numeric(panel.df$proprght)
panel.df$freecomp <- as.numeric(panel.df$freecomp)
panel.df$intrght_1 <- as.numeric(panel.df$intrght_1)
panel.df$econplan <- as.numeric(panel.df$econplan)

# Removing outliers

panel.df <- panel.df %>% filter(polity2 > -11)

# Setting "no" values for indicator variables to "0"
 
panel.df <- panel.df %>% mutate(market = replace(market,market == 2, 0)) 
panel.df <- panel.df %>% mutate(market = replace(market,market == 96, NA))

panel.df <- panel.df %>% mutate(forinves = replace(forinves,forinves == 2, 1))
panel.df <- panel.df %>% mutate(forinves = replace(forinves,forinves == 3, 1))
panel.df <- panel.df %>% mutate(forinves = replace(forinves,forinves == 4, 0))
panel.df <- panel.df %>% mutate(forinves = replace(forinves,forinves == 96, NA))

panel.df <- panel.df %>% mutate(bank = replace(bank, bank == 2, 0))
panel.df <- panel.df %>% mutate(bank = replace(bank, bank == 96, NA))

panel.df <- panel.df %>% mutate(provwork = replace(provwork, provwork == 2, 0))
panel.df <- panel.df %>% mutate(provwork = replace(provwork, provwork == 96, NA))

panel.df <- panel.df %>% mutate(provhlth = replace(provhlth, provhlth == 2, 0))
panel.df <- panel.df %>% mutate(provhlth = replace(provhlth, provhlth == 96, NA))

panel.df <- panel.df %>% mutate(exprop = replace(exprop, exprop == 2, 0))
panel.df <- panel.df %>% mutate(exprop = replace(exprop, exprop == 96, NA))
panel.df <- panel.df %>% mutate(exprop = replace(exprop, exprop == 98, NA))

panel.df <- panel.df %>% mutate(proprght = replace(proprght, proprght == 2, 0))
panel.df <- panel.df %>% mutate(proprght = replace(proprght, proprght == 90, NA))
panel.df <- panel.df %>% mutate(proprght = replace(proprght, proprght == 96, NA))

panel.df <- panel.df %>% mutate(freecomp = replace(freecomp, freecomp == 2, 0))
panel.df <- panel.df %>% mutate(freecomp = replace(freecomp, freecomp == 96, NA))

panel.df <- panel.df %>% mutate(econplan = replace(econplan, econplan == 2, 0))
panel.df <- panel.df %>% mutate(econplan = replace(econplan, econplan == 96, NA))

#Removing one duplicate observation

panel.df <- panel.df %>%   
  group_by(country, year) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  mutate(is_duplicated = dup_id > 1)

panel.df <- panel.df %>% filter(dup_id == 1)

# creating a variable with ten year moving averages of GDP and polity scores

panel.df$GDP10 <- rollapply(panel.df$GDP, 10, FUN = "mean", fill = NA)
panel.df$polity2_10 <- rollapply(panel.df$polity2, 10, FUN = "mean", fill = NA) 

# Creating data subsets limited to specific time periods

panel.df.1875_1965 <- panel.df %>%
  filter(1875 <= year & year <= 1965)

Panel.df.1965 <- panel.df %>%
  filter(year >= 1965)

### GROWTH MODELS

# Simple correlation between polity scores and GDP

raschky_model.FE <- plm(log(GDP10) ~ polity2_10,
                        data = panel.df,
                        effect = "twoways",
                        index = c("country", "year"), 
                        model = "within")

raschky_model.covs <- vcovHC(raschky_model.FE, type = "HC1", cluster = "group")
raschky_model.se <- sqrt(diag(raschky_model.covs))

# Fixed Effects model including all variables

full_FE.model <- plm(log(GDP10) ~ polity2_10 + log(lag(GDP10, 1)) + market + specleg_4 + 
                       forinves + bank + provwork + provhlth + expcond_1 + 
                       expcond_2 + proprght + intrght_1 + medmark_1 + econplan, 
                     data = panel.df,
                     effect = "twoways",
                     index = c("country", "year"),
                     model = "within")

full_FE.model.covs <- vcovHC(full_FE.model, type = "HC1", cluster = "group")
full_FE.model.se <- sqrt(diag(full_FE.model.covs))

# Fixed effects model excluding polity scores

FE_ex_polity.model <- plm(log(GDP10) ~  log(lag(GDP10, 1)) + market + specleg_4 + 
                       forinves + bank + provwork + provhlth + expcond_1 + 
                       expcond_2 + proprght + intrght_1 + medmark_1 + econplan, 
                     data = panel.df,
                     effect = "twoways",
                     index = c("country", "year"),
                     model = "within")

FE_ex_polity.model.covs <- vcovHC(FE_ex_polity.model, type = "HC1", cluster = "group")
FE_ex_polity.model.se <- sqrt(diag(FE_ex_polity.model.covs))


### DEMOCRACY MODELS

# All data

polity_FE.model <- plm(polity2 ~ GDP + market + specleg_4 + forinves + bank + provwork + 
                         provhlth + expcond_1 + expcond_2 + proprght + 
                         intrght_1 + medmark_1 + econplan, 
                            data = panel.df,
                            effect = "twoways",
                            index = c("country", "year"),
                            model = "within")

polity_FE.model.covs <- vcovHC(polity_FE.model, type = "HC1", cluster = "group")
polity_FE.model.se <- sqrt(diag(polity_FE.model.covs))

# Using only data from 1875 - 1965

polity_FE.1875.model <- plm(polity2 ~ GDP + market + specleg_4 + forinves + bank + provwork + 
                         provhlth + expcond_1 + expcond_2 + proprght + 
                         intrght_1 + medmark_1 + econplan, 
                       data = panel.df.1875_1965,
                       effect = "twoways",
                       index = c("country", "year"),
                       model = "within")

polity_FE.1875.model.covs <- vcovHC(polity_FE.1875.model, type = "HC1", cluster = "group")
polity_FE.1875.model.se <- sqrt(diag(polity_FE.1875.model.covs))


# Using only data from 1965 onwards

polity_FE.1965.model <- plm(polity2 ~ GDP + market + specleg_4 + forinves + bank + provwork + 
                         provhlth + expcond_1 + expcond_2 + proprght + 
                         intrght_1 + medmark_1 + econplan, 
                       data = Panel.df.1965,
                       effect = "twoways",
                       index = c("country", "year"),
                       model = "within")

polity_FE.1965.model.covs <- vcovHC(polity_FE.1965.model, type = "HC1", cluster = "group")
polity_FE.1965.model.se <- sqrt(diag(polity_FE.1965.model.covs))


# Display results with cluster robust standard errors

Table1 <- stargazer(raschky_model.FE, full_FE.model, FE_ex_polity.model, 
          type = "text", 
          se = list(raschky_model.se, full_FE.model.se, FE_ex_polity.model.se), 
          title = "Table 1: Growth models",
          dep.var.caption = "Outcome variable: Logarithm of 10 year moving averge of per capita GDP",
          dep.var.labels.include = FALSE,
          add.lines = list(c("Fixed effects?", "two-way", "two-way", "two-way")),
          covariate.labels = c("PolityIV score (10 year moving average)", "Outcome variable lagged one year", 
                               "Free market", "Finance bills", "Foreign Investment", 
                               "Central bank","Provide work",  "Provide health", 
                               "Infrastructure", "Redistribution", "Property rights",
                               "UN declaration of rights", "Media market", "Economic plan"))




Table2 <- stargazer(polity_FE.model, polity_FE.1875.model, polity_FE.1965.model,
                              type = "text", 
                              se = list(polity_FE.model.se, polity_FE.1875.model.se, polity_FE.1965.model.se),
                              title = "Table 2: Constitutional attributes on their relationship to polity scores",
                              dep.var.caption = "Outcome variable: Polity scores",
                              column.labels = c("All data", "1875 - 1965", "1965 - 2018"),
                              dep.var.labels.include = FALSE,
                              model.numbers = FALSE,
                              add.lines = list(c("Fixed effects?", "two-way", "two-way", "two-way")),
                              covariate.labels = c("Log GDP per capita", "Free market", 
                                                   "Finance bills", "Foreign Investment", "Central bank",
                                                   "Provide work",  "Provide health", 
                                                   "Infrastructure", "Redistribution", "Property rights",
                                                   "UN declaration of rights", "Media market", "Economic plan")  )


