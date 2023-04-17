###This code sample includes some parts of the code used in 
##my undergraduate thesis.

###load used packages 
library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(fixest)
library(modelsummary)
library(ggplot2)

###set the directory
setwd("C:/YAMAOKA/~")

##load data.
toiletdist <- read_excel("_district.xlsx", sheet = 3)


####calculate latrine coverage of all districts in India.
toiletdist_per <- toiletdist%>%
  select("census_name","before_sbm" = `Total HH with Toilet before SBM`,
         "n_13" =`2013`, "n_14" =`2014-15`, "n_15" = `2015-16`, 
         "n_16" = `2016-17`,"n_17" =`2017-18`, "n_18" = `2018-19`, 
         "n_19" = `2019-20`, "n_20" =`2020-21`, "n_21" = `2021-22`,
         "tot_after" = `Total HH covered in (LOB + BLS + NoLB)`)%>%
  mutate("2012" = before_sbm/tot_after,
         "2013" = (before_sbm + n_13)/tot_after,
         "2014" = (before_sbm + n_13 + n_14)/tot_after,
         "2015" = (before_sbm + n_13 + n_14 + n_15)/tot_after,
         "2016" = (before_sbm + n_13 + n_14 + n_15 + n_16)/tot_after,
         "2017" = (before_sbm + n_13 + n_14 + n_15 + n_16 + n_17)/tot_after,
         "2018" = (before_sbm + n_13 + n_14 + n_15 + n_16 + n_17 + n_18)/tot_after,
         "2019" = (before_sbm + n_13 + n_14 + n_15 + n_16 + n_17 + n_18 + n_19)/tot_after,
         "2020" = (before_sbm + n_13 + n_14 + n_15 + n_16 + n_17 + n_18 + n_19 + n_20)/tot_after,
         "2021" = (before_sbm + n_13 + n_14 + n_15 + n_16 + n_17 + n_18 + n_19 + n_20 + n_21)/tot_after)%>%
  select("census_name","2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "tot_after")

###select needed columns and convert the wide data frame into long form. 
toiletdist_per <- toiletdist_per%>%
  select("census_name", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")%>%
  pivot_longer(cols = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
               names_to = "year",
               values_to = "coverage")

write.xlsx(toiletdist_per, "C:/YAMAOKA/~/treated_coverage.xlsx")
###end this section


####load several data
paneldist221217 <- read.csv("paneldist221105_3.csv") 
popdist <- read.csv("popdist.csv")
maritialdist <- read.csv("maritialratedist221105.csv")
treated_coverage <- read_excel("treated_coverage.xlsx")

###merge data by the "left_join" package according to their "census_name"
paneldist221217 <- left_join(paneldist221107, popdist, by = "census_name")
paneldist221217 <- left_join(paneldist221107, maritialdist, by = "census_name")
paneldist221217 <- left_join(paneldist221107, treated_coverage, by = "census_name")

###calculate crime rate
paneldist221217 <- paneldist221107%>%
  mutate(rape_rate = (total_rape/TOT_P)*100000,
         assault_rate = (assault_on_women/TOT_P)*100000,
         insult_rate = (insult_to_women_modesty/TOT_P)*100000,
         cruelty_rate = (cruelty_by_husband_or_his_relative/TOT_P)*100000)

#make dummy variable that takes 1 when a district get treated by policy, otherwise zero from 2014.
paneldist221217 <- paneldist221107%>%
  mutate(treated = case_when(year < 2014 ~ 0,
                             n_putlatrine == 0 & year == 2014 ~ 0,
                             n_putlatrine > 0 & year == 2014 ~ 1,
                             year > 2014 ~ 1))


###create a graph that indicates the changes in crime rates over time.
paneldist221217%>%
  group_by(year, lat_high)%>%
  summarise(assault_per_0.1M =sum(assault_on_women)/sum(TOT_P)*100000)%>%
  mutate(lat_high = fct_explicit_na(fct_recode(factor(lat_high),
                                               "High Toltet in 2014" = "1", "Low Toilet in 2014" = "0")))%>%
  ggplot(aes(x = year, y = assault_per_0.1M, group = lat_high ))+
  scale_x_continuous(breaks = seq(2011, 2019, by = 1))+
  geom_line(aes(linetype = lat_high), size =1)+
  geom_point(aes(x = year, y = assault_per_0.1M))+
  labs(x = "Year",
       y = "Assault cases per 100000 people",
       title = "Assault cases per 100000 people over year by Toilet Coverage")+
  theme_bw()


###run diff-in-diff regressions with three different specifications
modellist <- list(
  "Simple Rape" = feols(rape_rate ~ DiD_treat| year + district, data = paneldist221217, cluster =~district),
  "Controlled Rape" = feols(rape_rate ~ DiD_treat + LocationDwater_Away + No_drainage  + ODrisk | year, data = paneldist221217, cluster =~district),
  "FE Rape" = feols(rape_rate ~ DiD_treat + LocationDwater_Away*as.factor(year) + No_drainage*as.factor(year) + Total_HH_Dilapidated*as.factor(year)| district + year, data = paneldist221217, cluster =~district)
  )

###produce regression tables using the "msummary" package.
msummary(modellist,
         stars = c('*' = .1, '**' = .05, '***' = 0.01),
         coef_omit =  "^(?!DiD)",
         gof_omit = "AIC|BIC|Log.Lik.|R2 Pseudo")


###conduct event-study analysis
###set lag and lead year based on treated year.
eventpanel <- paneldist221217%>%
  mutate(relative = year - treated_year)

eventpanel <- eventpanel%>%
  mutate(lead3 = case_when(relative <= -3 ~ 1, TRUE ~0),
         lead2 = case_when(relative == -2 ~ 1, TRUE ~0),
         lead1 = case_when(relative == -1 ~ 1, TRUE ~0),
         lag0 = case_when(relative == 0 ~ 1, TRUE ~0),
         lag1 = case_when(relative == 1 ~ 1, TRUE ~0),
         lag2 = case_when(relative == 2 ~ 1, TRUE ~0),
         lag3 = case_when(relative == 3 ~ 1, TRUE ~0),
         lag4 = case_when(relative == 4 ~ 1, TRUE ~0),
         lag5 = case_when(relative >= 5 ~ 1, TRUE ~0))

###run regression for estimating the impact of a policy on an outcome of interest 
eventrape <- feols(rape_rate ~ lag5 + lag4 + lag3 + lag2 + lag1 + lag0
                   +lead2 + lead3 +LocationDwater_Away*as.factor(year) + No_drainage*as.factor(year) + ODrisk*as.factor(year) 
                   | district , data = eventpanel, cluster = ~district)

###plot the result of event-study analysis.
order_dist <- c("lead3", "lead2", "lag0",
                "lag1", "lag2", "lag3", "lag4", "lag5")

plotdistrape <- tibble(
  estimate = c(coef(eventrape)[order_dist], 0),
  se       = c(se(eventrape)[order_dist], 0),
  time = c(-3, -2, 0, 1, 2, 3, 4, 5, -1)
)

plotdistrape%>%
  ggplot(aes(x = time, y = estimate,
             ymin = estimate - 1.96*se,
             ymax = estimate + 1.96*se))+
  geom_line() +
  geom_point(size = 0.3, color = "black", alpha = 0.5) +
  geom_pointrange() +
  scale_x_continuous(breaks = seq(-3, 5, by = 1)) +
  geom_hline(yintercept = 0, color = "red", linetype = 3)+
  labs(x = "Years Relative to the 2014",
       y = "Event-Study Coefficients",
       title = "Event-Study Analysis of the Impact on Rape Rate")+
  theme_bw()