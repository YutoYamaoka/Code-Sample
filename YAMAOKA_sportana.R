###load packages used to extract and clean data.
library(tidyverse)
library(xlsx)
library(readxl)
library(data.table)

###load packages used for visualization
library(ggplot2)


#####load all files from the same file
setwd("C:/~myfile")

result <- list.files(path = "C:/~myfile" , pattern = "*.xlsx", full.names = TRUE) %>% 
  map_df(~read_excel(.x)%>%
           mutate(across(.fns = as.character))) %>%
  type_convert()

###preserve merged data in a file 
write.xlsx(result, "C:/~myfile")

###reload a data frame after reshaping it by hand on Excel. 
###I mainly change the column names of Japanese letters into ones of alphabet by hand.
hanshin_2021 <- read_xlsx("Hanshin_2021.xlsx")


###remove rows with the only appearance of defense.(row data includes the appearances of defence only)
hanshin_2021 <- hanshin_2021%>%
  filter(bat_result_jp != "---")

##divide a column of calender(yyyy/mm/dd) into two columuns of month and day (and omit the info of year.)
hanshin_2021 <- hanshin_2021%>%
  separate(day_month, c("Year", "Month", "Day"))%>%
  select(-"Year")

###and regard May and April as one month
###it is conventional to do so when you see stats of NPB baseball because few games are held in March
hanshin_2021 <- hanshin_2021%>%
  mutate(baseball_callender = case_when(Month == '03' | Month == '04' ~ "3.4",
                                        Month == '05' ~ "5",
                                        Month == '06' ~ "6",
                                        Month == '07' ~ "7",
                                        Month == '08' ~ "8",
                                        Month == '09' ~ "9",
                                        Month == '10' ~ "10"))

hanshin_2021$baseball_callender <- as.numeric(hanshin_2021$baseball_callender)

hanshin_2021$Day <- as.numeric(hanshin_2021$Day)

hanshin_2021 <- hanshin_2021%>%
  mutate(BA_callender = case_when(Month == '03' | Month == '04' ~ "3.4",
                                  Month == '05' ~ "5",
                                  Month == '06' ~ "6",
                                  Month == '07' & Day <= 06 ~ "6",
                                  Month == '07' & Day > 06 ~ "7",
                                  Month == '08' ~ "8",
                                  Month == '09' ~ "9",
                                  Month == '10' ~ "10"))

###make a indicator"AfterAkansuyo" that takes 1 after July 6th and otherwise 0.
hanshin_2021$BA_callender <- as.numeric(hanshin_2021$BA_callender)

hanshin_2021 <- hanshin_2021%>%
  mutate(AfterAkansuyo = case_when(BA_callender <= 6 ~ 0,
                                   TRUE ~ 1))



###make a function of removing all characters other than minus(-) from cells and keep numbers,
###omit them from the column named "gap".
minusfunction <- function(s) as.numeric(sub(".*?([-+]?\\d*\\.?\\d+).*", "\\1", s))
hanshin_2021$gap <- minusfunction(hanshin_2021$gap)

###class results into whether they are counted as 打席数 or 打数.
hanshin_2021 <- hanshin_2021%>%
  mutate(atbat_counted = case_when(bat_result_class == 0 ~ 0,
                                   TRUE ~ 1))

###calculate each average of players. 
hanshin_2021$bat_result_class <- as.numeric(hanshin_2021$bat_result_class)

average_2021 <- hanshin_2021%>%
  filter(atbat_counted == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(Ave = mean(avecount))

###calculate each average with Runner in Scoring Position.
average_RISP_2021 <- hanshin_2021%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1|R3B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(Ave_RISP = mean(avecount))

###calculate each average without Runner in Scoring Position.
average_noRISP_2021 <- hanshin_2021%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 0 & R3B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(Ave_noRISP = mean(avecount))

###calculate each average man on second 
average_2b_2021 <- hanshin_2021%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(Ave_R2B = mean(avecount))

##calculate each average man on third and no man on second.
average_only3b_2021 <- hanshin_2021%>%
  filter(atbat_counted == 1)%>%
  filter(R3B == 1 & R2B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(Ave_onlyR3B = mean(avecount))


Ave_list <- average_2021%>%
  left_join(average_noRISP_2021, by = "batter")%>%
  left_join(average_RISP_2021, by = "batter")%>%
  left_join(average_2b_2021, by = "batter")%>%
  left_join(average_only3b_2021, by = "batter")%>%
  filter(batter %in% c("サンズ", "マルテ", "近本光司", "佐藤輝明",
                       "糸原健斗", "大山悠輔", "中野拓夢", "梅野隆太郎",
                       "糸井嘉男", "ロハスジュニア"))

Ave_list <- Ave_list%>%
  select(batter, "打率" = average, "得点圏外打率" = Ave_noRISP, "得点圏打率" = Ave_RISP,
         "ランナー2塁時打率" = Ave_R2B, "ランナー3塁時打率(2塁無)" = Ave_onlyR3B)%>%
  melt(id.vars = "batter", variable.name = "Case", value.name = "Average")

###make a graph ofaverage of each player 
Ave_list%>%
  ggplot()+
  geom_bar(aes(x = Case, y = Average, fill = Case), position = "stack", stat = "identity")+
  facet_grid(~batter)+
  theme(axis.text.x=element_blank())

###PRE period: calculate each average of players until July 6th
PREaverage_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve = mean(avecount))

###calculate each average with Runner in Scoring Position.
PREaverage_RISP_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1|R3B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve_RISP = mean(avecount))

###calculate each average without Runner in Scoring Position.
PREaverage_noRISP_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 0 & R3B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve_noRISP = mean(avecount))

###calculate each average man on second 
PREaverage_2b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve_R2B = mean(avecount))

##calcuate each average no man on second.
PREaverage_no2b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve_noR2B = mean(avecount))


##calculate each average man on third and no man on second.
PREaverage_only3b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R3B == 1 & R2B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(PREAve_onlyR3B = mean(avecount))

###merge data with five types of case
PREAve_list <- PREaverage_2021%>%
  left_join(PREaverage_noRISP_2021, by = "batter")%>%
  left_join(PREaverage_RISP_2021, by = "batter")%>%
  left_join(PREaverage_2b_2021, by = "batter")%>%
  left_join(PREaverage_no2b_2021, by = "batter")%>%
  left_join(PREaverage_only3b_2021, by = "batter")%>%
  filter(batter %in% c("サンズ", "マルテ", "近本光司", "佐藤輝明",
                       "糸原健斗", "大山悠輔", "中野拓夢", "梅野隆太郎",
                       "糸井嘉男", "ロハスジュニア"))

###change wide dataframe into long form
PREAve_list <- PREAve_list%>%
  select(batter, "打率" = PREAve, "得点圏外打率" = PREAve_noRISP, "得点圏打率" = PREAve_RISP,
         "ランナー2塁時打率" = PREAve_R2B, "ランナー2塁なし" = PREAve_noR2B, "ランナー3塁時打率(2塁無)" = PREAve_onlyR3B)%>%
  melt(id.vars = "batter", variable.name = "Case", value.name = "PREAverage")


###After period: calculate each average of players after July 6th
POSTaverage_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 1)%>%
  filter(atbat_counted == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve = mean(avecount))

###calculate each average with Runner in Scoring Position.
POSTaverage_RISP_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 1)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1|R3B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve_RISP = mean(avecount))

###calculate each average without Runner in Scoring Position.
POSTaverage_noRISP_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 1)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 0 & R3B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve_noRISP = mean(avecount))

###calculate each average man on second 
POSTaverage_2b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 1)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 1)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve_R2B = mean(avecount))

###calculate each average no man on second
POSTaverage_no2b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 0)%>%
  filter(atbat_counted == 1)%>%
  filter(R2B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve_noR2B = mean(avecount))

##calculate each average man on third and no man on second
POSTaverage_only3b_2021 <- hanshin_2021%>%
  filter(AfterAkansuyo == 1)%>%
  filter(atbat_counted == 1)%>%
  filter(R3B == 1 & R2B == 0)%>%
  mutate(avecount = case_when(bat_result_class == 1 ~ 1,
                              TRUE ~ 0))%>%
  group_by(batter)%>%
  summarise(POSTAve_onlyR3B = mean(avecount))

###merge data with five types of case
POSTAve_list <- POSTaverage_2021%>%
  left_join(POSTaverage_noRISP_2021, by = "batter")%>%
  left_join(POSTaverage_RISP_2021, by = "batter")%>%
  left_join(POSTaverage_2b_2021, by = "batter")%>%
  left_join(POSTaverage_no2b_2021, by = "batter")%>%
  left_join(POSTaverage_only3b_2021, by = "batter")%>%
  filter(batter %in% c("サンズ", "マルテ", "近本光司", "佐藤輝明",
                       "糸原健斗", "大山悠輔", "中野拓夢", "梅野隆太郎",
                       "糸井嘉男", "ロハスジュニア"))
###change wide data frame into long form
POSTAve_list <- POSTAve_list%>%
  select(batter, "打率" = POSTAve, "得点圏外打率" = POSTAve_noRISP, "得点圏打率" = POSTAve_RISP,
         "ランナー2塁時打率" = POSTAve_R2B, "ランナー2塁なし" = POSTAve_noR2B, "ランナー3塁時打率(2塁無)" = POSTAve_onlyR3B)%>%
  melt(id.vars = "batter", variable.name = "Case", value.name = "POSTAverage")


#####merge POST with Post Ave_list
TimeAve_list <- left_join(PREAve_list, POSTAve_list, by = c("batter", "Case"))

TimeAve_list <- TimeAve_list%>%
  melt(id.vars =c("batter", "Case"), variable.name = "Time", value.name = "Average")
