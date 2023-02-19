#Load packages
library(ggplot2)
library(dplyr)
library(zoo)
library(devtools)
library(tidyverse)
library(lubridate)

#Create sample dataframe with Date list, as well as mean and standard devs for HRV, Sleep, subjective "Total Wellness" and session RPE
df <- tibble(
  Date = seq(as.Date("2022-07-01"), by = "day", length.out = 180),
  HRV = round(rnorm(n=180, mean = 50, sd = 10), 1),
  Sleep = round(rnorm(n = 180, mean = 8, sd = 0.5),.5),
  Wellness = round(rnorm(n = 180, mean = 70, sd = 10),1),
  sRPE = round(rnorm(n = 180, mean = 150, sd = 75)))
          
#Create columns for %ile scores for HRV, Sleep, Total Wellness, and sRPE
#Each wellness metric is divided by 3 and then summed to create equal weighting on "Recovery Score"
#Recovery Score multiplied by 100 to give score out of 100
df2 <- df %>%
  mutate(WorkloadScore = rank(sRPE)/length(sRPE)*100) %>%
  mutate(percent_rank_sleep = rank(Sleep)/length(Sleep)/3) %>%
  mutate(percent_rank_HRV = rank(HRV)/length(HRV)/3) %>%
  mutate(percent_rank_TotWell = rank(Wellness)/length(Wellness)/3) %>%
  mutate(RecoveryScore = (percent_rank_TotWell + percent_rank_sleep + percent_rank_HRV)*100)

#+/- 1.5 Standard Devs for the Recovery score to create "ideal range" ribbon around recovery score; this is where you would
#+ideally want to target for workload score
df2$Recov_Avg = mean(Load_Recovery$RecoveryScore)/1.5
df2$Recov_SD = sd(Load_Recovery$RecoveryScore)/1.5

df3 <- df2 %>%
  mutate(Recov_Top = RecoveryScore + Recov_SD) %>%
  mutate(Recov_Bottom = RecoveryScore - Recov_SD)

#Create thresholds for recovery score points so that results are either Red, Yellow or Green
df3$bucket = case_when(df3$RecoveryScore > 79 ~ "Green",
                   df3$RecoveryScore < 60 ~ "Red", 
                   TRUE ~ "Yellow")

#Create final df with data filtered to desired range (one month here) and desired metrics
df4 <- df3 %>%
  mutate(Date = ymd(Date)) %>%
  select(Date, WorkloadScore, RecoveryScore, Recov_Top, Recov_Bottom, bucket) %>%
  filter(df3$Date > '2022-12-01')

#Plot sRPE as a line, Recovery Score as points, and the Recov_Top and _Bottom as ribbon
ggplotly(ggplot(data = df4) + 
           geom_ribbon(aes(x = Date, ymax = Recov_Top, ymin = Recov_Bottom), fill = "grey", alpha = 0.5) +
           geom_point(aes(x = Date, y = RecoveryScore), size = 3, color = df4$bucket) +
           geom_line(aes(x = Date, y = WorkloadScore), color = "black", size = 1.5) +
           geom_point(aes(x = Date, y = WorkloadScore), color = "black", size = 3) +
           theme_ipsum())


       