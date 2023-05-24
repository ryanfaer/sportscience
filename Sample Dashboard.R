#Load Packages 
library(tidyverse)
library(rio)
library(timetk)
library(lubridate)
library(plotly)
library(zoo)

#Create Date List
Dates = seq(as.Date("2022-01-01"), by= "day", length.out = 360)

List_Date <- tibble(
  Date = rep(Dates,8))

#Create Roster List
Roster <- rep(c("PlayerA", "PlayerB", "PlayerC","PlayerD","PlayerE","PlayerF","PlayerG","PlayerH"),360)
Pos <- rep(c("Mid-Field","Mid-Field","Mid-Field","Mid-Field","Back-Field","Back-Field","Back-Field","Back-Field"),360)

List_Roster <- tibble(
  Athlete = Roster,
  Position = Pos)

List_Roster2 <- List_Roster %>% arrange(Athlete)


#Build Date+Roster List
List_RosterDate <- List_Roster2 %>%
  mutate(Date = List_Date$Date)

#Create sample dataframe
sampleDF <- List_RosterDate %>% 
  mutate(
    Load_Prac = round(rnorm(n=2880, mean = 700, sd = 120),0),
    Load_SC = round(rnorm(n=2880, mean = 250, sd = 30),0),
    Load_Comp = round(rnorm(n=2880, mean = 500, sd = 80),0),
    Practice_HR_AVG = round(rnorm(n=2880, mean = 150, sd = 12),0),
    Practice_HR_Max = round(rnorm(n=2880, mean = 178, sd = 5),0),
    Distance = round(rnorm(n=2880, mean = 7500, sd = 1200),0),
    HSR = round(rnorm(n=2880, mean = 100, sd = 35),0),
    Accelerations = round(rnorm(n=2880, mean = 30, sd = 8),0),
    Decelerations = round(rnorm(n=2880, mean = 18, sd = 6),0),
    Sprints = round(rnorm(n=2880, mean = 3, sd = 1.3),0),
    Fatigue = round(rnorm(n=2880, mean = 70, sd = 6),0),
    Mood = round(rnorm(n=2880, mean = 80, sd = 10),0),
    Motivation = round(rnorm(n=2880, mean = 85, sd = 5),0),
    Soreness = round(rnorm(n=2880, mean = 65, sd = 20),0),
    Stress = round(rnorm(n=2880, mean = 60, sd = 23),0),
    Sleep = round(rnorm(n=2880, mean = 75, sd = 10),0)) %>%
  rename(MaxHR = "Practice_HR_Max") %>%
  mutate(Day = weekdays(Date),
         Month = month(Date),
         Week = week(Date)) %>%
  mutate(Decelerations = case_when(Decelerations < 0 ~ 0, TRUE ~ Decelerations),
         HSR = case_when(HSR < 0 ~ 0, TRUE ~ HSR),
         Sprints = case_when(Sprints < 0 ~ 0, TRUE ~ Sprints),
         Stress = case_when(Stress < 0 ~ 0, TRUE ~ Stress),
         Accelerations = case_when(Accelerations < 0 ~ 0, TRUE ~ Accelerations),
         Soreness = case_when(Soreness < 0 ~ 0, TRUE ~ Soreness)) %>%
  mutate(Load_Comp = case_when(Day %in% c("Monday", "Thursday") ~ Load_Comp, TRUE ~ 0),
         Load_Prac = case_when(Day == "Sunday" ~ 0, TRUE ~ Load_Prac),
         Load_SC = case_when(Day == "Tuesday" | Day == "Thursday" | Day == "Saturday" | Day == "Sunday" ~ 0, TRUE ~ Load_SC),
         Distance = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ 0, TRUE ~ Distance),
         HSR = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ 0, TRUE ~ HSR),
         Accelerations = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ 0, TRUE ~ Accelerations),
         Decelerations = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ 0, TRUE ~ Decelerations),
         Sprints = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ 0, TRUE ~ Sprints),
         Practice_HR_AVG = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ NA_real_, TRUE ~ Practice_HR_AVG),
         MaxHR = case_when((Load_Prac == 0) & (Load_Comp == 0) ~ NA_real_, TRUE ~ MaxHR)) %>%
  group_by(Athlete, Position) %>%
  arrange(Date) %>%
  mutate(Acute_HSR = rollapply(HSR, width = 7, FUN = sum, align = 'right', fill = NA),
         Chronic_HSR = rollapply(HSR, width = 28, FUN = sum, align = 'right', fill = NA)/4) %>%
  ungroup() %>%
  mutate(ACWR_HSR = round(Acute_HSR/Chronic_HSR,2)) %>%
  mutate(LSR = round(Distance - HSR,0)) %>%
  mutate(LSR = case_when(LSR < 0 ~ 0, TRUE ~ LSR)) %>%
  mutate(Total_Wellness = round(Fatigue+Soreness+Stress+Sleep+Mood+Motivation/6,0))

#Get HR max per player
HR_Data <- sampleDF %>%
  select(Athlete, Position, MaxHR) %>%
  drop_na() %>%
  group_by(Athlete, Position) %>%
  summarize(Max_HR = max(MaxHR)) %>%
  ungroup()

#merge HR data with Cleaned Time Series Data
sampleDF_2 <- merge(sampleDF,HR_Data, .by = c(Athlete, Position), all = T) %>%
  mutate(pct_MaxHR = MaxHR/Max_HR*100)

#Get Wellness %iles
Wellness_Data <- sampleDF_2 %>%
  select(Athlete, Position, Date, Fatigue, Stress, Soreness, Mood, Motivation, Sleep, Total_Wellness) %>%
  drop_na() %>%
  group_by(Athlete, Position) %>%
  mutate(percent_rank_TotWell = round(rank(Total_Wellness)/length(Total_Wellness)*100,0), na.rm = T) %>%
  ungroup()

#Get Workload %iles
Load_Pctiles <- sampleDF_2 %>%
  mutate(Tot_Load = Load_Comp + Load_Prac + Load_SC) %>%
  filter(Tot_Load > 0) %>%
  select(Athlete, Position, Date, Tot_Load) %>%
  drop_na() %>%
  group_by(Athlete, Position) %>%
  mutate(percent_rank_self = round(rank(Tot_Load)/length(Tot_Load)*100,0)) %>%
  ungroup()


#Create final %ile DF
sampleDF_pctiles <- merge(Wellness_Data, Load_Pctiles, .by = c(Athlete, Position, Date, all = T)) %>% 
  select(Athlete, Position, Date, percent_rank_self, percent_rank_TotWell)



    