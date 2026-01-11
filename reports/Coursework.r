library(dplyr)
library(lubridate)
getwd()
setwd('//Users//pavithra_govinda_raj//Stats_Report')
library(ProjectTemplate)
create.project('MAS8600_Assignment')

renv()#requirement in the report
git()#version history is important so set up before beginning any work

setwd('//Users//pavithra_govinda_raj//Stats_Report//MAS8600_Assignment')
install.packages('renv')
renv::init()
getwd()
load.project()
ls()
df1<-cyber.security.2_enrolments
view(df1)
#pre-processing 
gender_pt <- df1 %>%
  group_by(gender) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

enrolled_at <- ymd_hms(df1$enrolled_at, tz = "UTC")
df1$year <- year(df1$enrolled_at)
df1$month <- month(df1$enrolled_at)


df1$fully_participated_at[df1$fully_participated_at == ""] <- NA
df1$fully_participated_at <- parse_date_time(df1$fully_participated_at, orders = c("ymd HMS", "dmy HMS", "mdy HMS"), tz = "UTC")
df1$year_completed <- ifelse(is.na(df1$fully_participated_at), NA, year(df1$fully_participated_at))
df1$month_completed <- ifelse(is.na(df1$fully_participated_at), NA, month(df1$fully_participated_at))



adoption_pt_enrolments_7 <- df1 %>%
  group_by(year, month) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

completed_pt_enrolments_7 <- df1 %>%
  group_by(year_completed, month_completed) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

view(df1)
view(completed_pt_enrolments_7)
