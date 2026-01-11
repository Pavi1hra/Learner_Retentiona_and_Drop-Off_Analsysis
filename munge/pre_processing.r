#pre-processing
library(dplyr)
df<-cyber.security.7_enrolments

enrolled_at <- ymd_hms(df$enrolled_at, tz = "UTC")
df$year_enroled <- year(df$enrolled_at)
df$month_enroled <- month_enroled(df$enrolled_at)

df$fully_participated_at[df$fully_participated_at == ""] <- NA
df$fully_participated_at <- parse_date_time(df$fully_participated_at, orders = c("ymd HMS", "dmy HMS", "mdy HMS"), tz = "UTC")
df$year_completed <- ifelse(is.na(df$fully_participated_at), NA, year(df$fully_participated_at))
df$month_completed <- ifelse(is.na(df$fully_participated_at), NA, month(df$fully_participated_at))

gender_pt_enrolments_7 <- df %>%
  group_by(gender) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

adoption_pt_enrolments_7 <- df %>%
  group_by(year_enroled, month_enroled) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

completed_pt_enrolments_7 <- df %>%
  group_by(year_completed, month_completed) %>%
  summarise(number_of_learners = n_distinct(learner_id)
  )

df<- cyber.security.7_leaving.survey.responses
df$left_at <- as.POSIXct(df$left_at, format='%Y-%m-%d %H:%M:%S')

df <- df %>%
  mutate(
    year = year(left_at),
    month = month(left_at),
    month_name = month.name[month]  
  )

drop_off <- df %>%
  group_by(year, month, last_completed_week_number, last_completed_step_number) %>%
  summarise(number_of_learners = n(), .groups = "drop") %>%
  arrange(year, month, last_completed_week_number, last_completed_step_number)

drop_off_clean <- drop_off %>%
  filter(!is.na(last_completed_step_number) & last_completed_step_number != "Missing")

view()
cache("completed_pt_enrolments_7")


