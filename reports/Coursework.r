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
view(adoption_pt_enrolments_7)

load('cached_objects.RData')
completed_course <- sum(completed_pt_enrolments_7$number_of_learners[-nrow(completed_pt_enrolments_7)])
total_enroled <- sum(adoption_pt_enrolments_7$number_of_learners)
completion_rate <- (completed_course/total_enroled) * 100

#__________________________
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

ggplot(drop_off_clean, aes(x = last_completed_step_number, y = number_of_learners)) +
  geom_bar(stat = "identity", fill = "#007BA7", show.legend = FALSE) + 
  labs(
    title = "Number of Learners by Last Completed Step",
    x = "Last Completed Step Number",
    y = "Number of Learners"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#----------------------------------------
df2<-cyber.security.7_enrolments

# Convert 'enrolled_at' to datetime format (if not already in POSIXct)
df2$enrolled_at <- as.POSIXct(df2$enrolled_at, format = "%Y-%m-%d %H:%M:%S")

# Add new columns for 'year' and 'month'
df2 <- df2 %>%
  mutate(
    year = year(enrolled_at),  # Extract year
    month = month(enrolled_at)  # Extract month
  )

ggplot(df2, aes(x = year, y = gender)) +
  geom_jitter(width = 0.1, height = 0.1, color = "blue", alpha = 0.6) +
  labs(
    title = "Scatter Plot of Month vs Gender",
    x = "Month",
    y = "Gender"
  ) +
  theme_minimal()

view(df2)

