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
library(dplyr)
library(lubridate)

df <- cyber.security.7_leaving.survey.responses

# Convert left_at to POSIXct datetime
df$left_at <- as.POSIXct(df$left_at, format='%Y-%m-%d %H:%M:%S')

# Extract year, month, and month name
df <- df %>%
  mutate(
    year = year(left_at),
    month = month(left_at),
    month_name = month.name[month]
  )

# Group by last completed week and step, but keep rows with NA and label them as dropped off early
drop_off <- df %>%
  mutate(
    # Create a new variable to identify learners with missing step info
    drop_off_status = ifelse(
      is.na(last_completed_week_number) | is.na(last_completed_step_number),
      "Dropped off before first step",
      "Completed some step"
    ),
    # Replace NAs in step numbers with 0 (or another marker) for grouping
    last_completed_week_number = ifelse(is.na(last_completed_week_number), 0, last_completed_week_number),
    last_completed_step_number = ifelse(is.na(last_completed_step_number), 0, last_completed_step_number)
  ) %>%
  group_by(year, month, last_completed_week_number, last_completed_step_number, drop_off_status) %>%
  summarise(number_of_learners = n(), .groups = "drop") %>%
  arrange(year, month, last_completed_week_number, last_completed_step_number)

# Create descriptive week_step labels, including for those dropped off before any step
drop_off <- drop_off %>%
  mutate(
    week_step = case_when(
      drop_off_status == "Dropped off before first step" ~ "Dropped off before Step 1",
      TRUE ~ paste("Week", last_completed_week_number, "- Step", last_completed_step_number)
    )
  )

# Order week_step factor for plotting (put "Dropped off before Step 1" first)
drop_off_ordered <- drop_off %>%
  arrange(last_completed_week_number, last_completed_step_number) %>%
  mutate(
    week_step = factor(
      week_step,
      levels = c("Dropped off before Step 1", unique(week_step[week_step != "Dropped off before Step 1"]))
    )
  )

    ggplot(drop_off_ordered, aes(x = week_step, y = number_of_learners)) +
      geom_bar(stat = "identity", fill = "#007BA7") +
      labs(
        title = "Number of Learners by Last Completed Week and Step",
        x = "Last Completed Week and Step",
        y = "Number of Learners"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) 

#----------------------------------------
actual_duration<-cyber.security.7_video.stats

actual_duration <- cyber.security.7_video.stats %>%
      mutate(
        step_position = as.character(step_position),      # 🔒 preserve step labels (e.g. "3.20")
        duration_hours = video_duration / 3600
      )

user_duration<- cyber.security.7_step.activity

#prepare data for scatter plot
plot_df <- user_duration %>%
  filter(learner_id %in% sample(unique(learner_id), 50)) %>%
  mutate(
    first_visited_at = ymd_hms(first_visited_at, tz = "UTC"),
    time_num = as.numeric(first_visited_at)
  )
#scatterplot for linearity check
ggplot(plot_df, aes(x = time_num, y = step)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  
  # Diagonal reference line (ideal linear progression)
  geom_abline(
    intercept = min(plot_df$step, na.rm = TRUE),
    slope = diff(range(plot_df$step, na.rm = TRUE)) /
      diff(range(plot_df$time_num, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  labs(
    title = "Linearity Diagnostic: Step Order vs Time",
    subtitle = "Dashed line indicates ideal linear progression",
    x = "Time (numeric)",
    y = "Step Order"
  ) +
  theme_minimal()


#not really sure what the below code does; don't delete without understanding impact on other functions
user_duration <- cyber.security.7_step.activity %>%
  mutate(
    step = as.character(step),   # 🔒 lock step format
    first_visited_at = ymd_hms(first_visited_at, quiet = TRUE),
    last_completed_at = ymd_hms(last_completed_at, quiet = TRUE),
    
    duration_seconds = if_else(
      is.na(last_completed_at),
      0,
      as.numeric(difftime(last_completed_at, first_visited_at, units = "secs"))
    ),
    
    duration_seconds = if_else(duration_seconds < 0, 0, duration_seconds)
  ) %>%
  group_by(step) %>%
  summarise(
    total_duration_seconds = sum(duration_seconds, na.rm = TRUE),
    total_duration_minutes = total_duration_seconds / 60,
    total_duration_hours = total_duration_seconds / 3600,
    .groups = "drop"
  )

#step vs time spent on it 
user_duration %>%
  filter(learner_id %in% sample(unique(learner_id), 20)) %>%
  ggplot(
    aes(
      x = first_visited_at,
      y = step,
      group = learner_id,
      color = learner_id
    )
  ) +
  geom_line(alpha = 0.4) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")


# Merge the two dataframes by step/step_position
combined_df <- actual_duration %>%
  left_join(
    user_duration %>% select(step = step, total_duration_hours),
    by = c("step_position" = "step")
  )

# Ensure step_position is treated as an ordered factor for plotting
combined_df <- combined_df %>%
  mutate(step_position = factor(step_position, levels = unique(step_position)))

library(dplyr)
library(ggplot2)

# Ensure step_position / step columns are character
actual_duration <- actual_duration %>%
  mutate(step_position = as.character(step_position))

user_duration <- user_duration %>%
  mutate(step = as.character(step))


# 1. Count number of learners per step
learners_per_step <- cyber.security.7_step.activity %>%
  mutate(step = as.character(step)) %>%
  group_by(step) %>%
  summarise(
    number_of_learners = n_distinct(learner_id),
    .groups = "drop"
  )

# 2. Calculate expected duration
expected_duration <- cyber.security.7_video.stats %>%
  mutate(
    step_position = as.character(step_position),
    video_duration_hours = video_duration / 3600
  ) %>%
  left_join(
    learners_per_step,
    by = c("step_position" = "step")
  ) %>%
  mutate(
    expected_duration_hours = video_duration_hours * number_of_learners
  )

combined_df <- expected_duration %>%
  left_join(
    user_duration %>% select(step, total_duration_hours),
    by = c("step_position" = "step")
  )

# Compute scale factor to align line with bars
scale_factor <- max(combined_df$expected_duration_hours, na.rm = TRUE) /
  max(combined_df$total_duration_hours, na.rm = TRUE)

ggplot(combined_df, aes(x = step_position)) +
  # Bars: expected duration
  geom_bar(
    aes(y = expected_duration_hours),
    stat = "identity",
    fill = "#007BA7"
  ) +
  # Line: actual user duration (scaled)
  geom_line(
    aes(y = total_duration_hours * scale_factor, group = 1),
    color = "#FF6347",
    size = 1
  ) +
  geom_point(
    aes(y = total_duration_hours * scale_factor),
    color = "#FF6347",
    size = 2
  ) +
  # Dual y-axis
  scale_y_continuous(
    name = "Expected Duration (hours)",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Actual User Duration (hours)"
    )
  ) +
  labs(
    title = "Expected vs Actual Time Spent per Step",
    x = "Step Position"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
leaving_survey_responses <- cyber.security.7_leaving.survey.responses
weekly_survey_responses <- cyber.security.2_weekly.sentiment.survey.responses
video_stats <- cyber.security.7_video.stats
df <- leaving_survey_responses %>%
  left_join(video_stats, by = c("last_completed_step" = "step_position"))
cor.test(df$video_duration, df$last_completed_week_number, method="spearman")

step_activity <- cyber.security.7_step.activity
step_activity <- step_activity %>%
  mutate(
    first_visited_at = as.POSIXct(first_visited_at, format="%Y-%m-%d %H:%M:%S"),
    last_completed_at  = as.POSIXct(last_completed_at,  format="%Y-%m-%d %H:%M:%S")
  )
step_activity <- step_activity %>%
  mutate(
    duration_secs = as.numeric(difftime(last_completed_at, first_visited_at, units = "secs"))
  )
step_summary <- step_activity %>%
  group_by(step) %>%
  summarise(
    median_duration = median(duration_secs, na.rm=TRUE),
    mean_duration = mean(duration_secs, na.rm=TRUE),
    p25 = quantile(duration_secs, 0.25, na.rm=TRUE),
    p75 = quantile(duration_secs, 0.75, na.rm=TRUE)
  )

ggplot(step_summary, aes(x=step, y=median_duration)) +
  geom_col() +
  labs(y="Median duration (secs)", x="Step")

library(dplyr)
library(ggplot2)
library(scales)  # for pretty_breaks()

# Step 1 & 2: Same as before (convert and calculate durations)
step_activity <- step_activity %>%
  mutate(
    first_visited_at = as.POSIXct(first_visited_at, format = "%Y-%m-%d %H:%M:%S"),
    last_visited_at = as.POSIXct(last_visited_at, format = "%Y-%m-%d %H:%M:%S"),
    duration_secs = as.numeric(difftime(last_visited_at, first_visited_at, units = "secs"))
  )

# Step 3: Summarize median duration and learner count per step
step_summary <- step_activity %>%
  group_by(step) %>%
  summarise(
    median_duration_secs = median(duration_secs, na.rm = TRUE),
    learner_count = n_distinct(learner_id)
  )

step_summary <- step_summary %>%
  mutate(step_num = as.numeric(step)) %>%
  arrange(step_num) %>%
  mutate(step_factor = factor(step, levels = unique(step)))

# Recompute scaling values
max_duration <- max(step_summary$median_duration_secs, na.rm = TRUE)
max_learners <- max(step_summary$learner_count, na.rm = TRUE)

step_summary <- step_summary %>%
  mutate(
    learner_count_scaled = learner_count / max_learners * max_duration
  )

# Step 5: Plot bars + line with secondary y-axis
ggplot(step_summary, aes(x = step_factor)) +
  geom_col(aes(y = median_duration_secs),
           fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = learner_count_scaled, group = 1),
            color = "firebrick", size = 1) +
  geom_point(aes(y = learner_count_scaled),
             color = "firebrick", size = 2) +
  scale_y_continuous(
    name = "Median Duration (seconds)",
    sec.axis = sec_axis(~ . / max_duration * max_learners,
                        name = "Number of Learners")
  ) +
  labs(
    x = "Step",
    title = "Median Duration per Step with Learner Count Overlay"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))



view(df)
view(expected_duration)
view(combined_df)
print(completion_rate)

