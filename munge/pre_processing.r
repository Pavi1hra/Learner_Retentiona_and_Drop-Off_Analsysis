#pre-processing
df<-cyber.security.7_enrolments

enrolled_at <- ymd_hms(df$enrolled_at, tz = "UTC")
df$year_enroled <- year(df$enrolled_at)
df$month_enroled <- month(df$enrolled_at)

df$fully_participated_at[df$fully_participated_at == ""] <- NA
df$fully_participated_at <- parse_date_time(df$fully_participated_at, orders = c("ymd HMS", "dmy HMS", "mdy HMS"), tz = "UTC")
df$year_completed <- ifelse(is.na(df$fully_participated_at), NA, year(df$fully_participated_at))
df$month_completed <- ifelse(is.na(df$fully_participated_at), NA, month(df$fully_participated_at))

adoption_pt_enrolments_7 <- df %>%
  group_by(year_enroled, month_enroled) %>%
  summarise(number_of_learners = n_distinct(learner_id))

completed_pt_enrolments_7 <- df %>%
  group_by(year_completed, month_completed) %>%
  summarise(number_of_learners = n_distinct(learner_id))

# Step 1: Calculate total learners across all months
total_learners <- sum(adoption_pt_enrolments_7$number_of_learners)

# Step 2: Calculate adoption rate for each month
adoption_pt_enrolments_7 <- adoption_pt_enrolments_7 %>%
  mutate(adoption_rate = (number_of_learners / total_learners) * 100)

cache("adoption_pt_enrolments_7")
completed_course <- completed_pt_enrolments_7$number_of_learners[-nrow(completed_pt_enrolments_7)] |>
  na.omit() |>
  sum()

total_enroled <- adoption_pt_enrolments_7$number_of_learners |>
  na.omit() |>
  sum()
total_completion_rate <- (completed_course / total_enroled) * 100
cache("total_completion_rate")

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
  filter(
    !is.na(last_completed_week_number),
    !is.na(last_completed_step_number)
  ) %>%
  mutate(
    week_step = paste(
      "Week", last_completed_week_number,
      "- Step", last_completed_step_number
    )
  )

drop_off_ordered <- drop_off_clean %>%
  arrange(last_completed_week_number, last_completed_step_number) %>%
  mutate(
    week_step = factor(
      week_step,
      levels = unique(week_step)
    )
  )

cache("drop_off_ordered")

leaving_survey_responses <- cyber.security.7_leaving.survey.responses
weekly_survey_responses <- cyber.security.7_weekly.sentiment.survey.responses
cache("weekly_survey_responses")
video_stats <- cyber.security.7_video.stats
df <- leaving_survey_responses %>%
  left_join(video_stats, by = c("last_completed_step" = "step_position"))

cache("df")

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
summary_stats <- step_activity %>%
  group_by(step) %>%
  summarise(
    median_duration = median(duration_secs, na.rm=TRUE),
    mean_duration = mean(duration_secs, na.rm=TRUE),
    p25 = quantile(duration_secs, 0.25, na.rm=TRUE),
    p75 = quantile(duration_secs, 0.75, na.rm=TRUE)
  )

cache("summary_stats")

step_activity <- step_activity %>%
  mutate(
    first_visited_at = as.POSIXct(first_visited_at, format = "%Y-%m-%d %H:%M:%S"),
    last_completed_at = as.POSIXct(last_completed_at, format = "%Y-%m-%d %H:%M:%S"),
    duration_secs = as.numeric(difftime(last_completed_at, first_visited_at, units = "secs"))
  )

# Step 3: Summarize median duration and learner count per step
step_summary <- step_activity %>%
  group_by(step) %>%
  summarise(
    median_duration_secs = median(duration_secs, na.rm = TRUE),
    learner_count = n_distinct(learner_id)
  )

# Step 4: Scale learner_count to duration scale for plotting line
max_duration <- max(step_summary$median_duration_secs, na.rm = TRUE)
max_learners <- max(step_summary$learner_count, na.rm = TRUE)

cache("max_duration")
cache("max_learners")

step_summary <- step_summary %>%
  mutate(
    learner_count_scaled = learner_count / max_learners * max_duration
  )

step_summary <- step_summary %>%
  mutate(
    step_major = as.numeric(sub("\\..*$", "", step)),
    step_minor = as.numeric(sub("^.*\\.", "", step))
  ) %>%
  arrange(step_major, step_minor) %>%
  mutate(
    step = factor(step, levels = step)
  )

cache("step_summary")
view(step_summary)

