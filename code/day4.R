# Day 4

# Setup
library(pacman)
p_load(readr, dplyr, janitor, tidyr, stringr, purrr, lubridate)


# Part 1 ----------------

# Find the guard with the most minutes asleep. What's their sleepiest minute?

dat <- read_delim("data/day4_1.txt", delim = "]", col_names = FALSE) 
dat$X1 <- gsub("\\[", "", dat$X1)
dat <- dat %>%
  separate(X1, into = c("date", "time"), sep = " ")
dat$time <- paste0(dat$time, ":00")
dat$stamp <- ymd_hms(paste(dat$date, dat$time, sep = " "))
dat$g_id <- parse_number(dat$X2)
dat <- dat %>%
  select(-time) %>%
  arrange(stamp) %>%
  mutate(min = minute(stamp), date = as.Date(date), hour = hour(stamp))

valid_times <- data.frame(
  expand.grid(
    unique(as.Date(dat$stamp)),
    0:59,
    0:23
  )) %>%
  set_names(c("day", "minute", "hour"))

big <- full_join(dat,
                 valid_times,
                 by = c("date" = "day", "min" = "minute", "hour")) %>%
  arrange(date, hour, min) %>%
  rename(status = X2)

big$status[str_detect(big$status, "begins shift")] <- "awake"
big$status[str_detect(big$status, "wakes")] <- "awake"
big$status[str_detect(big$status, "asleep")] <- "asleep"
big <- big %>%
  fill(g_id, status) %>%
  filter(hour == 0)

sleepiest_guard <- big %>%
  group_by(g_id) %>%
  summarise(mins_sleep = sum(status == "asleep")) %>%
  arrange(desc(mins_sleep)) %>%
  pull(g_id) %>%
  first

sleepiest_minute <- big %>%
  filter(g_id == sleepiest_guard) %>%
  group_by(min) %>%
  summarise(sleep_count = sum(status == "asleep")) %>%
  arrange(desc(sleep_count)) %>%
  pull(min) %>%
  first

sleepiest_guard * sleepiest_minute

# Part 2 - which guard has the highest sleep frequency in a single minute?

part_2 <- big %>%
  group_by(g_id, min) %>%
  summarise(sleep_count = sum(status == "asleep")) %>%
  arrange(desc(sleep_count))

first(part_2$g_id) * first(part_2$min)
