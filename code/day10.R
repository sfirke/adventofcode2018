# setup
library(pacman)
p_load(readr, ggplot2, dplyr, tidyr)

dat <- read_delim("data/day10.txt", delim = "=", col_names = FALSE) %>%
  separate(X2, into = c("x", "y"), sep = ", ") %>%
  separate(X3, into = c("x_speed", "y_speed"), sep = ", ") %>%
  select(-X1) %>%
  mutate_all(parse_number)
  
points <- select(dat, x, y)%>%
  mutate(y = -y) # because y denotes "units down" not up

speeds <- dat %>%
  select(x = x_speed, y = y_speed) %>%
  mutate(y = -y)

coords <- points

close_vals <- numeric(20000)
for(i in 1:20000){
  coords <- coords + speeds
  close_vals[i] <- mean(sd(coords$x), sd(coords$y))
  
}

summary(close_vals)
which.min(close_vals) # 10946 is closest time?
close_vals[10942:10950] # yep, this window gets closer then farther

adjusted <- points
for(i in 1:10946){
adjusted <- adjusted + speeds  
}

# plot
summary(adjusted$x); summary(adjusted$y)

ggplot(adjusted,
       aes(x, y)) +
  geom_point() +
  ylim(-300, -100)


# Part 2
# A freebie!  Copy and paste from above, 10946
# Does that mean I should have done part 1 differently?