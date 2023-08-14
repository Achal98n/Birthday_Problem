library(tidyverse)
sample(365, 28, replace = TRUE)
# When the values 2 and 1 appear for the second time, they're TRUE
duplicated(c(1, 2, 3, 4, 2, 5, 1))
# This asks "are any values here duplicated?"
any(duplicated(c(1, 2, 3, 4, 2, 5, 1)))
any(duplicated(sample(365, 28, replace = TRUE)))
crossing(first = c("a", "b", "c"),
         second = c(1, 2, 3))
# Generate combinations of 10,000 replications and number of people (2-28)
crossed <- summarized <- crossing(trial = seq_len(10000),
                                  people = seq(2, 28, 2))

crossed
# Generate a list column of samples
sim <- crossed %>%
  mutate(birthday = map(people, ~ sample(365,., replace = TRUE)),
         multiple = map_lgl(birthday, ~ any(duplicated(.))))

sim
# Find the percentage of cases where multiple is TRUE
summarized <- sim %>%
  group_by(people) %>%
  summarize(chance = mean(multiple))

summarized
# pbirthday is not vectorized, so we have to use map_dbl
summarized %>%
  mutate(exact = map_dbl(people, pbirthday)) %>%
  ggplot(aes(people, chance)) +
  geom_line() +
  geom_line(aes(y = exact), lty = 2, color = "red") +
  labs(x = "# of people",
       y = "Probability two have the same birthday")
