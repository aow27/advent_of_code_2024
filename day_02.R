library(tidyverse)

input <- read_lines('7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9') %>% 
  as_tibble()

input <- read_lines('data/input_02.txt') %>% 
  as_tibble()

df<- input %>% 
  rename(line = value) %>% 
  mutate(report = row_number(),
         value = str_split(line, ' ')) %>% 
  unnest_longer(value, indices_to = 'level') %>% 
  relocate(level, .after = report) %>% 
  mutate(value = as.numeric(value),
         max_level = max(level),
         change = value - lag(value),
         
         safe = ifelse(between(abs(change),
                               1,
                               3),
                       1,
                       0) * sign(change),
         safe_sum = sum(safe, na.rm = T),
         .by = report) 

df %>% 
  summarise(across(c(max_level,
                     safe_sum),
                   max),
            .by = report) %>% 
  summarise(sum(max_level == abs(safe_sum) + 1))


# Part 2 ------------------------------------------------------------------


df<- input %>% 
  rename(line = value) %>% 
  mutate(report = row_number(),
         value = str_split(line, ' ')) %>% 
  unnest_longer(value, indices_to = 'level') %>% 
  relocate(level, .after = report) %>% 
  # filter(report == 403) %>%
  mutate(value = as.numeric(value),
         max_level = max(level),
         change = value - lag(value),
         change_2 = value - lag(value,2),
         
         safe = ifelse(between(abs(change),
                               -3,
                               3),
                       1,
                       NA) * sign(change),
         safe_2 = ifelse(between(abs(change_2),
                               1,
                               3),
                       1,
                       0) * sign(change),
         
         safe_sum = sum(safe, na.rm = T),
         safe_sum_2 = case_when(sum(change == 0, na.rm = T) > 1 ~ NA,
                                
                                max_level == abs(safe_sum) + 3 &
                                  safe_2 != 0 &
                                  sign(change) != sign(safe_sum) &
                                  (sign(lead(change_2) == sign(safe_sum)) | is.na(lead(change_2) == sign(safe_sum)))
                                ~ 1,
                                
                                max_level == abs(safe_sum) + 2 &
                                  change == 0 ~ 1),
         .by = report) %>% 
  filter(sum(is.na(safe)) <=  1)


df %>% 
  summarise(across(c(max_level,
                     safe_sum),
                   max),
            safe_sum_2 = sum(safe_sum_2, na.rm = T),
            .by = report) %>% 
  summarise(sum(max_level == abs(safe_sum) + 1),
            sum(safe_sum_2))
