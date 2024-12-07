library(tidyverse)


source('AoC functions.R')

download_advent(2024,
                3,
                funct = readr::read_lines)
input %>% 
  as_tibble( ) %>% 
  mutate(mults = str_extract_all(value, 
                            'mul\\([0-9]{1,3},[0-9]{1,3}\\)')) %>% 
  unnest_longer(mults, indices_to = 'col') %>% 
  select(-value) %>% 
  mutate(vals = str_extract_all(mults,'[0-9]+')) %>% 
  unnest_wider(vals, names_sep = '_') %>% 
  mutate(across(vals_1:vals_2,
                as.numeric)) %>% 
  summarise(sum(vals_1 * vals_2))

# Part 2 ------------------------------------------------------------------


df <- input %>% 
  as_tibble( ) %>% 
  mutate(mults = str_extract_all(value, 
                                 "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don\\'t\\(\\)|do\\(\\)"),
         row = row_number()) %>% 
  unnest_longer(mults, indices_to = 'col') %>% 
  mutate(enable_flag =case_when(mults == "don't()" ~ 0,
                                mults == "do()"    ~ 1)) %>% 
  # group_by(row) %>% 
  fill(enable_flag) %>% 
  # ungroup() %>% 
  select(-value) %>% 
  replace_na(list(enable_flag = 1)) %>%
  filter(str_detect(mults, 
                    "don\\'t\\(\\)|do\\(\\)", 
                    negate = T)) %>% 
  mutate(vals = str_extract_all(mults,'[0-9]+')) %>% 
  unnest_wider(vals, names_sep = '_') %>% 
  mutate(across(vals_1:vals_2,
                as.numeric))  

df %>% 
  summarise(sum(vals_1 * vals_2 * enable_flag))

