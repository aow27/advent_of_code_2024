library(tidyverse)

source('AoC functions.R')


download_advent(2024,
                5,
                funct = readr::read_lines)


df <- input %>% 
  as_tibble() %>% 
  slice(1:(which(value == "")-1)) %>% 
  mutate(rule = row_number(),
         page = str_split(value, '\\|'),
         order = str_replace(value,
                        '\\|',
                         '_'),
         order_wrong = str_c(str_sub(value, 
                                     4,
                                     5), '_',
                             str_sub(value,
                                     1,
                                     2)))

rules <- input %>% 
  as_tibble() %>% 
  slice((which(value == "") + 1):max(row_number())) %>% 
  mutate(rule = row_number(),
         pos = str_split(value, ','),
         pairs = map(pos,
                     ~ str_c(.x[1:length(.x) - 1], '_',
                             .x[2:length(.x)])),
         comb = map(pos,
                    ~ (combn(.x, 2, simplify = FALSE)) %>% 
                      map_chr(~ paste(.x, collapse = '_'))
         )) %>% 
  mutate(wrong_flag = map(comb,
                          ~ sum(.x %in% df$order_wrong)),
         middle = map(pos,
                      ~ .x[ceiling(length(.x)/2)])) %>% 
  select(-pos,
         -pairs) 

rules %>% 
  filter(wrong_flag == 0) %>% 
  summarise(sum(as.numeric(middle)))


# Part 2 ------------------------------------------------------------------


rules <- input %>% 
  as_tibble() %>% 
  slice((which(value == "") + 1):max(row_number())) %>% 
  mutate(rule = row_number(),
         pos = str_split(value, ','),
         comb = map(pos,
                    ~ (combn(.x, 2, simplify = FALSE)) %>% 
                      map_chr(~ paste(.x, collapse = '_'))
         )) %>% 
  mutate(wrong_flag = map(comb,
                          ~ sum(.x %in% df$order_wrong)),
         wrong_check = map(comb,
                          ~ (.x %in% df$order_wrong)),
         middle = map(pos,
                      ~ .x[ceiling(length(.x)/2)])) %>% 
  filter(wrong_flag != 0)
