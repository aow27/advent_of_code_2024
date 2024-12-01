library(tidyverse)


source('AoC functions.R')

download_advent(2024,
                1,
                funct = readr::read_delim,
                delim = '\  ',
                skip_empty_rows = FALSE,
                col_names = c('list_1',
                              'remove',
                              'list_2'))

ordered_input <- tibble(list_1 = sort(input$list_1),
                        list_2 = sort(input$list_2)) %>% 
  mutate(diff = abs(list_1-list_2)) 

ordered_input %>% 
  summarise(sum = sum(diff)) 
  

# Part 2 ------------------------------------------------------------------

input %>% 
  rowwise() %>% 
  mutate(num_matches = sum(list_1 == input$list_2),
         similarity_score = num_matches * list_1) %>%
  ungroup() %>% 
  summarise(sum(similarity_score))


