library(tidyverse)

input <- readr::read_lines('data/input_04.txt') %>% 
  as_tibble()

word = 'XMAS'

letters <- input %>% 
  mutate(row = row_number(),
         value = str_split(value, '')) %>% 
  unnest_longer(value, indices_to = 'col') %>% 
  mutate(col_ref_min = col - 1,
         col_ref_max = col + 1,
         row_ref_min = row - 1,
         row_ref_max = row + 1,
  ) %>% 
  mutate(value_updated = str_c(value), 
         row_updated = row,
         col_updated= col,
         .after = value)

reference <- letters %>% 
  select(-ends_with('updated')) %>% 
  select(-contains('_ref'))

for (i in 1:3){
  
  letters <- letters %>% 
    filter(value_updated == str_sub(word,
                                    1,
                                    str_length(value_updated))) %>% 
    left_join(reference,
              join_by(between(y$col,
                              x$col_ref_min,
                              x$col_ref_max),
                      between(y$row,
                              x$row_ref_min,
                              x$row_ref_max)), suffix = c('', '_new')
    ) %>% 
    mutate(value_updated = str_c(value_updated,
                                 value_new), .after = value) %>% 
    filter(!(col == col_new &
               row == row_new)) %>% 
    mutate(row_updated = str_c(row_updated, '_', row_new),
           col_updated = str_c(col_updated, '_', col_new),
           row = row_new,
           col = col_new,
           col_ref_min = col - 1,
           col_ref_max = col + 1,
           row_ref_min = row - 1,
           row_ref_max = row + 1) %>% 
    select(-ends_with('_new'))
  
  if (i == 3) {
    letters <- letters %>% 
      filter(value_updated == str_sub(word,
                                      1,
                                      str_length(value_updated)))
  }
}        


direction_check <- function(input){ 
  
  diff <- str_split(input, '_') %>% 
    map(as.numeric) %>% 
    map (~ .x[1:3] - .x[2:4])
  
  
  diff %>% map_dbl( ~ ifelse(all(.x == 1), 1, 
                             ifelse(all(.x == 0), 0, 
                                    ifelse(all(.x == -1), -1, NA))))
  
}


letters_1 <- letters %>% 
  mutate(row_dir = direction_check(row_updated),
         col_dir = direction_check(col_updated))

letters_1 %>% 
  filter(!is.na(row_dir) & !is.na(col_dir))


# Part 2 ------------------------------------------------------------------


reference <-  input %>% 
  mutate(row = row_number(),
         value = str_split(value, '')) %>% 
  unnest_longer(value, indices_to = 'col') %>% 
  select(-contains('_ref'))

letters <- input %>% 
  mutate(row = row_number(),
         value = str_split(value, '')) %>% 
  unnest_longer(value, indices_to = 'col') %>% 
  relocate(row, .before = col) %>% 
  mutate(col_ref_min = col - 1,
         col_ref_max = col + 1,
         row_ref_min = row - 1,
         row_ref_max = row + 1,
  ) %>% 
  mutate(value_updated = str_c(value), 
         row_updated = row,
         col_updated= col,
         .after = value) %>% 
  filter(value == 'A',
         
         !col %in% c(max(col),
                     min(col)),
         !row %in% c(max(row),
                     min(row)))


for (i in 1:2){
  letters <- letters %>% 
    
    left_join(reference %>% 
                filter(value %in% c('M')),
              join_by(between(y$col,
                              x$col_ref_min,
                              x$col_ref_max),
                      between(y$row,
                              x$row_ref_min,
                              x$row_ref_max)), suffix = c('', '_new')
    ) %>% 
    mutate(value_updated = str_c(value_updated,
                                 value_new), .after = value) %>% 
    filter(row != row_new,
           col != col_new)  %>%
    mutate(prev = map2(str_split(str_sub(row_updated, str_length(row) + 1), "_"), 
                       str_split(str_sub(col_updated, str_length(col) + 1), "_"),
                       ~ str_c(.x, '-', .y)),
           prev = ifelse(prev == "-",
                     NA,
                     prev),
           has_existed = map2_int(prev, 
                                  str_c(row_new, '-', col_new),
                                  ~sum(.x == .y)),
           
           diag = map2(str_split(str_sub(row_updated, 3), "_"),
                       str_split(str_sub(col_updated, 3), "_"),
                       ~ str_c(as.numeric(.x) + 2, '-', as.numeric(.y) + 2)),

           diag2 = map2(str_split(str_sub(row_updated, 3), "_"),
                       str_split(str_sub(col_updated, 3), "_"),
                       ~ str_c(as.numeric(.x) - 2, '-', as.numeric(.y) + 2)),

           diag3 = map2(str_split(str_sub(row_updated, 3), "_"),
                       str_split(str_sub(col_updated, 3), "_"),
                       ~ str_c(as.numeric(.x) + 2, '-', as.numeric(.y) - 2)),

           diag4 = map2(str_split(str_sub(row_updated, 3), "_"),
                       str_split(str_sub(col_updated, 3), "_"),
                       ~ str_c(as.numeric(.x) - 2, '-', as.numeric(.y) - 2)),
           is_diag = map2_int(diag, 
                                  str_c(row_new, '-', col_new),
                                  ~sum(.x == .y, na.rm = T)) +
             map2_int(diag2, 
                      str_c(row_new, '-', col_new),
                      ~sum(.x == .y, na.rm = T)) +
             map2_int(diag3, 
                      str_c(row_new, '-', col_new),
                      ~sum(.x == .y, na.rm = T)) +
             map2_int(diag4, 
                      str_c(row_new, '-', col_new),
                      ~sum(.x == .y, na.rm = T)),
           ) %>%
    filter(has_existed == 0 | is.na(has_existed),
           is_diag == 0 | is.na(is_diag)) %>% 
    
    mutate(row_updated = str_c(row_updated, '_', row_new),
           col_updated = str_c(col_updated, '_', col_new),
           m_rows = str_split(str_sub(row_updated, 3), "_"))  %>% 
    select(-ends_with('_new')) 
}

letters <- letters %>% select(-(diag:is_diag))

for (i in 1:2){
  letters <- letters %>% 
    
    left_join(reference %>% 
                filter(value %in% c('S')),
              join_by(between(y$col,
                              x$col_ref_min,
                              x$col_ref_max),
                      between(y$row,
                              x$row_ref_min,
                              x$row_ref_max)), suffix = c('', '_new')
    ) %>% 
    mutate(value_updated = str_c(value_updated,
                                 value_new), .after = value) %>% 
    filter(row != row_new,
           col != col_new)  %>%
    mutate(prev = map2(str_split(str_sub(row_updated, str_length(row) + 1), "_"), 
                       str_split(str_sub(col_updated, str_length(col) + 1), "_"),
                       ~ str_c(.x, '-', .y)),
           has_existed = map2_int(prev, 
                                  str_c(row_new, '-', col_new),
                                  ~sum(.x == .y)),
    ) %>%
    filter(has_existed == 0) %>% 
    
    mutate(row_updated = str_c(row_updated, '_', row_new),
           col_updated = str_c(col_updated, '_', col_new),
           m_rows = str_split(str_sub(row_updated, 3), "_"))  %>% 
    select(-ends_with('_new')) 
}


letters_x <- letters %>%  
  mutate(across(col_ref_min:col_ref_max,
                ~ str_count(col_updated, as.character(.x)), .names = '{.col}_match'),
         across(row_ref_min:row_ref_max,
                ~ str_count(row_updated, as.character(.x)), .names = '{.col}_match')) %>% 
  filter(col_ref_min_match == 2,
         row_ref_min_match == 2,
         col_ref_min_match == 2,
         row_ref_min_match == 2)

letters_x %>% 
  distinct(row, col)


letters_x
