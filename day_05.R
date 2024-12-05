library(tidyverse)

input <- read_lines('data/input_05.txt') %>% 
  as_tibble()


df <- input %>% 
  slice(1:(which(value == "")-1)) %>% 
  mutate(rule = row_number(),
         page = str_split(value, '\\|')) %>% 
  unnest_longer(page, indices_to = 'order') %>% 
  mutate(value_updated = value,
         first_page = str_sub(value, 1,2),
         pages = str_split(value_updated, '\\|'),
         .after = value)


order <- df

# order_save <- order

order <- order %>% 
  filter(order == 2) %>% 
  left_join(df %>% 
              select(-value_updated) %>% 
              filter(order == 1), 
            by = join_by(x$page_recent == y$page),
            suffix = c('', '_new'),
            relationship = 'many-to-many') %>% 
  mutate(page_recent = str_sub(value_new, 4), 
         rule = str_c(rule, "_", rule_new),
         
         has_existed = map2_int(pages, 
                                page_recent,
                                ~sum(.x == .y)),
         .after = pages) %>% 
  filter(has_existed <= 1) %>% 
  
  
  mutate(value_updated = str_c(value_updated,'|', page_recent),
         pages = str_split(value_updated, '\\|'),
         mid_pages = map(pages,
                         function(x) x[2:length(x)]),
         .after = pages
  ) %>% 
  select(!contains('_new'))






page_orders <- order %>% 
  select(pages) %>% 
  unnest_longer(pages) %>% 
  as_vector() %>%  unname()


direction_check <- function(list,
                            page_orders
){ 
  
  position <- map(list, 
                  ~ match(.x, page_orders))
  
  
  diff <- map(position,
              ~ .x[2:(length(.x))] - .x[1:(length(.x) - 1)])
  
  
  diff %>% map_dbl( ~ ifelse(all(.x > 0), 1, 
                             ifelse(all(.x == 0), 0, 
                                    ifelse(all(.x < -0), -1, NA))))
  
}


update <- 
  df <- input %>% 
  slice_tail((which(value == "")+1))  %>% 
  
  mutate(list = str_split(value, ","),
         diff = direction_check(list, 
                                page_orders))
