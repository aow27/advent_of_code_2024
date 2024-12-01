# A function that will  download the input for a given day and year and input it into the 
# folder called data if it doesn't exist already, and then creates a tibble called input within the global environment 


# Requires knowing the AoC session cookie, which is typically found in the storage section of a web inspector
# I've then saved this in my Rprofile file which isn't publicly shared

download_advent <- function(year,
                            day,
                            read_in = TRUE,
                            sample = FALSE,
                            funct,
                            ...){
  
  if(file.exists(glue::glue('data/input{year}_{day}.txt'))){
    message('This file has already been downloaded from the AoC servers - no need to download again
            The stored file in the folder has been read in as a tibble called input.')
    
  } else {
    if(!file.exists('cookie.txt')){
      message("The cookie input isn't saved, please make sure you have a cookie saved in a text file")
      
      break
    }
    
    cookie <- read_lines('cookie.txt')
    
    glue::glue('curl "https://adventofcode.com/{year}/day/{day}/input" -H "cookie: session={cookie}" -o "data/input{year}_{day}.txt" 2>/dev/null') %>% 
      system()
    
    message(glue::glue('A file at "data/input{year}_{day}.txt" has now been created - enjoy!
            The new file in the folder has been read in as a tibble called input.'))
    
  }
  
  if(exists('cookie')) rm(cookie, envir = .GlobalEnv)
  
  input <- funct(glue::glue('data/input{year}_{day}.txt'), ...)
  
  if(read_in) input <<- input
  
  if(sample) input
}
