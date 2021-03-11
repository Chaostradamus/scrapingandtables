library(tidyverse)
library(baseballr)
devtools::install_github("BillPetti/baseballr")

d1_schools <- master_ncaa_team_lu %>%
  dplyr::filter(division == 1, year == 2021)

safe_ncaa_scrape <- purrr::safely(ncaa_scrape)

ncaa_scraper <- function(schoolid, school, type) {
  if (type == "batting") {
    message(paste("Getting batting stats for", school))
    
    stats <- safe_ncaa_scrape(teamid = schoolid, year = 2021, type = "batting")
  } else {
    message(paste("getting pitching stats for", school))
  
    stats <- safe_ncaa_scrape(teamid = schoolid, year = 2021, type = "pitching")                          
  }
  
  Sys.sleep(sample(seq(.005,.02,.001),1))
  
  return(stats)
}

batting_stats <- 1:nrow(d1_schools) %>%
  purrr::map(function(x) ncaa_scraper(d1_schools$school_id[x], 
                                      d1_schools$school[x],
                                      type = "batting"))


pitching_stats <- 1:nrow(d1_schools) %>%
  purrr::map(function(x) ncaa_scraper(d1_schools$school_id[x], 
                                      d1_schools$school[x],
                                      type = "pitching"))

d1_batting_stats <- batting_stats %>%  map("result") %>% 
  bind_rows()

d1_pitching_stats <- pitching_stats %>%  map("result") %>% 
  bind_rows()

d1_pitching_stats <- d1_pitching_stats %>% 
  dplyr::filter(str_detect(Player, "(Totals)") == FALSE,
                str_detect(Player, "(opponent Totals)") == FALSE)

readr::write_csv(d1_batting_stats, "d1_batting_0308.csv")

readr::write_csv(d1_pitching_stats, "d1_pitching_0308.csv")