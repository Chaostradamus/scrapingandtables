library(tidyverse)
library(readr)
library(dplyr)
library(rvest)
library(RCurl)
library(XML)
library(gganimate)

##reading in jaylen browns table and adding player column

jb = "https://www.basketball-reference.com/players/b/brownja02.html"

jb_page = read_html(jb)

jbtable <- jb_page %>% html_nodes("table") %>% 
  html_table() %>% . [[1]]

jbtable2 <- jbtable %>% 
  mutate(player = "Jaylen Brown")

## reading in jayson tatum's table

tatum = "https://www.basketball-reference.com/players/t/tatumja01.html"

tatum_page = read_html(tatum)


tatumtable <- tatum_page %>% html_nodes("table") %>% 
  html_table() %>%  . [[1]]

tatumtable2 <- tatumtable %>% 
  mutate(player = "Jayson Tatum")

##reading in tracy mcgrady's table

lonzo = "https://www.basketball-reference.com/players/b/balllo01.html"

lonzo_page = read_html(lonzo)


lonzotable <- lonzo_page %>% html_nodes("table") %>% 
  html_table() %>%  . [[1]]

lonzotable2 <- lonzotable %>% 
  mutate(player = "Lonzo Ball")

## Steph's table

steph = "https://www.basketball-reference.com/players/c/curryst01.html"

steph_page = read_html(steph)


stephtable <- steph_page %>% html_nodes("table") %>% 
  html_table() %>%  . [[1]]

stephtable2 <- stephtable %>% 
  mutate(player = "Stephen Curry")

## Brandon Ingram table


ingram = "https://www.basketball-reference.com/players/i/ingrabr01.html"

ingram_page = read_html(ingram)


ingramtable <- ingram_page %>% html_nodes("table") %>% 
  html_table() %>%  . [[1]]

ingramtable2 <- ingramtable %>% 
  mutate(player = "Brandon Ingram")

## deangelo russell table


dlo = "https://www.basketball-reference.com/players/r/russeda01.html"

dlo_page = read_html(dlo)


dlotable <- dlo_page %>% html_nodes("table") %>% 
  html_table() %>%  . [[1]]

dlotable2 <- dlotable %>% 
  mutate(player = "Deangelo Rusell")

### deleting career row to clean up data
jbtable3 <- jbtable2[-c(6), ]  
tatumtable3 <- tatumtable2[-c(5),]
stephtable3 <- stephtable2[-c(13),]
lonzotable3 <- lonzotable2[-c(5, 6, 7, 8),]
ingramtable3 <- ingramtable2[-c(6, 7, 8, 9),]
dlotable3 <- dlotable2[-c(9, 10, 11, 12, 13, 14),]





totalstable <- jbtable3 %>% 
  full_join(stephtable3) %>% 
  full_join(tatumtable3) %>% 
  full_join(lonzotable3) %>% 
  full_join(ingramtable3) %>% 
  full_join(dlotable3)

mixedtable <- totalstable %>% 
  ggplot(aes(x = Season, y = PTS, color = player)) +
  geom_point()


totalstable %>% 
  select(PTS, player, Season, Age) %>% 
  ggplot(aes(x = Age, y = PTS, color = player)) +
  geom_line(size = 1.5) +
  transition_reveal(Age) +
  labs(x = "Age of player",
       y = "Average points per game",
       title = "Player points per game at specific age w/ gganimate")

totalstable %>% 
  ggplot(aes(x = Age, y = PTS, color = player))+
  geom_line(size = 2) +
  facet_wrap(~ player) +
  labs(x = "Age of player",
       y = "Average points per game",
       title = "Player points per game at specific age in facet wrap to show individuals")


totalstable %>%
  ggplot(aes(x = Age, y = PTS, color = player)) +
  geom_boxplot() +
  labs(x = "Age of player",
       y = "Average points per game",
       title = "Player points per game at specific age in boxplot to show median, min, max")


