#+ setup, include=FALSE
library(knitr)
opts_chunk$set(eval = FALSE, warning = FALSE, message = FALSE)
#+ echo = TRUE, indent = '    '
#' ---
#' title: "Mass shooting tracker 2013-2015"
#' author: "Gabi Huiber"
#' date: "September 2, 2015"
#' ---
#' 
#' LIBRARIES
#' 
#' All hail Hadley Wickham
library('dplyr')
library('lubridate')
library('ggplot2')
#' 
#' RAW DATA
#' 
#' Download link [here](http://shootingtracker.com/wiki/Main_Page).
y2013 <- read.csv(file = 'data/2013MASTER.csv', stringsAsFactors = FALSE)[ ,c('date', 'killed', 'wounded', 'location')] %>% 
  rename(Date = date, Dead = killed, Injured = wounded, Location = location)
y2014 <- read.csv(file = 'data/2014MASTER.csv', stringsAsFactors = FALSE)[,c('Date', 'Dead', 'Injured', 'Location')]
y2015 <- read.csv(file = 'data/2015CURRENT.csv', stringsAsFactors = FALSE)[,c('Date', 'Dead', 'Injured', 'Location')]
#'
#' Turn it into a balanced state x date panel
keeps <- do.call(rbind, list(y2013, y2014, y2015)) %>% 
  mutate(state = tolower(substr(Location, start = nchar(Location) - 1, stop = nchar(Location)))) %>%
  mutate(state = replace(state, which(state == 'ee'), 'tn')) %>%
  mutate(state = replace(state, which(state == 'c.'), 'dc')) %>%
  mutate(state = replace(state, which(state == 'ka'), 'ks')) %>%
  mutate(state = replace(state, which(state == 'io'), 'oh')) %>%
  mutate(state = replace(state, which(state == 'na'), 'la')) %>%
  mutate(state = replace(state, which(state == 'is'), 'il')) %>%
  mutate(state = replace(state, which(state == 'as'), 'ks')) %>%
  mutate(state = replace(state, which(Location == 'San Juan, Puerto Rico'), 'pr')) %>%  
  select(-Location) %>%
  group_by(Date, state) %>% summarise(dead = sum(Dead), wounded = sum(Injured)) %>%
  ungroup() %>% rename(date = Date)
panelset <- keeps %>% 
  tidyr::expand(state, date)
#' 
#' States that didn't have a mass shooting during this time
setdiff(tolower(state.abb), unique(panelset$state))
#' 
#' 

