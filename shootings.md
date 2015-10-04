



---
title: "Mass shooting tracker 2013-2015"
author: "Gabi Huiber"
date: "September 2, 2015"
---



```r
# LIBRARIES
# 
# All hail Hadley Wickham:
library('dplyr')
library('lubridate')
library('ggplot2')
# And the people at Trulia:
library('choroplethr')
# 
# RAW DATA
# 
# There are some inconsistencies from one year to the next, so 
# you have to munge it a bit.
myurl <- 'http://shootingtracker.com/tracker'
keep2013 <- c('date', 'killed', 'wounded', 'location')
keep2014 <- c('Date', 'Dead', 'Injured', 'Location')
y2013 <- read.csv(file = paste(myurl, 
                               '2013MASTER.csv', sep = '/'), 
                  stringsAsFactors = FALSE)[, keep2013] %>% 
  rename(Date = date, 
         Dead = killed, 
         Injured = wounded, 
         Location = location)
y2014 <- read.csv(file = paste(myurl, 
                               '2014MASTER.csv', sep = '/'), 
                  stringsAsFactors = FALSE)[, keep2014]
y2015 <- read.csv(file = paste(myurl, 
                               '2015CURRENT.csv', sep = '/'), 
                  stringsAsFactors = FALSE)[, keep2014]
#
# Next, turn it into a balanced state x date panel. First,
# correct state abbreviation where needed.
keeps <- do.call(rbind, list(y2013, y2014, y2015)) %>% 
  mutate(state = tolower(substr(Location, start = nchar(Location) - 1, 
                                stop = nchar(Location)))) %>%
  mutate(state = replace(state, which(state == 'ee'), 'tn')) %>%
  mutate(state = replace(state, which(state == 'c.'), 'dc')) %>%
  mutate(state = replace(state, which(state == 'ka'), 'ks')) %>%
  mutate(state = replace(state, which(state == 'io'), 'oh')) %>%
  mutate(state = replace(state, which(state == 'na'), 'la')) %>%
  mutate(state = replace(state, which(state == 'is'), 'il')) %>%
  mutate(state = replace(state, which(state == 'as'), 'ks')) %>%
  mutate(state = replace(state, 
                         which(Location == 'San Juan, Puerto Rico'), 
                         'pr')) %>%  
  select(-Location) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  group_by(state, date) %>% 
  summarise(dead = sum(Dead), wounded = sum(Injured)) %>%
  ungroup() 
# 
# Now calculate total days of exposure over the 3 input files
days <- min(keeps$date) + days(0:as.integer(max(keeps$date)-min(keeps$date)))
panelset <- tbl_df(expand.grid(sort(unique(keeps$state)),days)) %>%
  rename(state = Var1, date = Var2) %>% mutate(state = as.character(state))
# 
# Some states didn't have any mass shootings during this time.
quiet <- setdiff(tolower(state.abb), unique(panelset$state))
# 
# Get state population estimates. The `source()`d file below
# came from making some mods in Vim to csv file from US Census.
source('statepops')
# 
# Now you've got to fiddle with some state names, and to add PR and DC.
states <- cbind(sapply(state.name, 
                       function(x) gsub(' ','_', tolower(x))), 
                tolower(state.abb))
states <- rbind(states, 
                c('district_of_columbia', 'dc'), 
                c('puerto_rico', 'pr'))
# 
# Define a helper function for processing each of the state-level 
# pop.XXX character vectors to get yearly population figures.
getPop <- function(x) {
  put <- as.numeric(sapply(x, function(y) gsub(',','', y)))
  names(put) <- c('census','base', paste('y', c(2010:2014), sep = ''))
  put
}
pops <- data.frame(do.call(rbind, 
                lapply(states[,1], 
                       function(x) getPop(get(paste('pop', x, sep = '.'))))))
pops <- mutate(pops, state = states[,2]) %>% select(state, y2014) 
# 
# Now merge daily dead, wounded with person-days of exposure
panelset <- inner_join(panelset, pops) %>% left_join(keeps) %>% 
  replace(is.na(.), 0) %>% arrange(state, date)
# 
# By state, by year
perstateyear <- panelset %>% 
  mutate(year = year(date)) %>% 
  group_by(state, y2014, year) %>% 
  summarise(dead = sum(dead), wounded = sum(wounded), days = n()) %>% 
  ungroup() %>% 
  mutate(dead_incidence_100M = 10^8 * dead / (y2014 * days), 
         wounded_incidence_100M = 10^8 * wounded / (y2014 * days))
# 
# US-wide daily risk by year
peryear <- perstateyear %>% 
  group_by(year, days) %>% 
  summarise(population = sum(y2014), 
            dead = sum(dead), 
            wounded = sum(wounded)) %>%
  mutate(dead_incidence_100M = 10^8 * dead / (population * days), 
         wounded_incidence_100M = 10^8 * wounded / (population * days))
# 
# This seems right: the incidence per 100M person-days for the US
# is the weighted average across the states:
chk <- mean(peryear$dead_incidence_100M) == 
  sum(perstateyear$dead_incidence_100M * perstateyear$y2014) / sum(perstateyear$y2014)
# 
# So how do states compare on the daily risk of dying or 
# being wounded in a mass shooting, estimated over the 
# entire range of data?
perstate <- perstateyear %>% 
  select(state, y2014, days, dead, wounded) %>% 
  group_by(state) %>% 
  summarise(risk_dead = 10^8 * sum(dead) / sum(y2014 * days), risk_wounded = 10^8 * sum(wounded) / sum(y2014 * days)) %>%
  ungroup()
# 
# Try a choropleth map
drawMap <- function(dead = TRUE) {
  df <- select(perstate, state, risk_dead) %>%
    rename(value = risk_dead)
  if(dead == FALSE) {
    df <- select(perstate, state, risk_wounded) %>%
      rename(value = risk_wounded)
  }
  regions <- tbl_df(data.frame(states)) %>%
    select(X1, X2) %>% rename(state = X2, 
                              region = X1) %>%
    mutate(region = as.character(region)) %>%
    mutate(region = gsub('_', ' ', region)) %>%
    filter(region != 'puerto rico')
  df <- left_join(regions, df) %>% 
    select(region, value)
  df
}
risk_of_dying <- drawMap()
risk_of_being_wounded <- drawMap(FALSE)
cp_dead <- state_choropleth(risk_of_dying, 
                            title = 'Daily risk of being killed in a mass shooting, by state, cases per 100M people')
cp_wounded <- state_choropleth(risk_of_being_wounded, 
                               title = 'Daily risk of being wounded in a mass shooting, by state, cases per 100M people')
```


Shooting data from [here](http://shootingtracker.com/wiki/Main_Page) covers 
years 2013-2015 (as of this writing). If you pair it with 2014 population data from 
[this .csv file](http://www.census.gov/popest/data/state/totals/2014/tables/NST-EST2014-01.csv), you can get an idea of the daily risk of dying or being 
wounded in a mass shooting across the US, by state, as shown below:


```r
cp_dead
cp_wounded
quiet.states <- names(states[,1][as.character(states[,2]) %in% quiet])
nth <- rep(', ', length(quiet))
nth[length(nth)-1] <- ' and '
nth[length(nth)] <- '.'
```


There are 4 states that did not have a mass shooting since at least 2012. They are Hawaii, New Hampshire, North Dakota and Wyoming.
