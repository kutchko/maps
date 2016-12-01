rm(list=ls())

library(tidyverse)


setwd('~/Desktop/maps')
election.data.county <- read.csv('HelloWorldData/us-presidential-election-county-results-2004-through-2012.csv')
partisanship <- election.data.county %>%
    select(year, fips, vote_rep, vote_dem) %>%
    group_by(year) %>%
    
    mutate(TotalRep = sum(vote_rep), TotalDem = sum(vote_dem)) %>%
    mutate(VotePercCountry = 100 * (TotalDem - TotalRep) / (TotalRep + TotalDem)) %>%
    select(-c(TotalRep, TotalDem)) %>%
    mutate(VotePercFips = 100 * (vote_dem - vote_rep) / (vote_dem + vote_rep)) %>%
    mutate(CountyPartisanship = VotePercFips - VotePercCountry)

write.csv(partisanship, file='county_partisanship_2004_2012.csv', quote=FALSE,
          row.names=FALSE)




