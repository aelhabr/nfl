

load("data/external/bigfour_public.rda")
names(bigfour_public)
typeof(bigfour_public)

library("dplyr")
lapply(bigfour_public, pull, unique, sort)

library("lubridate")
bigfour_2 <-
  bigfour_public %>% 
  mutate(
    yyyy = year(gameDate),
    mm = month(gameDate),
    dd = day(gameDate)
  ) %>% 
  arrange(yyyy, mm, dd)

bigfour_nfl <-
  bigfour_public %>% 
  filter(sport == "nfl")

bigfour_nfl %>% 
  mutate(rank_fav = row_number(desc(p_home))) %>% 
  arrange(desc(rank_fav)) %>% 
  top_n(-10, rank_fav)

bigfour_tms <- bigfour_nfl %>% pull(home_team) %>% sort() %>% unique()
write.csv(bigfour_tms, "bigfour_tms.csv", row.names = FALSE)

bigfour_nfl %>% 
  filter(home_team == "Los Angeles Rams") %>% 
  arrange(gameDate)

# load("data/external/nflparity.rda")
class(bigfour_public$gameDate)
