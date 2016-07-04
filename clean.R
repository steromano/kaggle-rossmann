setwd(sys.getenv('ROSSMANN_HOME'))
source('load.R')

read_csv('data/raw/train.csv') %>%
  set_names(tolower(names(.))) %>%
  # all stores are always closed on Sunday
  filter(dayofweek != 7) %>%
  # this is basically the same as open
  select(-stateholiday) %>%
  group_by(store) %>%
  arrange(date) %>%
  # impute sales on closing days using rolling
  # seasonal average
  mutate(
    sales = seasonal_avg_impute(
      ifelse(as.logical(open), sales, NA),
      frequency = 6, 
      order = 4
    )
  ) %>%
  ungroup %>%
  write_csv('data/clean/train_clean.csv')


read_csv('data/raw/store.csv') %>% 
  set_names(tolower(names(.))) %>%
  # set competitiondistance to a very large number
  # for stores with no competition
  mutate(competitiondistance = ifelse(
    is.na(competitiondistance),
    1000000,
    competitiondistance
  )) %>%
  write_csv('data/clean/store_clean.csv')
