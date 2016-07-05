setwd(sys.getenv('ROSSMANN_HOME'))
source('load.R')

train_clean <- 
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
  ungroup

store_clean <- 
  read_csv('data/raw/store.csv') %>% 
  set_names(tolower(names(.))) %>%
  # set competitiondistance to a very large number
  # for stores with no competition
  mutate(competitiondistance = ifelse(
    is.na(competitiondistance),
    1000000,
    competitiondistance
  )) %>%
  # add daily sales historical average for each store
  inner_join(
    train_clean %>%
      group_by(store) %>%
      summarise(sales_hist_avg = mean(sales, na.rm = TRUE))
  )

test_clean <-
  read_csv('data/raw/test.csv') %>%
  set_names(tolower(names(.)))


write_csv(train_clean, 'data/clean/train_clean.csv')
write_csv(store_clean, 'data/clean/store_clean.csv')
write_csv(test_clean, 'data/clean/test_clean.csv')
