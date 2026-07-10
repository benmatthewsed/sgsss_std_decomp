library(tidyverse)
library(rsample)
library(DasGuptR)

clogg <- read_csv(here::here("data", "example2-clogg.csv"))

clogg_long <- 
clogg |> 
  filter(`Age groups` != "All ages") |> 
   pivot_longer(
    cols = -`Age groups`,
    names_to = c("parity", ".value"),
    names_pattern = "parity(\\d+)_(.*)"
  ) |> 
  rename(age_groups = `Age groups`)
  
original_res <- 
DasGuptR::dgnpop(
  clogg_long,
  pop = "parity",
       factors = c("rate"),
      id_vars = "age_groups",
      crossclassified = "size") |> 
  DasGuptR::dg_table()

write_csv(clogg_long,
here::here("data", "example2-clogg-long.csv"))



library(rsample)

set.seed(12345)

n_draws <- 1000


clogg_uncount <- 
clogg_long |> 
  uncount(size, .remove = FALSE) |> # we need to keep the size variable to set .remove as FALSE
  group_by(age_groups, parity) |> 
  mutate(
    d = as.integer(row_number() <= round(rate * size / 100))
  ) |> 
  ungroup()

# DG wrapper

dg_clogg <- function(clogg_df){

DasGuptR::dgnpop(
  clogg_df,
  pop = "parity",
       factors = c("rate"),
      id_vars = "age_groups",
      crossclassified = "size") |> 
  DasGuptR::dg_table() |> 
  rownames_to_column() # this will be helpful later!

}


## re-aggregate the data

agg_clogg <- function(splits){
  
  rsample::analysis(splits) |> 
      count(age_groups, parity, d, name = "size") |> # this is the opposite of uncount()
  pivot_wider(names_from = "d", # turns the dataset wider to make the next step easier
                values_from = "size") |> 
  mutate(size = `1` + `0`,
  rate = `1` / size * 100) |> 
  select(age_groups, parity, size, rate) # keep only the columns we need
  
}

clogg_uncount |> 
  count(age_groups, parity, d, name = "size") |> # this is the opposite of uncount()
  pivot_wider(names_from = "d", # turns the dataset wider to make the next step easier
                values_from = "size") |> 
  mutate(size = `1` + `0`,
  rate = `1` / size * 100) |> 
  select(age_groups, parity, size, rate)


# bootstrap the individual data

clogg_straps <- 
rsample::bootstraps(
  clogg_uncount, times = 100
) |> 
  mutate(clogg_agg = map(splits, agg_clogg))

#clogg_boot_res <- 
clogg_straps |> 
  mutate(results = map(clogg_agg, dg_clogg)) |> 
  select(id, results) |> 
  unnest(results) |> 
 # filter(is.na(diff))
  group_by(rowname) |> 
  summarise(se_diff = sd(diff)) 


original_res |> 
  rownames_to_column() |> 
  left_join(clogg_boot_res) |> 
  mutate(conf_low = diff - 1.96 * se_diff,
  conf_upp = diff + 1.96 * se_diff)

# you don't have to use a normal approximation!

clogg_straps |> 
  mutate(results = map(clogg_agg, dg_clogg)) |> 
  select(id, results) |> 
  unnest(results) |> 
 # filter(is.na(diff))
  group_by(rowname) |> 
  summarise(estimate = quantile(diff, c(0.025, 0.5, 0.975), na.rm = TRUE),
ci = c(0.025, 0.5, 0.975)) |> 
  pivot_wider(id_cols = rowname,
  names_from = ci,
values_from = estimate)

