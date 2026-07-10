library(tidyverse)

# data per table 2 in https://journals.sagepub.com/doi/10.1177/1536867X1701700213

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
  

write_csv(clogg_long,
here::here("data", "example2-clogg-long.csv"))