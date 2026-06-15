
# an intuitive method that doesn't quite work ----------------------------



# direct standardization

library(tidyverse)
library(DasGuptR)

data(reconv)

head(reconv,
n = 20)

reconv |> 
  count(year)

# just first and last year

reconv_04_16 <- 
  reconv |> 
  filter(year == 2004 | year == 2016)

reconv_04_16 <- 
reconv_04_16 |> 
  group_by(year) |> 
  mutate(demog_struct = offenders / convicted_population)

# crude rates

reconv_04_16 |> 
  group_by(year) |> 
  summarise(crude_rate = weighted.mean(prev_rate, demog_struct))

# 2004 rate is 0.324, 2016 rate is 0.272

# direct standardization

weights_2004 <- 
reconv_04_16 |> 
  ungroup() |> 
  filter(year == 2004) |> 
  select(Age, Sex, demog_struct_2004 = demog_struct)

reconv_04_16 |> 
  left_join(weights_2004) |> 
  summarise(crude_rate = weighted.mean(prev_rate, demog_struct_2004))

# 2016 reconviction rate standardized to 2004 cohort is 0.288!

weights_2016 <- 
reconv_04_16 |> 
  ungroup() |> 
  filter(year == 2016) |> 
  select(Age, Sex, demog_struct_2016 = demog_struct)

reconv_04_16 |> 
  left_join(weights_2016) |> 
  summarise(crude_rate = weighted.mean(prev_rate, demog_struct_2016))

# 2004 reconviction rate standardized to 2016 cohort is 0.295!


# a kind of intuitive method that does work ------------------------------



# calculate average demographic composition

indirect_04_16 <- 
reconv_04_16 |> 
  left_join(weights_2004) |> 
  left_join(weights_2016) |> 
  mutate(demog_struct_mean = (demog_struct_2004 + demog_struct_2016) / 2) |>
  group_by(year) |> 
  summarise(crude_rate = weighted.mean(prev_rate, demog_struct_mean))

indirect_04_16 

# this is what DasGuptR is doing

# first we have to create a combined sex age variable because we standardized by all
# demographic factors

dg_04_26 <- 
reconv_04_16 |> 
  mutate(sex_age = interaction(Sex, Age)) |> 
  dgnpop(pop = "year", factors = c("prev_rate"),
  id_vars = c("sex_age"), crossclassified = "offenders"
) |> 
  dg_table()

# we can check these are the same

all.equal(
  c(dg_04_26$`2004`[[1]], dg_04_26$`2016`[[1]]), # combines the first element of the 
  # 2004 and 2006 columns
  indirect_04_16$crude_rate
)


# decomposition ----------------------------------------------------------
