
# an intuitive method that doesn't quite work ----------------------------



# direct standardization

library(tidyverse)
library(DasGuptR)

data(reconv)

head(reconv,
n = 20)

# from the manual and our vignette
# Mean earnings as product of two factors for 
# Black males and White males 18 years and over, US 1980


eg2.1 <- data.frame(
  pop = c("Black", "White"),
  avg_earnings = c(10930, 16591),
  earner_prop = c(.717892, .825974)
)

# Crude Rate = total earnings / total population
# avg_earnings = total earnings / persons who earned
# earner_prop = persons who earned / total population

eg2.1 |> 
  group_by(pop) |> 
  summarise(crude_rate = avg_earnings * earner_prop)

# crude average earnings is 7847 for Black men and 13704 for White men

# direct standardization -------------------------------------------------
# standardizing by the proportion of men in employment

weights_White <- 
eg2.1 |> 
  filter(pop == "White") |> 
  select(earner_prop_White = earner_prop)

weights_Black <- 
eg2.1 |> 
  filter(pop == "Black") |> 
  select(earner_prop_Black = earner_prop)


eg2.1 |> 
  group_by(pop) |> 
  summarise(crude_rate = avg_earnings * weights_White$earner_prop_White)

# standardizing by White male employment rate the average earnings for Black men is 9028

eg2.1 |> 
  group_by(pop) |> 
  summarise(crude_rate = avg_earnings * weights_Black$earner_prop_Black)

# standardizing by Black male employment rate the average earnings for White men is 11911

eg2.1 |> 
  mutate(earner_prop_Black = weights_Black$earner_prop_Black,
  earner_prop_White = weights_White$earner_prop_White) |> 
  mutate(earner_prop_mean = (earner_prop_Black + earner_prop_White) / 2) |> 
  group_by(pop) |> 
  summarise(crude_rate_mean = avg_earnings * earner_prop_mean)



# decomposition by hand --------------------------------------------------

# calculate the average earnings standardized rates as before

weights_White <- 
eg2.1 |> 
  filter(pop == "White") |> 
  select(earner_prop_White = earner_prop,
  avg_earnings_White = avg_earnings)

weights_Black <- 
eg2.1 |> 
  filter(pop == "Black") |> 
  select(earner_prop_Black = earner_prop,
  avg_earnings_Black = avg_earnings)

# this code is very awkward!

manual_eg2.1 <-
  eg2.1 |>
  mutate(
    earner_prop_Black = weights_Black$earner_prop_Black,
    earner_prop_White = weights_White$earner_prop_White,
    avg_earnings_Black = weights_Black$avg_earnings_Black,
    avg_earnings_White = weights_White$avg_earnings_White
  ) |>
  mutate(
    earner_prop_mean = (earner_prop_Black + earner_prop_White) / 2,
    avg_earnings_mean = (avg_earnings_Black + avg_earnings_White) / 2
  ) |>
  group_by(pop) |>
  summarise(
    crude_rate = avg_earnings * earner_prop,
    earner_prop_std_rate = avg_earnings * earner_prop_mean,
    avg_earnings_std_rate = avg_earnings_mean * earner_prop
  ) |>
  summarise(
    crude_diff = crude_rate[pop == "Black"] - crude_rate[pop == "White"],
    earner_prop_std_rate_diff = earner_prop_std_rate[pop == "Black"] -
      earner_prop_std_rate[pop == "White"],
    avg_earnings_std_rate_diff = avg_earnings_std_rate[pop == "Black"] -
      avg_earnings_std_rate[pop == "White"]
  ) |>
  mutate(
    earner_prop_std_rate_decomp = earner_prop_std_rate_diff / crude_diff * 100,
    avg_earnings_std_rate_decomp = avg_earnings_std_rate_diff / crude_diff * 100
  )

manual_eg2.1

# with DasGuptR ----------------------------------------------------------

dg_eg2.1 <- 
dgnpop(eg2.1, pop = "pop", factors = c("avg_earnings", "earner_prop")) |>
  dg_table()

dg_eg2.1

all.equal(
  round(
    c(
      manual_eg2.1$earner_prop_std_rate_decomp[[1]],
      manual_eg2.1$avg_earnings_std_rate_decomp[[1]]
    ),
    2
  ),
  dg_eg2.1$decomp[1:2]
)


