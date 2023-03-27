## Clean data merge
library(tidyverse)
library(here)
library(haven)

# Read in the data
covid <- read_dta(here("data", "covid.dta"))
noncovid <- read_dta(here("data", "noncovid.dta"))

# Construct a junction table corresponding to
# age and age0 coding.
# age_grps <- c(0, 1, 5, seq(15, 85, 10))
#
#
# state_agegrps <- expand_grid(
#   state = unique(noncovid$state),
#   age0 = unique(covid$age0)
# )
#
# age_tbl <- tibble(
#   age = c(0:100),
#   age0 = rep(age_grps, c(1, 4, rep(10, 8), 16))
# )
#
# junction_tbl <- full_join(state_agegrps, age_tbl, by = "age0")


# Join to the  table by age0
alldata <- full_join(covid, noncovid,
                     by = c("state", "age0"),
                     relationship = "many-to-many") |>
  # Drop where there are no covid deaths at a specific age-state-time
  filter(!is.na(covid)) |>
  relocate(age, .before = covid) |>
  arrange(year, state, month, age) |>
  # fill pop19 and uspop downwards for ages 85>
  # These will be NA otherwise
  fill(pop19, uspop)

alldata


## Sanity-checking
alldata |>
  # All pandemic, all months, whole country
  filter(level == 0, year == 1900, state == "US") |>
  select(state, age0, age, covid, dx, ex, pop19,
         uspop, totdead, pdead) |>
  print(n = Inf)



alldata |>
  # By year, for 2022, whole country
  filter(level == 0, year == 1900, state = "US") |>
  select(state, age0, age, covid, dx, ex, pop19,
         uspop, totdead, pdead) |>
  print(n = Inf)


alldata |>
  # By year, for 2022, New York
  filter(level == 0, year == 1900, state == "NY") |>
  select(state, age0, age, covid, dx, ex, pop19,
         uspop, totdead, pdead) |>
  print(n = Inf)

save(alldata, file = here("out", "alldata.Rdata"))
write_csv(alldata, here("out", "alldata.csv"))

