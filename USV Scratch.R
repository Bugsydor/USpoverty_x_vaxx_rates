# It may be useful to compare vaccination rates across different vaccines
# on a map. Then if they're mostly the same, just pick one. If they differ,
# find out why. Do some of them vary significantly with poverty level?
# With geographic location?

library(tidyverse)

US_Immunization_Survey_2017 <- read_rds("US_Immunization_Survey_2017.rds")

pruned_US_Immunization_2017 <- US_Immunization_Survey_2017 %>%
  select(YEAR, CEN_REG, EDUC1, INCPORAR, INCPORAR_I, INCPOV1, INCQ298A,
         RENT_OWN, EST_GRANT, STATE, D6R, D7, N_PRVR, 104:133, 452:453) %>%
# mutate state codes into state abbreviations
  mutate(State_Ab = case_when(
    STATE == 1 ~ "AL",
    STATE == 2 ~ "AK",
    STATE == 4 ~ "AZ",
    STATE == 5 ~ "AR",
    STATE == 6 ~ "CA",
    STATE == 8 ~ "CO",
    STATE == 9 ~ "CT",
    STATE == 10 ~ "DE",
    STATE == 11 ~ "DC",
    STATE == 12 ~ "FL",
    STATE == 13 ~ "GA",
    STATE == 15 ~ "HI",
    STATE == 16 ~ "ID",
    STATE == 17 ~ "IL",
    STATE == 18 ~ "IN",
    STATE == 19 ~ "IA",
    STATE == 20 ~ "KS",
    STATE == 21 ~ "KY",
    STATE == 22 ~ "LA",
    STATE == 23 ~ "ME",
    STATE == 24 ~ "MD",
    STATE == 25 ~ "MA",
    STATE == 26 ~ "MI",
    STATE == 27 ~ "MN",
    STATE == 28 ~ "MS",
    STATE == 29 ~ "MO",
    STATE == 30 ~ "MT",
    STATE == 31 ~ "NE",
    STATE == 32 ~ "NV",
    STATE == 33 ~ "NH",
    STATE == 34 ~ "NJ",
    STATE == 35 ~ "NM",
    STATE == 36 ~ "NY",
    STATE == 37 ~ "NC",
    STATE == 38 ~ "ND",
    STATE == 39 ~ "OH",
    STATE == 40 ~ "OK",
    STATE == 41 ~ "OR",
    STATE == 42 ~ "PA",
    STATE == 44 ~ "RI",
    STATE == 45 ~ "SC",
    STATE == 46 ~ "SD",
    STATE == 47 ~ "TN",
    STATE == 48 ~ "TX",
    STATE == 49 ~ "UT",
    STATE == 50 ~ "VT",
    STATE == 51 ~ "VA",
    STATE == 53 ~ "WA",
    STATE == 54 ~ "WV",
    STATE == 55 ~ "WI",
    STATE == 56 ~ "WY",
    TRUE ~ "ZZ"
  ))


# trying out usmap library
library(usmap)

# plot the 50 states
plot_usmap(regions = "states")

# plot the western states with population data
plot_usmap(
  data = statepop, values = "pop_2015", 
  include = c("CA", "ID", "NV", "OR", "WA"),
  lines = "red"
) +
  scale_fill_continuous(
    low = "white", high = "blue", name = "Population (2014)", 
    label = scales::comma
  ) +
  labs(
    title = "Western US States", 
    subtitle = "These are the states in the Pacific Timezone.") +
  theme(legend.position = "left")

# group by state
US_Immunization_2017_by_state <- pruned_US_Immunization_2017 %>%
  filter(N_PRVR > 0 & State_Ab != "ZZ") %>%
  count(State_Ab) %>%
  rename("state" = State_Ab)

plot_usmap(data = US_Immunization_2017_by_state, values = "n", labels = TRUE) +
  scale_fill_continuous(
    low = "white",
    high = "orange"
  )

# The above code gets me a plot showing how many observations in the data
# came from each state. Washington is quite overrepresented, while
# a few [4] states aren't represented at all.

# WRONG. I was using NCHS state codes instead of the proper FIPS codes.
# That has been fixed now, and there are now blank states.


# Let's try making a states plot with the line color determined by
# the percentage of poor people:

poor_data <- pruned_US_Immunization_2017 %>%
  filter(N_PRVR > 0 & State_Ab != "ZZ") %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  rename("state" = State_Ab) %>%
  group_by(state) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

plot_usmap(data = poor_data, values = "percent_poor", labels = TRUE) +
  scale_fill_continuous(
    low = "green",
    high = "red"
  )

# Looks like New Mexico, Arkansas, and Mississippi have the highest
# number of poor people per capita, by the definitions used by the CCD.

rich_data <- pruned_US_Immunization_2017 %>%
  filter(N_PRVR > 0 & State_Ab != "ZZ") %>%
  mutate(is_rich = ifelse(INCPOV1 == 1, T, F)) %>%
  rename("state" = State_Ab) %>%
  group_by(state) %>%
  count(is_rich) %>%
  spread(key = is_rich, value = n) %>%
  rename("rich" = "TRUE", "not_rich" = "FALSE") %>%
  mutate(percent_rich = rich / (rich + not_rich) * 100)

plot_usmap(data = rich_data, values = "percent_rich", labels = TRUE) +
  scale_fill_continuous(
    low = "red",
    high = "green"
  )

### SALIENT VACCINATION DATA ####

# PU4313314, P_UTDROT_S, P_UTDHEPA2. COmbine these three into a single
# mutate() flag to flag observations that are fully vaccinated.

vax_data <- pruned_US_Immunization_2017 %>%
  filter(N_PRVR > 0 & State_Ab != "ZZ") %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S,
    T, F)) %>%
  rename("state" = State_Ab) %>%
  group_by(state) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data, values = "percent_vaxxed", labels = TRUE) +
  scale_fill_continuous(
    high = "green",
    low = "red"
  )

vax_data <- pruned_US_Immunization_2017 %>%
  filter(N_PRVR > 0 & State_Ab != "ZZ") %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S,
    T, F)) %>%
  rename("fips" = STATE) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data, values = "percent_vaxxed", labels = TRUE) +
  scale_fill_continuous(
    high = "green",
    low = "red"
  )

## FINDINGS: ####
# At least in 2017, there was no significant link between a state's
# relative poverty rate and its vaccination rate. The same is true of
# rates of households earning > $75k annually and vaccination. While
# there are some cases of overlap between high poverty rate and low
# vaccination rate (for example, Mississippi is among the worst in both
# cases), there are also many states where such apparent correlation is
# reversed.

# It is also apparent that the Hepatitis A vaccine is by far less popular
# than the rest of the body of recommended childhood vaccines, despite
# occupying a space on the World Health Organization's List of Essential
# Medicines and being recommended by the CDC for all children. Removing
# it from the mix of vaccines to check the data for raises apparent
# vaccination rates across the board to the point that the top of the old
# default scale (approx. 60%) becomes the bottom of the new scale.


### More ideas ####

# Group data by state, then summarize into means etc.?

# Might be useful to create a binary/trinary column to flag whether (or to
# what degree) a child has been vaccinated. Would involve a complex
# conditional statement taking into account several columns, since there's
# a lot of overlap...

# Alternatively, it might be valuable to combine several of the UTD 
# flags into a single flag.

# Check to see if states' vax concentrations vary by year.