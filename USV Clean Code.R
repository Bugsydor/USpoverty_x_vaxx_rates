### Import Libraries ####
library(tidyverse)
library(usmap)

### NEW PLAN ####

# Instead of answering two questions using the consistency stuff, just answer the one question about poverty. Open with a chart using all of the data ignoring
# state lines, then move into the map.

### Import Data ####
NISPUF13 <- read_rds("NISPUF13.RDS") %>%
  filter(N_PRVR > 0) %>%
  select("year" = YEAR, "fips" = STATE, INCPOV1, PU4313314, P_UTDROT_S, 
         P_UTDHEPA2)

NISPUF14 <- read_rds("NISPUF14.RDS") %>%
  filter(N_PRVR > 0) %>%
  select("year" = YEAR, "fips" = STATE, INCPOV1, PU4313314, P_UTDROT_S, 
         P_UTDHEPA2)

NISPUF15 <- read_rds("NISPUF15.RDS") %>%
  filter(N_PRVR > 0) %>%
  select("year" = YEAR, "fips" = STATE, INCPOV1, PU4313314, P_UTDROT_S, 
         P_UTDHEPA2)

NISPUF16 <- read_rds("NISPUF16.RDS") %>%
  filter(N_PRVR > 0) %>%
  select("year" = YEAR, "fips" = STATE, INCPOV1, PU4313314, P_UTDROT_S, 
         P_UTDHEPA2)

NISPUF17 <- read_rds("NISPUF17.RDS") %>%
  filter(N_PRVR > 0) %>%
  select("year" = YEAR, "fips" = STATE, INCPOV1, PU4313314, P_UTDROT_S, 
         P_UTDHEPA2)

NISPUF5Y <- bind_rows(NISPUF13, NISPUF14, NISPUF15, NISPUF16, NISPUF17)


### Poverty/Vax Overview Plot ####
poverty_by_year <- NISPUF5Y %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(year) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

lax_vax_by_year <- NISPUF5Y %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(year) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

poverty_vs_vax_overview <- left_join(poverty_by_year, lax_vax_by_year) %>%
  select(year, percent_poor, percent_vaxxed)

ggplot(data = poverty_vs_vax_overview, aes(x = year)) +
  geom_col(aes(y = percent_vaxxed), fill = "indianred4") +
  geom_text(aes(y = percent_vaxxed, label = sprintf("%0.2f", round(percent_vaxxed, digits = 2))), 
            vjust = -.3, size = 5, color = "black") +
  geom_col(aes(y = percent_poor), fill = "plum4") +
  geom_text(aes(y = percent_poor, label = sprintf("%0.2f", round(percent_poor, digits = 2))), 
            vjust = -.3, size = 5, color = "ivory") +
  theme_bw() +
  labs(
    title = "As time went on, vaccination rates increased and poverty rates decreased overall.",
    x = "Year",
    y = "Percentage (%)"
  )

### Poverty Plots ####

poverty_palette <- colorRampPalette(c('#1a9850', '#91cf60', '#d9ef8b', '#ffffbf', '#fee08b', '#fc8d59', '#d73027'))(100)

poverty_gradient <- scale_fill_gradientn(colors = poverty_palette, limits = c(5, 50))

poor_data_13 <- NISPUF13 %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

plot_usmap(data = poor_data_13, values = "percent_poor", labels = TRUE) +
  poverty_gradient +
  labs(
    title = "In 2013, Arkansas had the highest percentage of survey respondants who fell below the poverty line.",
    fill = "Poverty Rate (%)"
  )

poor_data_17 <- NISPUF17 %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

plot_usmap(data = poor_data_17, values = "percent_poor", labels = TRUE) +
  poverty_gradient +
  labs(
    title = "In 2017, most states had fewer poor respondants.",
    fill = "Poverty Rate (%)"
  )

poor_data_5Y <- NISPUF5Y %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(year) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

plot_usmap(data = poor_data_5Y, values = "percent_poor", labels = TRUE) +
  poverty_gradient

### Vax Plots ####
vax_palette <- colorRampPalette(c('#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60','#1a9850'))(200)

vax_gradient <- scale_fill_gradientn(colors = vax_palette, limits = c(20, 70))

vax_data_13 <- NISPUF13 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_13, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

vax_data_14 <- NISPUF14 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_14, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

vax_data_15 <- NISPUF15 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_15, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

vax_data_16 <- NISPUF16 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_16, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

vax_data_17 <- NISPUF17 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_17, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

vax_data_5Y <- NISPUF5Y %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S & P_UTDHEPA2,
    T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = vax_data_5Y, values = "percent_vaxxed", labels = TRUE) +
  vax_gradient

### Vax Plots (No HepA) ####
lax_vax_palette <- colorRampPalette(c('#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60','#1a9850'))(200)

lax_vax_gradient <- scale_fill_gradientn(colors = vax_palette, limits = c(45, 85))

lax_vax_data_13 <- NISPUF13 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_13, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient

lax_vax_data_14 <- NISPUF14 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_14, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient

lax_vax_data_15 <- NISPUF15 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_15, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient

lax_vax_data_16 <- NISPUF16 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_16, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient

lax_vax_data_17 <- NISPUF17 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_17, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient

lax_vax_data_5Y <- NISPUF5Y %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

plot_usmap(data = lax_vax_data_5Y, values = "percent_vaxxed", labels = TRUE) +
  lax_vax_gradient
