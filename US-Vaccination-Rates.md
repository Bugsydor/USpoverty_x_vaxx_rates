---
title: "US Vaccination Rates"
author: "Ben Bragg"
date: "August 12, 2019"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
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
```


## Introduction

According to the [Centers for Disease Control and Prevention](https://www.cdc.gov/vaccines/schedules/easy-to-read/child-easyread.html), children between 0-6 years of age are recommended to receive a suite of vaccinations. This includes Hepatitis B (HepB), Rotavirus (RV), Diptheria Tetanus and Pertusis (DTaP), Haemophilus Influenzae Type B (Hib), Pneumococcal (PCV13), Polio (IPV), Measles Mumps and Rubella (MMR), Chicken Pox (Varicella), and Hepatitis A (HepA). That's the recommended list for life in the US, though additional vaccines may be advisable if one plans to leave the country. (Annual flu shots are also recommended by the CDC.)

But not everyone in the US follows these recommendations, for one reason or another. Some children have genetically compromised immune systems, and cannot be vaccinated at all. Others might not have access to healthcare due to poverty, lack of infrastructure, or sheer remoteness. Others still have parents among the infamous "Anti-Vax" movement that has been cropping up in people's news feeds lately.

The question I would like to answer is, is there a link between a state's poverty rate and its vaccination rate?


## The Data

The data I used came from the CDC's [National Immunization Surveys](https://www.cdc.gov/nchs/nis/data_files.htm) from 2013 to 2017. More information will be provided in the Works Cited section at the end.


```r
### Overview Data ####
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

### Poverty Data ####

poor_data_13 <- NISPUF13 %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

poor_data_17 <- NISPUF17 %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)

poor_data_5Y <- NISPUF5Y %>%
  mutate(is_poor = ifelse(INCPOV1 == 3, T, F)) %>%
  group_by(fips) %>%
  count(is_poor) %>%
  spread(key = is_poor, value = n) %>%
  rename("poor" = "TRUE", "not_poor" = "FALSE") %>%
  mutate(percent_poor = poor / (poor + not_poor) * 100)


### Vax Data (No HepA) ####

lax_vax_data_13 <- NISPUF13 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

lax_vax_data_14 <- NISPUF14 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

lax_vax_data_15 <- NISPUF15 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

lax_vax_data_16 <- NISPUF16 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

lax_vax_data_17 <- NISPUF17 %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)

lax_vax_data_5Y <- NISPUF5Y %>%
  mutate(full_vax = ifelse(
    PU4313314 == 1 & P_UTDROT_S, T, F)) %>%
  group_by(fips) %>%
  count(full_vax) %>%
  spread(key = full_vax, value = n) %>%
  rename("fully_vaccinated" = "TRUE", "not_fully_vaccinated" = "FALSE") %>%
  mutate(percent_vaxxed = fully_vaccinated / 
           (not_fully_vaccinated + fully_vaccinated) * 100)
```


## Poverty vs. Vaccination


```r
# Prepare palettes for plotting
poverty_palette <- colorRampPalette(c('#1a9850', '#91cf60', '#d9ef8b', '#ffffbf', '#fee08b', '#fc8d59', '#d73027'))(100)

poverty_gradient <- scale_fill_gradientn(colors = poverty_palette, limits = c(5, 50))

lax_vax_palette <- colorRampPalette(c('#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60','#1a9850'))(200)

lax_vax_gradient <- scale_fill_gradientn(colors = lax_vax_palette, limits = c(45, 85))
```


### Overview

Let's start by getting a bird's-eye view of things:


```r
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
```

![](Semester-Project-Report_files/figure-html/overview_plot-1.png)<!-- -->

That's what we see when the data from all fifty states are lumped together by year in a single convenient chart: A weak inverse relationship between poverty rate and vaccination rate. But is there an actual relationship when we zoom in, and look at things on a state-by-state basis?


### State-by-State

Are poor areas inherently any more or less likely to have higher or lower vaccination rates? Let's see what the maps say...


```r
plot_usmap(data = poor_data_13, values = "percent_poor", labels = TRUE) +
  poverty_gradient +
  labs(
    title = "In 2013, Arkansas had the highest percentage of survey respondants who fell below the poverty line.",
    fill = "Poverty Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 2013 comparison-1.png)<!-- -->

```r
plot_usmap(data = lax_vax_data_13, values = "percent_vaxxed", labels = FALSE) +
  lax_vax_gradient +
  labs(
    title = "In the same year, the vaccination rate map looks rather different.",
    fill = "Vaccination Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 2013 comparison-2.png)<!-- -->

As you can see, the two maps do not match up. Some green states turn red, some red states turn green, some states stay more-or-less the same color (Virginia and Arkansas in particular), etc. Does this pattern (or really, lack thereof) hold in 2017?


```r
plot_usmap(data = poor_data_17, values = "percent_poor", labels = TRUE) +
  poverty_gradient +
  labs(
    title = "2017 Poverty Levels",
    fill = "Poverty Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 2017 comparison-1.png)<!-- -->

```r
plot_usmap(data = lax_vax_data_17, values = "percent_vaxxed", labels = FALSE) +
  lax_vax_gradient +
  labs(
    title = "2017 Vaccination Rates",
    subtitle = "Second Verse, Same as the First",
    fill = "Vaccination Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 2017 comparison-2.png)<!-- -->

In a word, yes. While poverty rates generally declined across the US and vaccination rates generally rose, how they did so appears to be completely unrelated, and a similar lack of pattern between each state's poverty rate and vaccination rate continues from 2013.

To further hammer this point, let us examine the same maps, but with data aggregated across the five-year period.


```r
plot_usmap(data = poor_data_5Y, values = "percent_poor", labels = TRUE) +
  poverty_gradient +
  labs(
    title = "Five-Year Poverty Levels",
    fill = "Poverty Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 5Y comparison-1.png)<!-- -->

```r
plot_usmap(data = lax_vax_data_5Y, values = "percent_vaxxed", labels = FALSE) +
  lax_vax_gradient +
  labs(
    title = "Five-Year Vaccination Rates",
    subtitle = "Second Verse, Same as the First",
    fill = "Vaccination Rate (%)"
  )
```

![](Semester-Project-Report_files/figure-html/plots – 5Y comparison-2.png)<!-- -->


## Conclusions

In the United States of America, I can conclude that there is no direct link, whether directly or inversely related, between a state's poverty rate and its vaccination rate. Furthermore, there was no link between a change in poverty rate and a change in vaccination rate.


## Works Cited

Centers for Disease Control and Prevention. Datasets and Related Documentation for the National Immunization Survey - Child, 2010–2014. https://www.cdc.gov/nchs/nis/data_files.htm. 2019.

Centers for Disease Control and Prevention. NIS-Child Data and Documentation for 2015 to Present. https://www.cdc.gov/vaccines/imz-managers/nis/datasets.html. 2019.
