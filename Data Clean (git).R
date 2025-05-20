### Data Clean 

library(readxl)
library(tidyverse)

## Cohort Data 

dat_pao <- ...
dat_pao <- janitor::clean_names(dat_pao)

dat_pao <- dat_pao %>% 
  select(
    mrn:bmi, i_hot12_preop:non_arthritic_hip_5ypo
  )

dat_pao$group <- "PAO > 45"

## Control Data 

dat_con <- ...
dat_con <- janitor::clean_names(dat_con)

dat_con$group <- "Control"

## Merge 

# Merge 
common_columns <- intersect(names(dat_con), names(dat_pao))

dat_con_subset <- dat_con[, common_columns]
dat_pao_subset <- dat_pao[, common_columns]

dat_full <- rbind(dat_con_subset, dat_pao_subset)

## Separate and elongate 

dat_ihot <- dat_full %>% 
  select(
    mrn, age, gender, group, contains("i_hot")
  )

dat_ihot_long <- dat_ihot %>% 
  pivot_longer(
    cols = i_hot12_preop:i_hot12_5ypo,
    names_to = "time",
    values_to = "ihot_score"
  ) %>% 
  mutate(
    time_m = case_when(
      time == "i_hot12_10ypo" ~ 120, 
      time == "i_hot12_12mpo" ~ 12, 
      time == "i_hot12_18mpo" ~ 18, 
      time == "i_hot12_24mpo" ~ 24, 
      time == "i_hot12_3mpo" ~ 3, 
      time == "i_hot12_5ypo" ~ 60, 
      time == "i_hot12_6mpo" ~ 6, 
      time == "i_hot12_6wpo" ~ 1.5, 
      time == "i_hot12_preop" ~ 0
    )
  )

dat_ihot_long_an <- dat_ihot_long %>% 
  filter(
    time_m > 0 
  )

dat_nahs <- dat_full %>% 
  select(
    mrn, age, gender, group, contains("non_arthritic")
  )

dat_nahs_long <- dat_nahs %>% 
  pivot_longer(
    cols = non_arthritic_hip_preop:non_arthritic_hip_5ypo,
    names_to = "time",
    values_to = "nahs_score"
  ) %>% 
  mutate(
    time_m = case_when(
      time == "non_arthritic_hip_10ypo" ~ 120, 
      time == "non_arthritic_hip_12mpo" ~ 12, 
      time == "non_arthritic_hip_18mpo" ~ 18, 
      time == "non_arthritic_hip_24mpo" ~ 24, 
      time == "non_arthritic_hip_3mpo" ~ 3, 
      time == "non_arthritic_hip_5ypo" ~ 60, 
      time == "non_arthritic_hip_6mpo" ~ 6, 
      time == "non_arthritic_hip_6wpo" ~ 1.5, 
      time == "non_arthritic_hip_preop" ~ 0
    )
  )  %>% 
  filter(
    time_m <= 60
  )

dat_nahs_long_an <- dat_nahs_long %>% 
  filter(
    time_m > 0 
  )

