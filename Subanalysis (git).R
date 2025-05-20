### Pre-op Post-op Subanalysis 

### Additional Analysis 

rm(list = ls())

source("Data Clean.R")

library(broom)
library(lme4)
library(emmeans)

## iHOT: Pre-post PAO subanalysis 

dat_ihot_t <- dat_full %>% 
  select(mrn, group, contains("i_hot"))

last_ihot <- dat_ihot_t %>%
  select(mrn, starts_with("i_hot12")) %>%
  gather(key, value, -mrn) %>%
  group_by(mrn) %>%
  filter(!is.na(value)) %>%
  slice_tail(n = 1) %>%
  rename(last_followup_score = value)

dat_ihot_t <- left_join(dat_ihot_t, last_ihot, by = "mrn")

dat_ihot_t <- dat_ihot_t %>% 
  select(
    mrn, group, contains("pre"), contains("last")
  ) %>% 
  drop_na(
    
  )


full_ihot_pre_post <- dat_ihot_t %>% 
  pivot_longer(
    i_hot12_preop:last_followup_score,
    names_to = "Time",
    values_to = "ihot_score"
  )


full_ihot_pre_post$Time <- factor(full_ihot_pre_post$Time, levels = c("i_hot12_preop",  "last_followup_score"))
full_ihot_pre_post$Time <- factor(full_ihot_pre_post$Time, labels = c("Pre-Op", "Last Follow-Up"))

pre_post_ihot_p <- ggplot(
  data = full_ihot_pre_post,
  aes(
    x = Time,
    y = ihot_score,
    fill = group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  labs(
    x = "",
    y = "iHOT-12 Score",
    fill = ""
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )

full_ihot_pre_post_sum <- full_ihot_pre_post %>% 
  group_by(
    group, Time
  ) %>% 
  summarize(
    mean = mean(ihot_score, na.rm = T),
    sd = sd(ihot_score, na.rm = T)
  ) %>% 
  arrange(
    Time
  )

names(full_ihot_pre_post_sum) <- c("Group", "Month post-op", "Mean iHOT-12", "SD")

full_ihot_pre_post_sum <- full_ihot_pre_post_sum %>% kable(digits = 3) %>% kable_classic(html_font = 'cambria', full_width = F)

  
ihot_pre_post_mod <- lmer(ihot_score ~ group*Time + (1|mrn), data = full_ihot_pre_post)

## Pre-op vs Post-op for all groups 
ihot_posthoc_time <- emmeans(ihot_pre_post_mod, ~ Time | group)

pre_v_post_ihot_cont <- tidy(contrast(ihot_posthoc_time, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

pre_v_post_ihot_cont_conf <- as.data.frame(round(tidy(confint(contrast(ihot_posthoc_time, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
pre_v_post_ihot_cont_conf <- paste0("(", pre_v_post_ihot_cont_conf[, 1], ", ", pre_v_post_ihot_cont_conf[, 2], ")")

pre_v_post_ihot_cont <- cbind(pre_v_post_ihot_cont, pre_v_post_ihot_cont_conf)

names(pre_v_post_ihot_cont) <- c("Group", "Time Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

pre_v_post_ihot_cont <- pre_v_post_ihot_cont %>%
  mutate_if(
    is.numeric, round, digits = 2
  )  %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)



# Post hoc tests for between-group differences at each time point
ihot_posthoc_group <- emmeans(ihot_pre_post_mod, ~ group | Time)

ihot_cont_conf <- as.data.frame(round(tidy(confint(contrast(ihot_posthoc_group, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
ihot_cont_conf <- paste0("(", ihot_cont_conf[, 1], ", ", ihot_cont_conf[, 2], ")")

ihot_cont_sum <- tidy(contrast(ihot_posthoc_group, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

ihot_cont_sum <- cbind(ihot_cont_sum, ihot_cont_conf)

names(ihot_cont_sum) <- c("Time", "Group Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

ihot_cont_sum <- ihot_cont_sum %>%
  mutate_if(
    is.numeric, round, digits = 2
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)


## NAHS: Pre-post PAO subanalysis 

dat_nahs_t <- dat_full %>% 
  select(mrn, group, contains("non_"))

last_nahs <- dat_nahs_t %>%
  select(mrn, starts_with("non_")) %>%
  gather(key, value, -mrn) %>%
  group_by(mrn) %>%
  filter(!is.na(value)) %>%
  slice_tail(n = 1) %>%
  rename(last_followup_score = value)

dat_nahs_t <- left_join(dat_nahs_t, last_nahs, by = "mrn")

dat_nahs_t <- dat_nahs_t %>% 
  select(
    mrn, group, contains("pre"), contains("last")
  ) %>% 
  drop_na(
    
  )

full_nahs_pre_post <- dat_nahs_t

full_nahs_pre_post <- full_nahs_pre_post %>% 
  pivot_longer(
    non_arthritic_hip_preop:last_followup_score,
    names_to = "Time",
    values_to = "nahs_score"
  )


full_nahs_pre_post$Time <- factor(full_nahs_pre_post$Time, levels = c("non_arthritic_hip_preop", "last_followup_score"))
full_nahs_pre_post$Time <- factor(full_nahs_pre_post$Time, labels = c("Pre-Op", "Last Follow-Up"))

pre_post_nahs_p <- ggplot(
  data = full_nahs_pre_post,
  aes(
    x = Time,
    y = nahs_score,
    fill = group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  labs(
    x = "",
    y = "NAHS Score",
    fill = ""
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )

full_nahs_pre_post_sum <- full_nahs_pre_post %>% 
  group_by(
    group, Time
  ) %>% 
  summarize(
    mean = mean(nahs_score, na.rm = T),
    sd = sd(nahs_score, na.rm = T)
  ) %>% 
  arrange(
    Time
  )

names(full_nahs_pre_post_sum) <- c("Group", "Month post-op", "Mean NAHS", "SD")

full_nahs_pre_post_sum <- full_nahs_pre_post_sum %>% kable(digits = 3) %>% kable_classic(html_font = 'cambria', full_width = F)


nahs_pre_post_mod <- lmer(nahs_score ~ group*Time + (1|mrn), data = full_nahs_pre_post)

## Pre-op vs Post-op for all groups 
nahs_posthoc_time <- emmeans(nahs_pre_post_mod, ~ Time | group)
pre_v_post_nahs_cont <- tidy(contrast(nahs_posthoc_time, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

pre_v_post_nahs_cont_conf <- as.data.frame(round(tidy(confint(contrast(nahs_posthoc_time, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
pre_v_post_nahs_cont_conf <- paste0("(", pre_v_post_nahs_cont_conf[, 1], ", ", pre_v_post_nahs_cont_conf[, 2], ")")

pre_v_post_nahs_cont <- cbind(pre_v_post_nahs_cont, pre_v_post_nahs_cont_conf)

names(pre_v_post_nahs_cont) <- c("Group", "Time Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

pre_v_post_nahs_cont <- pre_v_post_nahs_cont %>%
  mutate_if(
    is.numeric, round, digits = 2
  )  %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)


# Post hoc tests for between-group differences at each time point
nahs_posthoc_group <- emmeans(nahs_pre_post_mod, ~ group | Time)

pre_post_nahs_cont_sum <- tidy(contrast(nahs_posthoc_group, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

pre_post_nahs_cont_conf <- as.data.frame(round(tidy(confint(contrast(nahs_posthoc_group, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
pre_post_nahs_cont_conf <- paste0("(", pre_post_nahs_cont_conf[, 1], ", ", pre_post_nahs_cont_conf[, 2], ")")

pre_post_nahs_cont_sum <- cbind(pre_post_nahs_cont_sum, pre_post_nahs_cont_conf)

names(pre_post_nahs_cont_sum) <- c("Time", "Group Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

pre_post_nahs_cont_sum <- pre_post_nahs_cont_sum %>%
  mutate_if(
    is.numeric, round, digits = 2
  )  %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)

