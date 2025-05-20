### Summary Stats 

# Sum Fun

{
  ## T-test and Chi-square Table Fn 
  
  library(table1)
  
  render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
         c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
  }
  
  render.cat <- function(x) {
    c("", 
      sapply(stats.default(x), 
             function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
  }
  
  
  pvalue <- function(x, ...) {
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    if (is.numeric(y)) {
      p <- t.test(y ~ g)$p.value
    } else {
      p <- chisq.test(table(y, g), simulate.p.value = T)$p.value
    }
    c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
  }
  
  
}

## Cohort Data 

dat_pao_sum <- ...
dat_pao_sum <- janitor::clean_names(dat_pao_sum)

dat_pao_sum <- dat_pao_sum %>% 
  select(
    mrn:bmi, i_hot12_preop:non_arthritic_hip_5ypo
  )

dat_pao_sum$group <- "PAO > 45"

## Control Data 

dat_con_sum <- ...
dat_con_sum <- janitor::clean_names(dat_con_sum)

dat_con_sum$group <- "Control"


dat_sum2 <- rbind(dat_con_sum[, c(2, 3, 20)], dat_pao_sum[, c(2, 3, 19)])

label(dat_sum2$age) <- "Age"
label(dat_sum2$gender) <- "Sex"
label(dat_sum2$group) <- "Group"

tab2 <- table1(
  ~ . | group,
  data = dat_sum2,
  overall = F,
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat,
  render.missing = NULL
)



# 
# 
# dat_sum <- dat_pao %>% 
#   select(
#     age:postop_flexion
#   ) %>% 
#   select(
#     !bone_graft_of_cysts
#   )
# 
# dat_sum$postop_ir90 <- as.numeric(dat_sum$postop_ir90)
# 
# dat_sum <- dat_sum %>% 
#   mutate_if(
#     is.character, as.factor
#   )
# 
# label(dat_sum$age) <- "Age"
# label(dat_sum$gender) <- "Sex"
# label(dat_sum$bmi) <- "BMI"
# dat_sum$beightons <- as.factor(dat_sum$beightons)
# label(dat_sum$beightons) <- "Beightons Score"
# label(dat_sum$duration_of_pain_at_initial_presentation) <- "Duration of Pain (Initial)"
# label(dat_sum$p_reop_ir90) <- "Pre-Op IR90"
# label(dat_sum$p_reop_flexion) <- "Pre-Op Flexion"
# label(dat_sum$preop_lcea) <- "Pre-Op LCE"
# label(dat_sum$preop_tonnis) <- "Pre-Op TONNIS Angle"
# label(dat_sum$preop_lwbz_joint_space) <- "Pre-Op LWBZ Joint Space"
# label(dat_sum$preop_medial_joint_space) <- "Pre-Op Medial Joint Space"
# label(dat_sum$preop_nad_mm) <- "Pre-Op NAD (mm)"
# label(dat_sum$preop_acetabular_eq_version) <- "Pre-Op Acetabular EQ Version"
# label(dat_sum$preop_femoral_torsion) <- "Pre-Op Femoral Torsion"
# label(dat_sum$tegner_score) <- "Tegner Score"
# label(dat_sum$laterality) <- "Laterality"
# dat_sum$microfracture_1_yes <- as.factor(dat_sum$microfracture_1_yes)
# label(dat_sum$microfracture_1_yes) <- "Microfracture (1 = yes)"
# label(dat_sum$postop_lcea) <- "Post-Op LCE"
# label(dat_sum$postop_tonnis_angle) <- "Post-Op TONNIS Angle"
# label(dat_sum$postop_nad) <- "Post-Op NAD"
# label(dat_sum$postop_ir90) <- "Post-Op IR90"
# label(dat_sum$postop_flexion) <- "Post-Op Flexion"
# 
# tab1 <- table1(
#   ~ .,
#   data = dat_sum,
#   # overall = F,
#   # extra.col=list(`P-value`= pvalue),
#   render.continuous = render.cont,
#   render.categorical = render.cat,
#   render.missing = NULL
# )
# 
