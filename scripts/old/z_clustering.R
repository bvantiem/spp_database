library(lme4)


# Depvar: I am satisfied with this insitutiton 
# Baseline model 
fit.controls.scales.inst <- lm(data.reg$q1 ~ ., data=indvars.controls.scales)
summary(fit.controls.scales.inst)$adj.r.squared

indvars.controls.scales.lme <- data.reg %>% dplyr::select(age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca, unit_type)
depvars.formula <- names(indvars.controls.scales.lme)
depvars.formula <- depvars.formula[-which(depvars.formula=="unit_type")]
depvars.formula <- paste(depvars.formula, collapse = "+")
depvars.formula <- as.formula("data.reg$q1 ~(1|unit_type)")
fit1 <- lmer(depvars.formula, data = indvars.controls.scales.lme)
summary(fit1)

fit2 <- lm(data.reg$q1 ~ unit_type, data = indvars.controls.scales.lme)
summary(fit2)

library(fixest)

# Does variation around the unit mean on satisfaction with staff influence satisfaction with the institution?
# We are interested in individual variation in perceptions!
fit3 <- feols(q1 ~ mam_staff|unit_type,
              data = data.reg, 
              cluster = ~ unit_type)
summary(fit3)

# Below is the same as.. 
temp <- data.reg %>% dplyr::select(mam_staff, unit_type, q1)
data.reg.dummy <- fastDummies::dummy_cols(temp)
names(data.reg.dummy)[which(names(data.reg.dummy)=="unit_type_gp-tc")] <- "unit_type_gp_tc"
vars <- names(data.reg.dummy)
vars <- paste(vars[-which(vars=="q1")], collapse = "+")
reg.form <- as.formula(paste("q1 ~",vars))
fit3 <- feols(reg.form,
              data = data.reg.dummy, 
              cluster = ~ unit_type)
summary(fit3)

# How much does each unit influence satisfaction with the institution relative to the overall mean?
temp <- data.reg %>% dplyr::select(unit_type, q1)
data.reg.dummy <- fastDummies::dummy_cols(temp)
names(data.reg.dummy)[which(names(data.reg.dummy)=="unit_type_gp-tc")] <- "unit_type_gp_tc"
vars <- names(data.reg.dummy)
vars <- vars[-which(vars=="q1")]
vars <- vars[-which(vars=="unit_type")]
vars <- paste(vars, collapse = "+")
reg.form <- as.formula(paste("q1 ~",vars))
fit3 <- feols(reg.form,
              data = data.reg.dummy, 
              cluster = ~ unit_type)
summary(fit3)


# ICC
# How much of the variation in the dependent variable is attributed to unit type? (Clustered at the unit level)
.03598/(.03598+.99924) # Severity
.2139/(.2139+1.1485) # Satisfaction
.3999/(.3999+.9248) # Staff satisfaction 
.01537/(.01537+.69951) # Care

# How much of the variation is attributed to unit type after we control for individual characteristics? ####
indvars.controls.scales.lme <- data.reg %>% dplyr::select(age_on_20220501, foreign_born, high_school, partner, children, est_months_served_on_20220501, race_code, cell, asca, unit_type)
depvars.formula <- names(indvars.controls.scales.lme)
depvars.formula <- depvars.formula[-which(depvars.formula=="unit_type")]
depvars.formula <- paste(depvars.formula, collapse = "+")
depvars.formula <- as.formula(paste("data.reg$mam_staff ~", depvars.formula, " + (1|unit_type)"))
fit1 <- lmer(depvars.formula, data = indvars.controls.scales.lme)
summary(fit1)



.05942/(.05942+1.10419) # satisfaction
.2502/(.2501+.8445) # staff
