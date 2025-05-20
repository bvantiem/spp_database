

i <- match(pcq_lookup$question_pa_2022a,fa_tab_1of$Item)
pcq_lookup$factor_of <- fa_tab_1of$Factor_of[i]
dat <- pcq2_w2_of %>% select(q10, q11, q12, q13, q14,
                             q15, q16, q17, q18, q19, q20, q21, q22,
                             q26, q29, q30, q31, q32,
                             q99, q100, q101,
                             q69, q70, q71, q72,
                             q73, q74, q75, q76,
                             q82, q83, q84, q85)
qnos <- names(dat)
# merge unit_type back into dat
# have dropped no cases so can just use the vector of IDs in pcq2_w2
dat$unit_type <- pcq2_w2$unit_type

# Score scales
scale.key <- list(prisoners = c("q10", "q11", "q12", "q13", "q14") ,
                  staff = c("q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22"),
                  safety = c("q26", "q29", "q30", "q31", "q32"),
                  shop = c("q99", "q100", "q101"),
                  actav = c("q69", "q70", "q71", "q72"), 
                  reint = c("q73", "q74", "q75", "q76") ,
                  autonomy = c("q82", "q83", "q84", "q85"))
dat <- cbind(dat, scoreItems(scale.key, dat)$scores)

library(factoextra)
fviz_nbclust(dat %>% select(-starts_with(c("q", "unit_type"))), kmeans, method = "wss")

# Cluster by unit type. Any clear patterns?
# Retain only unit types with more than 10 individuals in it
k <- 2
data.clusters <- kmeans(dat %>% select(-starts_with(c("q", "unit_type"))), centers = k)
temp1 <- as.data.frame(cbind(unit_type = as.character(dat$unit_type), 
                             cluster = as.numeric(data.clusters$cluster)))
temp1$cluster <- as.numeric(temp1$cluster)
table(temp1$cluster, temp1$unit_type)

# Staff / Prisoner
temp2 <- as.data.frame(dat %>% select(staff, prisoners))
data.clusters <- kmeans(temp2, centers = k)
temp3 <- as.data.frame(cbind(unit_type = as.character(dat$unit_type), 
                             cluster = as.numeric(data.clusters$cluster),
                             staff_score = dat$staff,
                             prisoner_score = dat$prisoner))
temp3$cluster <- as.factor(temp3$cluster)
temp3$staff_score <- as.numeric(temp3$staff_score)
temp3$prisoner_score <- as.numeric(temp3$prisoner_score)
table(temp3$cluster, temp3$unit_type)

ggplot(temp3 %>% filter(unit_type %in% c("gp", "gp-tc", "hons")), 
       aes(y = staff_score, x = prisoner_score, color = cluster, shape = unit_type)) + 
  geom_point()

# Autonomy / Actav 
temp2 <- as.data.frame(dat %>% select(autonomy, actav))
data.clusters <- kmeans(temp2, centers = k)
temp3 <- as.data.frame(cbind(unit_type = as.character(dat$unit_type), 
                             cluster = as.numeric(data.clusters$cluster),
                             autonomy_score = dat$autonomy,
                             actav_score = dat$actav))
temp3$cluster <- as.factor(temp3$cluster)
temp3$autonomy_score <- as.numeric(temp3$autonomy_score)
temp3$actav_score <- as.numeric(temp3$actav_score)
table(temp3$cluster, temp3$unit_type)

data.plot <- temp3 %>% filter(unit_type %in% c("gp", "gp-tc", "hons"))
ggplot(data.plot, 
       aes(y = autonomy_score, x = actav_score, color = cluster, shape = unit_type)) + 
  geom_point()


