# visits and contact items ####
vars <- c(scale.hyp.key[["visits"]],scale.hyp.key[["contact"]])
tab <- pcq_lookup[pcq_lookup$question_qno %in% vars,
                  c("scale_theory_long","question_pa_2022a")]
print(xtable(as.data.frame(tab), caption="Items"), include.rownames=FALSE)

# Missing data 
pcq3 <- pcq
pcq3 <- as.data.frame(pcq3)
for(i in names(pcq3)){
  b <- which(pcq3[,i]==111)
  pcq3[b,i] <- NA
}

# Contact
vars <- scale.hyp.key[["contact"]]
temp <- pcq3[,vars]
names(temp) <- pcq_lookup[pcq_lookup$question_qno %in% vars,"question_pa_2022a"]
names(temp) <- c("Satisfied with how often I can see family, friends, partner", "Satisfied with how often I can see child(ren)", "Satisfied with how often I can see my lawyer here")
gg_miss_upset(temp)  

# Visit
vars <- pcq_lookup[pcq_lookup$question_qno %in% scale.hyp.key[["visits"]],"question_qno"]
temp <- pcq3[,vars]
names(temp) <- pcq_lookup[pcq_lookup$question_qno %in% scale.hyp.key[["visits"]],"question_pa_2022a"]
gg_miss_upset(temp, nsets=8)  

# Factor Analysis Illustration
vars <- pcq_lookup[pcq_lookup$question_qno %in% c(scale.hyp.key[["visits"]],scale.hyp.key[["contact"]]),
                  "question_pa_2022a"]
tab <- data.frame(item = vars,
                  F1 = c(rep(1, 6), rep(2,2), rep(3,3)),
                  F2 = c(rep(1, 6), rep(2,2), rep(1,3)),
                  F3 = c(rep(1, 6), rep(2,2), rep(3,3)))
names(tab) <- c("Item", "Missing Data Imputed (N=631)", "Complete Cases (N=276)", "Bosma et al. 2020 (N=?)")
print(xtable(as.data.frame(tab), digits = 0, caption="Exploratory Factor Analysis"), include.rownames=FALSE)


