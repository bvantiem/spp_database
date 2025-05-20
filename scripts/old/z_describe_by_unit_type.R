# Table with overall means by Unit ####
data <- pcq2[,grep("mcc_", names(pcq2))]
tab <- describeBy(data, group = pcq2$unit_type, digits=2, mat=TRUE)
tab$scale_theory <- gsub("mcc_", "",rownames(tab)) # get scale names 
tab$scale_theory <- substr(tab$scale_theory, 1, nchar(tab$scale)-1)
tab <- tab[,c("mean", "group1", "scale_theory")]
tab <- reshape(tab, idvar = "scale_theory", timevar = "group1", direction = "wide", times = )
names(tab) <- gsub("mean.", "", names(tab))
tab <- left_join(tab,unique(pcq_lookup[which(pcq_lookup$pc_theory=="prison climate" &
                                               pcq_lookup$include_comparative_psych_analysis=="yes"),c("scale_theory_long","scale_theory", "scale_no_bosma")]))
tab <- tab %>%  arrange(scale_no_bosma) %>%
  relocate(scale_theory_long, .before=gp) %>%
  dplyr::select(-scale_theory, -scale_no_bosma, -ls) %>%
  filter(scale_theory_long %ni% c("Overall Question", "Subjective Severity of Imprisonment"))
names(tab) <- c("Scale", "General", "Therapeutic", "Honor", "Recovery", "Restrictive", "Transitional")

print(xtable(tab, digits = c(0,0,rep(2,6))), include.rownames = FALSE, file = "output/tables/unit_differences.txt")

# Calculate significant differences 

# temp <- pcq_lookup[pcq_lookup$question_qno %in% unlist(key.list),]
data <- pcq2[which(pcq2$unit_type != "ls"),]
names.list <- names(pcq2)[grep("mcc_", names(pcq2))]
for(i in names.list){
  print(i)
  lminput <- as.formula(paste(i, "~ unit_type")) 
  print(TukeyHSD(aov(lminput, data = data)))
  print("---------------------------------")
}

# Heat map of means 
data <- tab %>%
  pivot_longer(!Scale, names_to = "Unit", values_to = "Mean")
p <- ggplot(data, aes(x = Unit, y = Scale, fill = Mean)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Mean), color = "black", size = 3) +
  scale_fill_gradient(low = "blue", high = "white") +
  coord_fixed(ratio = .3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
ggsave("output/figures/unit_means.jpg", plot = p, width = 9, height = 6, dpi = 300)



