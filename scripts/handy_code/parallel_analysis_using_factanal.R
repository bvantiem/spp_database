## Handy factor analysis code ####
# # Factor analysis using factanal (alternative package)
# factanal.fit <- factanal(pcq2[complete.cases(pcq2[,qnos]),qnos], factors=12, rotation="promax")
# loadings(factanal.fit, digits=2)
# 
# # Scree plot alone  (using fa)
# # https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224
# # Note this has an option to work with missing values
# fa.fit  <- fa(pcq2[,qnos],
#               nfactors = 11, rotate = "oblimin")
# n_factors <- length(fa.fit$e.values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue = fa.fit$e.values)
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the unreduced correlation matrix)")