# Load necessary libraries
library(readr)
library(BEST)

# Load in data set from GitHub
descriptors.frame <- read_csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/raw_descriptions.csv", col_names = TRUE)

# Remove the "baseline" row because this isn't necessary for this analysis
descriptors.frame <- descriptors.frame[-1, ]

# Conduct a standard statistical t-test
descriptors.t <- t.test(descriptors.frame$tep, descriptors.frame$tetp, paired = TRUE)

# Conduct a Wilcoxon signed rank test
descriptors.w <- wilcox.test(descriptors.frame$tep,descriptors.frame$tetp,paired=TRUE)

# p value will likely be greater than 0.05, so conduct a Bayesian estimation
descriptors.b <- BESTmcmc(descriptors.frame$tep, descriptors.frame$tetp)

#summary(descriptors.b)
#plotAll(descriptors.b, credMass = 0.95)
#plot(descriptors.b)
#plot(descriptors.b, which = "sd")
#plot(descriptors.b, which = "mean")
#plot(descriptors.b, which = "effect")
#plot(descriptors.b, which = "nu")
#plotPostPred(descriptors.b)
#pairs(descriptors.b)
#head(descriptors.b$mu1)
muDiff <- descriptors.b$mu1 - descriptors.b$mu2
muDiff.1 <- mean(muDiff > 1.5)
sigmaDiff <- mean(descriptors.b$sigma1 - descriptors.b$sigma2)
#hist(descriptors.b$nu)
