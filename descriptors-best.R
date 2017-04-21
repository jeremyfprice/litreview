library(readr)
library(BEST)

descriptors.frame <- read_csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/raw_descriptions.csv", col_names = TRUE)

descriptors.frame <- descriptors.frame[-1, ]

t.test(descriptors.frame$tep, descriptors.frame$tetp, paired = TRUE)

wilcox.test(descriptors.frame$tep,descriptors.frame$tetp,paired=TRUE)

BESTout <- BESTmcmc(descriptors.frame$tep, descriptors.frame$tetp, burnInSteps = 1000)
BESTout
summary(BESTout)
plot(BESTout)
plot(BESTout, "sd")
plotPostPred(BESTout)
pairs(BESTout)
head(BESTout$mu1)
muDiff <- BESTout$mu1 - BESTout$mu2
mean(muDiff > 1.5)
mean(BESTout$sigma1 - BESTout$sigma2)
hist(BESTout$nu)
