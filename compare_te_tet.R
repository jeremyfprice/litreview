library(ggplot2)
library(extrafont)
library(ggthemes)
library(ggpubr)

compare.count <- read.csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/raw_descriptions.csv")
compare.count <- subset(compare.count, !(terms %in% "baseline"))
compare.count$te2 <- round((compare.count$te + 1) / sum(compare.count$te + 1), digits = 3)
compare.count$tet2 <- round((compare.count$tet + 1) / sum(compare.count$tet + 1), digits = 3)
compare.count$tet2[compare.count$tet == 0] <- 0
compare.count$difference <- round(log2(compare.count$te2/compare.count$tet2), digits = 3)
compare.count$difference[compare.count$difference == Inf] <- 3
#compare.count$difference <- compare.count$te2 - compare.count$tet2
compare.count[compare.count$difference > 0, "status"] <- "Teacher Education"
compare.count[compare.count$difference < 0, "status"] <- "Teacher Education with Technology"
#compare.title <- paste("Relative pairing of descriptors of Teacher Education compared to Teacher Education with Technology manuscripts")
compare.graph2 <- ggdotchart(compare.count, x = "abbreviations", y = "difference",
                             color = "status", sorting = "descending", add = "segments",
                             rotate = TRUE, dot.size = 6,
                             # label = abs(round(compare.count$difference, 1)),
                             #font.label = list(size = 8, vjust = 0.5, color = "white"),
                             ggtheme = theme_pubr()) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  scale_y_continuous(breaks = -3:3, limits = c(-3, 3), labels=c("8X", "4X","2X","Same","2X","4X", "8X+")) + xlab("Descriptors") +
  ylab("Relative difference (log2)") +
  theme(text=element_text(size=10, family="Noto Mono"))

compare.graph <- ggplot(compare.count, aes(x = reorder(abbreviations, difference), 
                                           y = difference, color = status)) +
  geom_segment(aes(x = reorder(abbreviations, difference),
                   y = 0, xend = reorder(abbreviations, difference), yend = difference), color = "#83786F") +
  geom_point(size = 6) + coord_flip() +
  theme_minimal() +
  scale_color_manual("Manuscript Category", values = c("#01426A", "#DC8823")) + 
  scale_y_continuous(breaks = -3:3, limits = c(-3, 3), labels=c("8X+", "4X","2X","Same","2X","4X", "8X+")) + xlab("Descriptors") +
  ylab("Relative difference (log)") +
  theme(text=element_text(size=10, family="Noto Mono")) + #theme(legend.position="bottom") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#EEEEEE")) +
  geom_hline(yintercept = 0, color = "#83786F", linetype = "solid")
  print(compare.graph)


tiff("results/compare-te_tet.tif", width = 11, height = 8.5, units = "in", res = 300)
  #dev.set(3)
  print(compare.graph)
dev.off()
#geom_bar(stat="identity", color="black") + 
#compare.ttable <- data.frame(category = compare.count$abbreviations, te = compare.count$te, tet = compare.count$tet)
#rotate <- function(x) t(apply(x, 2, rev))
#compare.ttable <- rotate(compare.ttable)
#compare.ttest <- pairwise.t.test(compare.ttable$te, compare.ttable$tet)
