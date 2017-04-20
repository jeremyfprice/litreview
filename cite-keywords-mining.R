library(readr)
library(reshape2)
library(widyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidytext)
library(tm)
#library(plyr)

plot.words <- function(filename, plot.var, plot.threshold, plot.name){
  set.seed(1234)
  desc_cors %>%
    filter(plot.var > plot.threshold) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = plot.var, edge_width = plot.var), edge_colour = "royalblue") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = plot.name), repel = TRUE,
                   point.padding = unit(0.2, "lines")) +
    theme_void()
}

# change TRUE to FALSE if you have no column headings in the CSV
x <- read_csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/cite-keywords.csv", col_names = FALSE)

ggplot(data=x, aes(x[,2])) + geom_histogram()

x_long <- melt(x, id.vars = c("X1", "X2"))

x_long <- na.omit(x_long)

x_long$value <- tolower(x_long$value)

x_long <- subset(x_long, !(value %in% c("preservice teachers", "technology integration", 
                                        "computer uses in education", 
                                        "preservice teacher education", 
                                        "technology uses in education", "teacher education", 
                                        "educational technology", "teacher education programs", 
                                        "mixed methods research", "interviews", 
                                        "qualitative research", "information technology", 
                                        "correlation", "statistical analysis", 
                                        "comparative analysis", "online surveys", 
                                        "focus groups", "semi structured interviews", 
                                        "likert scales", "predictor variables", 
                                        "teacher surveys", "surveys", "observation", 
                                        "questionnaires", "student surveys", 
                                        "regression (statistics)", "case studies", 
                                        "pretests posttests", "technological literacy", 
                                        "technology education", "coding", "scores", 
                                        "scoring rubrics", "content analysis", 
                                        "data analysis", "data collection", 
                                        "effect size", "statistical significance", 
                                        "participant observation", "elementary secondary education")))

x_long$value[grep("pedagogical content knowledge", x_long$value)] <- "tpack"
x_long$value[grep("attitude", x_long$value)] <- "attitudes"
x_long$value[grep("behavior", x_long$value)] <- "behavior"
x_long$value[grep("games", x_long$value)] <- "games"
x_long$value[grep("knowledge ", x_long$value)] <- "knowledge"
x_long$value[grep("science instruction", x_long$value)] <- "stem education"
x_long$value[grep("mathematics instruction", x_long$value)] <- "stem education"
x_long$value[grep("lesson plans", x_long$value)] <- "planning_design"
x_long$value[grep("instructional design", x_long$value)] <- "planning_design"
x_long$value[grep("curriculum development", x_long$value)] <- "planning_design"
x_long$value[grep("video technology", x_long$value)] <- "specific_technologies"
x_long$value[grep("handheld devices", x_long$value)] <- "specific_technologies"

#x_long$value[grep("teaching methods", x_long$value)] <- "methods"
#x_long$value[grep("school teachers", x_long$value)] <- "elementary secondary teachers"
#x_long$value[grep("science teachers", x_long$value)] <- "subject area teachers"
#x_long$value[grep("english teachers", x_long$value)] <- "subject area teachers"
#x_long$value[grep("mathematics teachers", x_long$value)] <- "subject area teachers"

x_long <- x_long[ , !(names(x_long) %in% c("X2","variable"))]
#duplicated.columns <- duplicated(t(x_long))
#x_long.reduced <- x_long[, !duplicated.columns]
x_long <- x_long[!duplicated(x_long), ]

#View(x_long)

x_long %>% 
  group_by(value) %>% 
  count(sort = TRUE)

x_count <- x_long %>% 
  group_by(value) %>% 
  count(sort = TRUE)

head(x_count)

x_count.sub <- subset(x_count, n > (mean(x_count$n) + 1))
#View(x_count.sub)

x_long.sub <- filter(x_long, value %in% x_count.sub$value)

desc_pairs <- x_long.sub %>% 
  pairwise_count(value, X1, sort = TRUE, upper = FALSE)

#View(desc_pairs)

desc_cors <- x_long.sub %>% 
  group_by(value) %>%
  filter(n() >= 3) %>%
  pairwise_cor(value, X1, sort = TRUE, upper = FALSE)

#View(desc_cors)


desc_tf_idf <- x_long.sub %>% 
  count(X1, value, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(value, X1, n)

desc_tf_idf %>% 
  arrange(tf_idf)
#View(desc_tf_idf)

# Just keep the important ones
x_long.keep <- subset(x_long, value %in% x_long.sub$value)

# Replace spaces and remove other characters
x_long.keep$value <- gsub(" ", "_", x_long.keep$value)
x_long.keep$value <- gsub("[\\(\\)]", "", x_long.keep$value)

#View(x_long.keep)

#x_wide.keep <- spread(x_long.keep, key = X1, value = value, fill = NA)
x_wide.keep <- dcast(x_long.keep, formula = X1 ~ value)
for(i in 1:ncol(x_wide.keep)){
  x_wide.keep[is.na(x_wide.keep[,i]), i] <- (paste("no", colnames(x_wide.keep[i]), sep = "_"))
}
colnames(x_wide.keep)[1] <- "manuscriptID"

#View(x_wide.keep)

write.csv(x_wide.keep, file = "data/lit_review-keywords-processed.csv", row.names = FALSE)

# rename NA from column name: http://stackoverflow.com/questions/38176326/replace-na-in-column-by-value-corresponding-to-column-name-in-seperate-table

#plot.words("", desc_cors$correlation, )

#x_wide <- aggregate(value ~ X1, x_long.sub, paste)
#x_wide.2 <- data.frame(matrix(unlist(x_wide$value)), stringsAsFactors = FALSE)
#x_wide.3 <- data.frame(Reduce(rbind, x_wide$value))

set.seed(1234)
desc_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(1234)
desc_pairs %>%
  filter(n >= 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()
