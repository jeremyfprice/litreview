# Load in libraries
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

# Load in raw data file from GitHub
descriptors.frame <- read_csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/cite-keywords.csv", col_names = FALSE)

# Convert dataframe from wide to long format (necessary for working with the data)
descriptors.frame.long <- melt(descriptors.frame, id.vars = c("X1", "X2"))

# Remove any empty cells
descriptors.frame.long <- na.omit(descriptors.frame.long)

# Convert all data to lower case for easier manipulation
descriptors.frame.long$value <- tolower(descriptors.frame.long$value)

# Define and remove stopwords, which will not be used in the analysis.
# These words 
descriptors.frame.long <- subset(descriptors.frame.long, !(value %in% c("preservice teachers", "technology integration", 
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
                                        "participant observation", "elementary secondary education",
                                        "elementary school teachers", "inservice teacher education")))



# Combine similar descriptors
descriptors.frame.long$value[grep("attitude", descriptors.frame.long$value)] <- "attitudes"
descriptors.frame.long$value[grep("behavior", descriptors.frame.long$value)] <- "behavior"
descriptors.frame.long$value[grep("games", descriptors.frame.long$value)] <- "games"
descriptors.frame.long$value[grep("knowledge ", descriptors.frame.long$value)] <- "knowledge"
descriptors.frame.long$value[grep("science instruction", descriptors.frame.long$value)] <- "stem education"
descriptors.frame.long$value[grep("mathematics instruction", descriptors.frame.long$value)] <- "stem education"
descriptors.frame.long$value[grep("discussion", descriptors.frame.long$value)] <- "discussion"
descriptors.frame.long$value[grep("scaffolding", descriptors.frame.long$value)] <- "scaffolding"
descriptors.frame.long$value[grep("constructivism", descriptors.frame.long$value)] <- "constructivism"
#descriptors.frame.long$value[grep("lesson plans", descriptors.frame.long$value)] <- "planning_design"
#descriptors.frame.long$value[grep("instructional design", descriptors.frame.long$value)] <- "planning_design"
#descriptors.frame.long$value[grep("curriculum development", descriptors.frame.long$value)] <- "planning_design"
descriptors.frame.long$value[grep("video technology", descriptors.frame.long$value)] <- "specific_technologies"
descriptors.frame.long$value[grep("handheld devices", descriptors.frame.long$value)] <- "specific_technologies"

# Rename "pedagogical content knowledge" to "tpack"
descriptors.frame.long$value[grep("pedagogical content knowledge", descriptors.frame.long$value)] <- "tpack"

# Rejected changes
#x_long$value[grep("teaching methods", x_long$value)] <- "methods"
#x_long$value[grep("school teachers", x_long$value)] <- "elementary secondary teachers"
#x_long$value[grep("science teachers", x_long$value)] <- "subject area teachers"
#x_long$value[grep("english teachers", x_long$value)] <- "subject area teachers"
#x_long$value[grep("mathematics teachers", x_long$value)] <- "subject area teachers"

# Look for duplications and remove them
descriptors.frame.long <- descriptors.frame.long[ , !(names(descriptors.frame.long) %in% c("X2","variable"))]
descriptors.frame.long <- descriptors.frame.long[!duplicated(descriptors.frame.long), ]


#dfl.count <- descriptors.frame.long %>% 
#  group_by(value) %>% 
#  count(sort = TRUE)

# Conduct a count of descriptors
descriptors.count <- descriptors.frame.long %>% 
  group_by(value) %>% 
  count(sort = TRUE)

# Select only the descriptors that appear 4 or more times
# For testing purposes, try 1 to include all descriptors
descriptors.count.sub <- subset(descriptors.count, n >= 3)

descriptors.frame.long.sub <- filter(descriptors.frame.long, value %in% descriptors.count.sub$value)

# Determine descriptor pairs
descriptors.pairs <- descriptors.frame.long.sub %>% 
  pairwise_count(value, X1, sort = TRUE, upper = FALSE)

descriptors.pairs.wide <- dcast(descriptors.pairs, formula = item1 ~ item2)
#row.names(descriptors.pairs.wide) <- descriptors.pairs.wide$item1
#descriptors.cors.wide$item1 <- NULL
# Write table back into a csv file for further processing
write.csv(descriptors.pairs.wide, file = "data/lit_review-descriptors-pairs-el.csv", 
          row.names = TRUE)

# Conduct correlation analysis between descriptors
descriptors.cors <- descriptors.frame.long.sub %>% 
  group_by(value) %>%
  filter(n() >= 2) %>%
  pairwise_cor(value, X1, sort = TRUE, upper = FALSE)

descriptors.cors.wide <- dcast(descriptors.cors, formula = item1 ~ item2)
row.names(descriptors.cors.wide) <- descriptors.cors.wide$item1
descriptors.cors.wide$item1 <- NULL

# Write table back into a csv file for further processing
write.csv(descriptors.cors.wide, file = "data/lit_review-descriptors-cors-el.csv", 
          row.names = TRUE)






# Conduct a tf_idf analysis
descriptors.tf_idf <- descriptors.frame.long.sub %>% 
  count(X1, value, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(value, X1, n)

descriptors.tf_idf %>% 
  arrange(tf_idf)

# Just keep the important descriptors
descriptors.frame.long.keep <- subset(descriptors.frame.long, value %in% descriptors.frame.long.sub$value)

# Replace spaces and remove other characters
descriptors.frame.long.keep$value <- gsub(" ", "_", descriptors.frame.long.keep$value)
descriptors.frame.long.keep$value <- gsub("[\\(\\)]", "", descriptors.frame.long.keep$value)

# Reshape table back into wide format
descriptors.frame.wide.keep <- dcast(descriptors.frame.long.keep, formula = X1 ~ value)

# Rename column names to the descriptor that they hold
for(i in 1:ncol(descriptors.frame.wide.keep)){
  descriptors.frame.wide.keep[is.na(descriptors.frame.wide.keep[,i]), i] <- 
    (paste("no", colnames(descriptors.frame.wide.keep[i]), sep = "_"))
}

# Rename first column to manuscriptID for standardization purposes
colnames(descriptors.frame.wide.keep)[1] <- "manuscriptID"

# Write table back into a csv file for further processing
write.csv(descriptors.frame.wide.keep, file = "data/lit_review-keywords-processed.csv", 
          row.names = FALSE)

# Plot histogram based on year published
ggplot(data=descriptors.frame, aes(descriptors.frame[,2])) + geom_histogram()

# Graph correlation relationships
set.seed(1234)
descriptors.cors %>%
  filter(correlation >= .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.05, "lines")) +
  theme_void()

# Graph paired relationships
set.seed(1234)
descriptors.pairs %>%
  filter(n >= 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()
