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
library(FactoMineR)
library(factoextra)
library(CAinterprTools)

# TODO: Separate tpack from non-tpack and chart differences in term counts left and right
# like https://www.r-bloggers.com/gender-and-verbs-across-100000-stories-a-tidy-analysis/

# Load in raw data file from GitHub
descriptors.frame <- read_csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/cite-keywords.csv", col_names = FALSE)
methods.frame <- read.csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/lit_review-data_sources-survey_combined.csv", header = TRUE)

year.frame <- descriptors.frame[1:2]
descriptors.frame[2] <- NULL

colnames(descriptors.frame)[1] <- "manuscriptID"

# Combine frames and merge by the Manuscript ID
overall.frame <- merge(keywords.frame, methods.frame, by.x = "manuscriptID")

# Set global minimum cut value
cut.min <<- 3

descriptors.OK.list <- colnames(descriptors.frame)[2:ncol(descriptors.frame)]
methods.OK.list <- colnames(methods.frame)[2:ncol(methods.frame)]

# Split the overall data into TPACK and non-TPACK sets for analysis
split.frame <- split(overall.frame, overall.frame$tpack)
split.frame$tpack$tpack <- NULL
split.frame$no_tpack$tpack <- NULL

# Convert dataframe from wide to long format (necessary for working with the data)
prepare.long <- function(data.set){
  # Remove any empty cells
  data.set <- na.omit(data.set)
  
  data.set <- melt(data.set, id.vars = c("manuscriptID"))

  for(i in 1:ncol(data.set)){
    data.set[,i] <- gsub("_", " ", data.set[,i])
    data.set[,i] <- tolower(data.set[,i])
  }
  
    
  # Define and remove stopwords, which will not be used in the analysis.
  data.set <- subset(data.set, !(variable %in% c("preservice teachers", "technology integration", 
                                              "computer uses in education", 
                                              "preservice teacher education", 
                                              "technology uses in education", "teacher education", 
                                              "educational technology", "teacher education programs", 
                                              "mixed methods research", "interviews", 
                                              "qualitative research", "information technology", 
                                              "correlation", "statistical analysis", 
                                              "comparative analysis", "online surveys", "educational change",
                                              "focus groups", "semi structured interviews", 
                                              "likert scales", "predictor variables", 
                                              "teacher surveys", "surveys", "observation", 
                                              "questionnaires", "student surveys", 
                                              "regression (statistics)", "case studies", 
                                              "pretests posttests", "technological literacy", 
                                              "technology education", "coding", "scores", 
                                              "scoring rubrics", "content analysis", "elementary education",
                                              "data analysis", "data collection", "science teachers",
                                              "effect size", "statistical significance",
                                              "participant observation", "elementary secondary education",
                                              "elementary school teachers", "inservice teacher education",
                                              "secondary school teachers", "english teachers")))
  
  # Combine similar descriptors
  data.set$value[grep("attitude", data.set$value)] <- "attitudes"
  data.set$value[grep("behavior", data.set$value)] <- "behavior"
  data.set$value[grep("games", data.set$value)] <- "games"
  data.set$value[grep("knowledge ", data.set$value)] <- "knowledge"
  data.set$value[grep("science instruction", data.set$value)] <- "stem education"
  data.set$value[grep("mathematics instruction", data.set$value)] <- "stem education"
  data.set$value[grep("discussion", data.set$value)] <- "discussion"
  data.set$value[grep("scaffolding", data.set$value)] <- "scaffolding"
  data.set$value[grep("constructivism", data.set$value)] <- "constructivism"
  data.set$value[grep("lesson plans", data.set$value)] <- "instructional design"
  data.set$value[grep("instructional design", data.set$value)] <- "instructional design"
  data.set$value[grep("curriculum development", data.set$value)] <- "instructional design"
  data.set$value[grep("video technology", data.set$value)] <- "specific technologies"
  data.set$value[grep("handheld devices", data.set$value)] <- "specific technologies"
  data.set$value[grep("faculty", data.set$value)] <- "teacher educators"
  data.set$value[grep("inquiry", data.set$value)] <- "stem education"
  
  for(i in 1:ncol(data.set)){
    # Replace spaces and remove other characters
    data.set[,i] <- gsub("[\\(\\)]", "", data.set[,i])
  }

  data.set <- data.set[!duplicated(data.set), ]

  return(data.set)
}

prepare.cluster <- function(data.set){
  for(i in 1:ncol(data.set)){
    data.set[,i][grep("no ", data.set[,i])] <- NA
  }

  # Remove any empty cells
  data.set <- na.omit(data.set)

  # Look for duplications and remove them
  data.set <- data.set[ , !(names(data.set) %in% c("X2","variable"))]
  data.set <- data.set[!duplicated(data.set), ]
  
  # Conduct a count of descriptors
  data.set.count <- data.set %>% 
    group_by(value) %>% 
    count(sort = TRUE)
  
  # Select only the descriptors that appear 4 or more times
  # For testing purposes, try 1 to include all descriptors
  data.set.sub <- subset(data.set.count, n >= cut.min)
  data.set <- filter(data.set, value %in% data.set.sub$value)
  
  return(data.set)
}

prepare.counting <- function(data.set){
  for(i in 1:ncol(data.set)){
    data.set[,i][grep("no ", data.set[,i])] <- NA
  }
  
  # Remove any empty cells
  data.set <- na.omit(data.set)
  
  # Look for duplications and remove them
  data.set <- data.set[ , !(names(data.set) %in% c("X2","variable"))]
  data.set <- data.set[!duplicated(data.set), ]
  
  # Conduct a count of descriptors
  data.set.count <- data.set %>% 
    group_by(value) %>% 
    count(sort = TRUE)
}

calculate.pairs <- function(data.set) {
  # Determine descriptor pairs
  
  data.pairs <- data.set %>% 
    pairwise_count(value, manuscriptID, sort = TRUE, upper = FALSE)
  data.set <- subset(data.pairs, n >= cut.min)
  data.set <- na.omit(data.set)
  return(data.set)
}

calculate.correlation <- function(data.set) {
  # Conduct correlation analysis between descriptors
  data.cors <- data.set %>% 
    group_by(value) %>%
    filter(n() >= cut.min) %>%
    pairwise_cor(value, manuscriptID, sort = TRUE, upper = FALSE)
  
  data.cors <- subset(data.cors, correlation >= 0.3 | correlation <= -0.3)
  return(data.cors)
  #descriptors.cors.wide <- dcast(descriptors.cors, formula = item1 ~ item2)
  #row.names(descriptors.cors.wide) <- descriptors.cors.wide$item1
  #descriptors.cors.wide$item1 <- NULL
  
  # Write table back into a csv file for further processing
  #write.csv(descriptors.cors.wide, file = "data/lit_review-descriptors-cors-el.csv", 
  #          row.names = TRUE)
}

calculate.scale <- function(data.set) {
  data.list <- as.list(scale(data.set$n, center = FALSE))
  return(data.list)
}

prepare.graph <- function(data.set, data.list) {
  data.el <- as.matrix(data.set[1:2])
  data.graph <- simplify(graph_from_edgelist(data.el, directed = FALSE))
  E(data.graph)$weight = as.numeric(data.list)
  return(data.graph)
  
}

calculate.communities <- function(data.graph){
  data.communities <- cluster_optimal(data.graph, weights = E(data.graph)$weight)
  return(data.communities)
}

plot.graph <- function(data.graph, data.communities){
  V(data.graph)$color <- data.communities$membership + 1
  plot(data.graph, layout = layout.fruchterman.reingold,
       edge.width = E(data.graph)$weight * 4)
}

bi.plot <- function(analysis.MCA, OK.list) { # analysis.type, max.dim,
  #analysis.biplot.cols.filename <- str_c("output-", analysis.stem, "-biplot_cols.tif")
  #analysis.biplot.filename <- str_c("output-", analysis.stem, "-biplot.tif")
  #analysis.biplot.cols <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
  #                                     axes = c(1,2), select.var = list(name = sources.list),
  #                                     col.var = "black") + theme_minimal()
  #analysis.biplot.cols <- analysis.biplot.cols + labs(title = analysis.biplot.cols.title)
  OK.list.names <- list(name = OK.list)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = FALSE,
                                     axes = c(1,2), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = OK.list.names) + theme_minimal()
#  analysis.biplot <- analysis.biplot + labs(title = analysis.biplot.title)
  # Output biplots
  print(analysis.biplot)
  #output.plot(analysis.biplot.cols, 300, analysis.biplot.cols.filename)
  #output.plot(analysis.biplot, 600, analysis.biplot.filename)
}

prepare.matrix <- function(data.set) {
  data.matrix <- acast(data.set, manuscriptID ~ variable)
  return(data.matrix)
}



do.cluster <- function(data.set) {
  data.set.cluster <- prepare.cluster(data.set)
  data.set.pairs <- calculate.pairs(data.set.cluster)
  data.set.scaled <- calculate.scale(data.set.pairs)
  data.set.graph <- prepare.graph(data.set.pairs, data.set.scaled)
  data.set.communities <- calculate.communities(data.set.graph)
  plot.graph(data.set.graph, data.set.communities)
}

do.MCA <- function(data.set){
  data.set.matrix <- prepare.matrix(data.set)
  data.set.OK.list <- as.list(colnames(data.set.matrix))
  data.set.MCA <- MCA(data.set.matrix, na.method = "NA", graph = FALSE)
  bi.plot(data.set.MCA, data.set.OK.list)
}

# OVERALL
overall.long <- prepare.long(overall.frame)
do.cluster(overall.long)
do.MCA(overall.long)

# TPACK
tpack.frame <- split.frame$tpack
tpack.long <- prepare.long(tpack.frame)
do.cluster(tpack.long)
do.MCA(tpack.long)

# NONTPACK MCA
non_tpack.frame <- split.frame$no_tpack
non_tpack.long <- prepare.long(non_tpack.frame)
do.cluster(non_tpack.long)
do.MCA(non_tpack.long)

overall.pairs <- calculate.pairs(overall.long)

tpack.count <- prepare.counting(tpack.long)

colnames(tpack.count)[2] <- "tpack"
tpack.count$tpack <- scale(tpack.count$tpack, center = FALSE)

non_tpack.count <- prepare.counting(non_tpack.long)

colnames(non_tpack.count)[2] <- "nontpack"
non_tpack.count$nontpack <- scale(non_tpack.count$nontpack, center = FALSE)

tpack.non.count <- merge(tpack.count, non_tpack.count, by = "value", 
                         all = TRUE)#, by.x = "value", all = TRUE)

tpack.non.count[is.na(tpack.non.count)] <- 0

tpack.non.count$tpack <- scale(tpack.non.count$tpack, center = FALSE)
tpack.non.count$nontpack <- scale(tpack.non.count$nontpack, center = FALSE)


tpack.non.count$difference <- tpack.non.count$tpack - tpack.non.count$nontpack



tpack.non.count <- tpack.non.count[with(tpack.non.count, order(-difference, value)), ]

tpack.non.count$ratio <- tpack.non.count$tpack / tpack.non.count$nontpack


p<-ggplot(tpack.non.count, aes(x=reorder(value, difference), y=difference, fill=value)) +
  geom_bar(stat="identity")+theme_minimal() + coord_flip() + theme(legend.position="none")


print(p)



bigrams <- overall.frame %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE)

bigrams_separated <- overall.pairs %>%
  separate(bigram, c("word1", "word2"), sep = " ")

he_she_words <- bigrams_separated %>%
  filter(word1 %in% c("tpack", "no tpack"))


# DESCRIPTORS

descriptors.long <- prepare.long(descriptors.frame)
descriptors.cluster <- prepare.cluster(descriptors.long)
descriptors.pairs <- calculate.pairs(descriptors.cluster)
descriptors.scaled <- calculate.scale(descriptors.pairs)
descriptors.graph <- prepare.graph(descriptors.pairs, descriptors.scaled)
descriptors.communities <- calculate.communities(descriptors.graph)

methods.long <- prepare.long(methods.frame)
methods.cluster <- prepare.cluster(methods.long)
methods.pairs <- calculate.pairs(methods.cluster)
methods.scaled <- calculate.scale(methods.pairs)
methods.graph <- prepare.graph(methods.pairs, methods.scaled)
methods.communities <- calculate.communities(methods.graph)



print(overall.communities$modularity)
plot.graph(overall.graph, overall.communities)
plot.graph(methods.graph, methods.communities)
plot.graph(descriptors.graph, descriptors.communities)

overall.correlation <- calculate.correlation(overall.cluster)



prepare.matrix <- function(data.set){
  rownames(data.set) <- data.set$manuscriptID
  data.set$manuscriptID <- NULL
#  for(i in 1:ncol(data.set)){
#    data.set[,i][grep("no_", data.set[,i])] <- NA
#  }
#  data.set <- data.set[,colSums(is.na(data.set))<nrow(data.set)]
#  data.set[!is.na(data.set)] <- 1
#  data.set[is.na(data.set)] <- 0
#  data.set <- data.frame(lapply(data.set, as.numeric), stringsAsFactors=FALSE)
#  data.set <- scale(data.set)
#  data.set <- t(data.set)
  return(data.set)
}

overall.matrix <- prepare.matrix(overall.frame)

overall.MCA <- MCA(overall.matrix, na.method = "NA")

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
