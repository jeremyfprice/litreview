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
library(extrafont)
library(linkcomm)

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
cut.min <<- 0
gsPalette <<- c("black", "gray40")

descriptors.OK.list <- colnames(descriptors.frame)[2:ncol(descriptors.frame)]
methods.OK.list <- colnames(methods.frame)[2:ncol(methods.frame)]

# Split the overall data into TPACK and non-TPACK sets for analysis
split.frame <- split(overall.frame, overall.frame$tpack)
tpack.frame <- split.frame$tpack
tpack.frame$tpack <- NULL
no_tpack.frame <- split.frame$no_tpack
no_tpack.frame$tpack <- NULL

# Split the overall data into attitudes and no-attitudes sets for analysis
split.frame <- split(overall.frame, overall.frame$attitudes)
attitudes.frame <- split.frame$attitudes
attitudes.frame$attitudes <- NULL
no_attitudes.frame <- split.frame$no_attitudes
no_attitudes.frame$attitudes <- NULL

# Split the overall data into survey and no-survey sets for analysis
split.frame <- split(overall.frame, overall.frame$survey)
survey.frame <- split.frame$survey
survey.frame$survey <- NULL
no_survey.frame <- split.frame$no_survey
no_survey.frame$survey <- NULL

# Split the overall data into artifact and no-artifact sets for analysis
split.frame <- split(overall.frame, overall.frame$artifact)
artifact.frame <- split.frame$artifact
artifact.frame$artifact <- NULL
no_artifact.frame <- split.frame$no_artifact
no_artifact.frame$artifact <- NULL


# Convert dataframe from wide to long format (necessary for working with the data)
prepare.long <- function(data.set){
  # Remove any empty cells
  #data.set <- na.omit(data.set)
  
  data.set <- melt(data.set, id.vars = c("manuscriptID"))

  for(i in 1:ncol(data.set)){
    data.set[,i] <- gsub("_", " ", data.set[,i])
  }
  for(i in 2:ncol(data.set)){
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
                                              "teacher surveys", "surveys",# "observation", 
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
  data.set$value[grep("learning activities", data.set$value)] <- "instructional design"
  data.set$value[grep("video technology", data.set$value)] <- "specific technologies"
  data.set$value[grep("handheld devices", data.set$value)] <- "specific technologies"
  data.set$value[grep("faculty", data.set$value)] <- "teacher educators"
  data.set$value[grep("inquiry", data.set$value)] <- "stem education"
  data.set$value[grep("college school cooperation", data.set$value)] <- "field experiences"
  data.set$value[grep("field experience programs", data.set$value)] <- "field experiences"
  data.set$value[grep("student teaching", data.set$value)] <- "field experiences"
  data.set$value[grep("teaching skills", data.set$value)] <- "pedagogy"
  data.set$value[grep("teaching methods", data.set$value)] <- "pedagogy"

  for(i in 1:ncol(data.set)){
    # Replace spaces and remove other characters
    data.set[,i] <- gsub("[\\(\\)]", "", data.set[,i])
  }

  data.set <- data.set[!duplicated(data.set), ]

  return(data.set)
}

output.plot <- function(plot.output, plot.resolution, file.name){
  tiff(file.name, width = 11, height = 8.5, units = "in", res = plot.resolution)
  print(plot.output)
  dev.off()
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

calculate.communities <- function(data.graph, data.el, data.scaled){
  data.graph <- data.set.graph
  data.el <- data.set.pairs
  data.scaled <- data.set.scaled
  #data.communities <- cluster_optimal(data.graph, weights = E(data.graph)$weight)
  data.communities <- cluster_fast_greedy(data.graph, weights = E(data.graph)$weight, modularity = TRUE)
  #data.communities <- cluster_edge_betweenness(data.graph, weights = E(data.graph)$weight)
  #data.communities <- cluster_walktrap(data.graph, weights = E(data.graph)$weight)
  #data.communities <- cluster_infomap(data.graph)
  #data.communities <- multilevel.community(data.graph, weights = E(data.graph)$weight)
  #data.communities <- label.propagation.community(data.graph, weights = E(data.graph)$weight)
  set.seed(123)
#  data.set2 <- prepare.numeric(overall.frame2)
#  d <- dist(data.set2, method = "euclidean")
#  res.hc <- hclust(d, method = "ward.D2" )
#  plot(res.hc, cex = 0.6, hang = -1)
  colnames(data.el) <- c("V1", "V2", "weight")
  data.el$weight <- data.scaled
  data.el <- data.frame(lapply(data.el, as.character), stringsAsFactors=FALSE)
  g_linkcomm = getLinkCommunities(data.el, directed = FALSE)
  #return(data.communities)
}

plot.graph <- function(data.graph, data.communities, data.title){
  cluster.file <- tolower(paste("results/cluster/cluster-", data.title,".tif", sep = ""))
  dendro.file <- tolower(paste("results/cluster/dendro-", data.title, ".tif", sep = ""))
  V(data.graph)$color <- data.communities$membership + 1
  graph.plot <- plot(data.graph, edge.width = E(data.graph)$weight * 4, layout = layout.auto)
  output.plot(graph.plot, 300, cluster.file)
  dendro.plot <- plot_dendrogram(data.communities, mode = "auto")
  output.plot(dendro.plot, 300, dendro.file)
}

bi.plot <- function(analysis.MCA, OK.list, data.title) {
  OK.list.names <- list(name = OK.list)
  dim.list <- c("1-2", "1-3", "2-3")
  dim.mca.list <- c(c(1, 2), c(1, 3), c(2, 3))
  plot.title12 <- paste("Biplot for dimensions 1 and 2 of", data.title, "manuscripts")
  plot.title13 <- paste("Biplot for dimensions 1 and 3 of", data.title, "manuscripts")
  plot.title23 <- paste("Biplot for dimensions 2 and 3 of", data.title, "manuscripts")
  plot.file12 <- tolower(paste("results/mca/biplot/", data.title, "-biplot12.tif", sep = ""))
  plot.file13 <- tolower(paste("results/mca/biplot/", data.title, "-biplot13.tif", sep = ""))
  plot.file23 <- tolower(paste("results/mca/biplot/", data.title, "-biplot23.tif", sep = ""))
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(1, 2), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = OK.list.names) + theme_minimal() +
    theme(text=element_text(size=10, family="Fira Code")) + ggtitle(plot.title12)
  output.plot(analysis.biplot, 600, plot.file12)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(1, 3), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = OK.list.names) + theme_minimal() +
    theme(text=element_text(size=10, family="Fira Code")) + ggtitle(plot.title13)
  output.plot(analysis.biplot, 600, plot.file13)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(2,3), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = OK.list.names) + theme_minimal() +
    theme(text=element_text(size=10, family="Fira Code")) + ggtitle(plot.title23)
  output.plot(analysis.biplot, 600, plot.file23)
}

prepare.numeric <- function(data.set){
  for(i in 1:ncol(data.set)){
    data.set[,i][grep("no_", data.set[,i])] <- NA
  }
  data.set <- data.set[,colSums(is.na(data.set))<nrow(data.set)]
  data.set[!is.na(data.set)] <- 1
  data.set[is.na(data.set)] <- 0
  data.set <- data.frame(lapply(data.set, as.numeric), stringsAsFactors=FALSE)
  data.set <- scale(data.set)
  data.set <- t(data.set)
  return(data.set)
}

prepare.matrix <- function(data.set) {
  data.matrix <- acast(data.set, manuscriptID ~ variable)
  return(data.matrix)
}

scree.plot <- function(data.set, data.title) {
  plot.title <- paste("Scree plot for", data.title, "manuscripts")
  plot.file <- tolower(paste("results/mca/scree/", data.title, "-screeplot.tif", sep = ""))
  analysis.screeplot <- fviz_screeplot(data.set, addlabels = TRUE, barfill = "gray40", 
                                       barcolor = "gray40", linecolor = "black") + theme_minimal()
  analysis.screeplot <- analysis.screeplot + labs(title = plot.title)
  # Output the scree plot
  output.plot(analysis.screeplot, 300, plot.file)
}

contribution.plot <- function(data.set, data.title) {
#  for(i in 1:3) {
  plot.1.title <- paste("Contribution plot for dimension 1 of", data.title, "manuscripts")
  plot.1.file <- tolower(paste("results/mca/contrib/", data.title, "1-contributionplot.tif", sep = ""))
  plot.2.title <- paste("Contribution plot for dimension 2 of", data.title, "manuscripts")
  plot.2.file <- tolower(paste("results/mca/contrib/", data.title, "2-contributionplot.tif", sep = ""))
  plot.3.title <- paste("Contribution plot for dimension 3 of", data.title, "manuscripts")
  plot.3.file <- tolower(paste("results/mca/contrib/", data.title, "3-contributionplot.tif", sep = ""))
  analysis.1.dimplot <- fviz_contrib(data.set, choice = "var", axes = 1, fill = "gray40", 
                                    color = "black") + theme_minimal() +
    labs(title = plot.1.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family="Fira Code"))
  analysis.2.dimplot <- fviz_contrib(data.set, choice = "var", axes = 2, fill = "gray40", 
                                     color = "black") + theme_minimal() +
    labs(title = plot.2.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family="Fira Code"))
  analysis.3.dimplot <- fviz_contrib(data.set, choice = "var", axes = 3, fill = "gray40", 
                                     color = "black") + theme_minimal() +
    labs(title = plot.3.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family="Fira Code"))
  output.plot(analysis.1.dimplot, 300, plot.1.file)
  output.plot(analysis.2.dimplot, 300, plot.2.file)
  output.plot(analysis.3.dimplot, 300, plot.3.file)
  #  }
}

do.cluster <- function(data.set, data.title) {
  data.set <- survey.long
  data.set.cluster <- prepare.cluster(data.set)
  data.set.pairs <- calculate.pairs(data.set.cluster)
  data.set.scaled <- calculate.scale(data.set.pairs)
  data.set.graph <- prepare.graph(data.set.pairs, data.set.scaled)
  #data.set.communities <- calculate.communities(data.set.graph)
  data.set.communities <- calculate.communities(data.set.pairs, data.set.scaled)
  ######################
  
  ######################
  #plot.graph(data.set.graph, data.set.communities, data.title)
}

for(i in 1:ncol(data.set.matrix)) {
  data.set.matrix[,i] <- as.integer(data.set.matrix[,i])
}

do.MCA <- function(data.set, data.title){
  data.set.matrix <- prepare.matrix(data.set)
  data.set.OK.list <- as.list(colnames(data.set.matrix))
  data.set.MCA <- MCA(data.set.matrix, na.method = "NA", graph = FALSE)
  bi.plot(data.set.MCA, data.set.OK.list, data.title)
  scree.plot(data.set.MCA, data.title)
  contribution.plot(data.set.MCA, data.title)
}

do.special.count <- function(data.set) {
  for(i in 1:ncol(data.set)){
    data.set[,i][grep("no_", data.set[,i])] <- NA
  }
  data.set <- prepare.long(data.set)
  # Remove any empty cells
  data.set <- na.omit(data.set)
  data.set <- subset(data.set, select = value)
  # Conduct a count of descriptors
  data.set.count <- data.set %>% 
    group_by(value) %>% 
    count(sort = TRUE)
  return(data.set.count)
}

do.compare.graph <- function(data.set.1, data.set.2, title.1, title.2) {
  file.name <- tolower(paste("results/compare/",title.1,"_", title.2, "-comparison.tif", sep = ""))
  data.set.1.count <- do.special.count(data.set.1)
  data.set.2.count <- do.special.count(data.set.2)
  colnames(data.set.1.count)[2] <- "data1"
  colnames(data.set.2.count)[2] <- "data2"
  compare.count <- merge(data.set.1.count, data.set.2.count, all = TRUE)
  compare.count[is.na(compare.count)] <- 0
  compare.count[4] <- round((compare.count[2] + 1) / sum(compare.count[2] + 1), digits = 3)
  compare.count[5] <- round((compare.count[3] + 1) / sum(compare.count[3] + 1), digits = 3)
  compare.count[6] <- round(log2(compare.count[4]/compare.count[5]), digits = 3)
  compare.count[compare.count[6] > 0, "status"] <- title.1
  compare.count[compare.count[6] < 0, "status"] <- title.2
  colnames(compare.count)[6] <- "log"
  compare.title <- paste("Relative pairing of descriptors of", title.1, "compared to", title.2, "manuscripts")
  compare.graph <- ggplot(compare.count, aes(x = reorder(value, log), 
                                                      y = log, fill = status)) +
    geom_bar(stat="identity", color="black") + theme_minimal() + coord_flip() +
    scale_y_continuous(breaks = -2:2, labels=c("4X","2X","Same","2X","4X")) + xlab("Descriptor") +
    scale_fill_manual("", values = c("gray80", "gray40")) +
    ylab("Relative difference (log)") + labs(title = compare.title) +
    theme(text=element_text(size=10, family="Fira Code"))
  output.plot(compare.graph, 300, file.name)
}

# OVERALL
overall.long <- prepare.long(overall.frame)
overall.count <- do.special.count(overall.frame)
overall.count.quant <- quantile(overall.count$n)
overall.reduced.filter <- subset(overall.count, (n >= overall.count.quant [4]))
#overall.reduced.filter <- rbind(overall.reduced.filter, "self efficacy")
overall.reduced.long <- subset(overall.long, (value %in% overall.reduced.filter$value))
count.graph <- ggplot(overall.count, aes(x = reorder(value, -n), y = n)) +
  geom_bar(stat = "identity", color = "gray40") + theme_minimal() +
  geom_hline(yintercept = overall.count.quant[4], color = "black", linetype = "dashed") + theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(text = element_text(size = 10, family = "Fira Code")) + ylab("Count") + xlab("Descriptors")
output.plot(count.graph, 300, "results/descriptor-cutoff.tif")

overall.reduced.list <- overall.reduced.long
overall.reduced.list$value <- NULL
overall.reduced.list$variable <- NULL
overall.reduced.list <- as.data.frame(unique(overall.reduced.list))

overall.leftout.long <- subset(overall.long, !(manuscriptID %in% overall.reduced.list$manuscriptID))

do.cluster(overall.long, "all")
do.MCA(overall.long, "all")
do.MCA(overall.reduced.long, "reduced")
do.cluster(overall.reduced.long, "reduced")

# TPACK
tpack.long <- prepare.long(tpack.frame)
do.cluster(tpack.long, "TPACK")
do.MCA(tpack.long, "TPACK")

# NONTPACK
no_tpack.long <- prepare.long(no_tpack.frame)
do.cluster(no_tpack.long, "non-TPACK")
do.MCA(no_tpack.long, "non-TPACK")

# ATTITUDES
attitude.long <- prepare.long(attitudes.frame)
do.cluster(attitude.long, "attitude")
do.MCA(attitude.long, "attitude")

# NON-ATTITUDES
no_attitude.long <- prepare.long(no_attitudes.frame)
do.cluster(no_attitude.long)
do.MCA(no_attitude.long, "non-attitude")

# SURVEY
survey.long <- prepare.long(survey.frame)
do.cluster(survey.long)
do.MCA(survey.long, "survey")

# NON-SURVEY
no_survey.long <- prepare.long(no_survey.frame)
do.cluster(no_survey.long)
do.MCA(no_survey.long, "non-survey")


do.compare.graph(tpack.frame, no_tpack.frame, "TPACK", "non-TPACK")
do.compare.graph(attitudes.frame, no_attitudes.frame, "attitude", "non-attitude")
do.compare.graph(survey.frame, no_survey.frame, "survey", "non-survey")
do.compare.graph(artifact.frame, no_artifact.frame, "artifact", "non-artifact")

