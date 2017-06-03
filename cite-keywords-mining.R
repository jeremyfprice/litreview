#######################
#                     #
#  LOAD IN LIBRARIES  #
#                     #
#######################


#library(readr)
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
library(AnthroTools)
library(ggalluvial)
library(WeightedCluster)
library(treemap)


######################
#                    #
#  DEFINE FUNCTIONS  #
#                    #
######################

# Convert dataframe from wide to long format (necessary for working with the data)
prepare.long <- function(data.set){
  # Turn off warnings, which 
  options(warn = -1)
  
  # Reshape from wide to long
  data.set <- melt(data.set, id.vars = c("manuscriptID"))

  # Loop through each column...
  for(i in 1:ncol(data.set)){
    # Remove any underscores and replace them with spaces for easier reading
    data.set[,i] <- gsub("_", " ", data.set[,i])
    # Set all content to lowercase
    data.set[,i] <- tolower(data.set[,i])
    # Remove parentheses characters
    data.set[,i] <- gsub("[\\(\\)]", "", data.set[,i])
  }

  # Define and remove stopwords, which will not be used in the analysis
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
                                              "secondary school teachers", "english teachers", "workshops")))
  
  data.set <- subset(data.set, data.set$variable %in% overall.count$value)
  
  
  # Combine similar descriptors into broader categories
  #data.set$value[grep("attitude", data.set$variable)] <- "attitudes"
  #data.set$value[grep("behavior", data.set$value)] <- "behavior"
  #data.set$value[grep("games", data.set$value)] <- "games"
  #data.set$value[grep("knowledge ", data.set$value)] <- "knowledge"
  #data.set$value <- gsub("science instruction", "stem education", data.set$value)
  #data.set$value <- gsub("mathematics instruction", "stem education", data.set$value)
  #data.set$value <- gsub("inquiry", "stem education", data.set$value)
  #data.set$value[grep("discussion", data.set$value)] <- "discussion"
  #data.set$value[grep("scaffolding", data.set$value)] <- "scaffolding"
  #data.set$value[grep("constructivism", data.set$value)] <- "constructivism"
  data.set$value <- gsub("lesson plans", "instructional design", data.set$value)
  data.set$variable <- gsub("lesson plans", "instructional design", data.set$variable)
  data.set$value <- gsub("curriculum development", "instructional design", data.set$value)
  data.set$variable <- gsub("curriculum development", "instructional design", data.set$variable)
  data.set$value <- gsub("learning activities", "instructional design", data.set$value)
  data.set$variable <- gsub("learning activities", "instructional design", data.set$variable)
  #data.set$value[grep("video technology", data.set$value)] <- "specific technologies"
  #data.set$value[grep("handheld devices", data.set$value)] <- "specific technologies"
  data.set$value <- gsub("college faculty", "teacher educators", data.set$value)
  data.set$variable <- gsub("college faculty", "teacher educators", data.set$variable)
  data.set$value <- gsub("faculty development", "teacher educators", data.set$value)
  data.set$variable <- gsub("faculty development", "teacher educators", data.set$variable)
  data.set$value <- gsub("college school cooperation", "field experiences", data.set$value)
  data.set$variable <- gsub("college school cooperation", "field experiences", data.set$variable)
  data.set$value <- gsub("field experience programs", "field experiences", data.set$value)
  data.set$variable <- gsub("field experience programs", "field experiences", data.set$variable)
  data.set$value <- gsub("student teaching", "field experiences", data.set$value)
  data.set$variable <- gsub("student teaching", "field experiences", data.set$variable)
  data.set$value <- gsub("teaching skills", "pedagogy", data.set$value)
  data.set$variable <- gsub("teaching skills", "pedagogy", data.set$variable)
  data.set$value <- gsub("teaching methods", "pedagogy", data.set$value)
  data.set$variable <- gsub("teaching methods", "pedagogy", data.set$variable)
  data.set$value <- gsub("educational practices", "pedagogy", data.set$value)
  data.set$variable <- gsub("educational practices", "pedagogy", data.set$variable)
  data.set$value <- gsub("inquiry", "stem education", data.set$value)
  data.set$variable <- gsub("inquiry", "stem education", data.set$variable)
  data.set$value <- gsub("instructional effectiveness", "teacher effectiveness", data.set$value)
  data.set$variable <- gsub("instructional effectiveness", "teacher effectiveness", data.set$variable)
  data.set$value <- gsub("teacher competencies", "teacher effectiveness", data.set$value)
  data.set$variable <- gsub("teacher competencies", "teacher effectiveness", data.set$variable)
  data.set$value <- gsub("knowledge", "tpack", data.set$value)
  data.set$variable <- gsub("knowledge", "tpack", data.set$variable)

  # Remove any duplicated rows to clean up the data set
  data.set <- data.set[!duplicated(data.set), ]
  
  # Send back the data set
  return(data.set)
  
  # Turn warnings back on just in case
  options(warn = 0)
}

output.plot <- function(plot.output, plot.resolution, file.name){
  # Open up a file to write to
  tiff(file.name, width = 11, height = 8.5, units = "in", res = plot.resolution)
  
  # Send the plot to the file
  print(plot.output)
  
  # Close the file
  dev.off()
}

prepare.cluster <- function(data.set){
#  data.set <- overall.reduced.long
  for(i in 1:ncol(data.set)){
    data.set[,i][grep("no ", data.set[,i])] <- NA
  }

  # Remove any empty cells
  data.set <- na.omit(data.set)

  data.set$variable <- NULL
  
  # Look for duplications and remove them
  #data.set <- data.set[ , !(names(data.set) %in% c("X2","variable"))]
  data.set <- data.set[!duplicated(data.set), ]
  
  # Conduct a count of descriptors
  data.set.count <- data.set %>% 
    group_by(value) %>% 
    count(sort = TRUE)
  
  # Select only the descriptors that appear 4 or more times
  # For testing purposes, try 1 to include all descriptors
  #data.set.sub <- subset(data.set.count, n >= cut.min)
  #data.set <- filter(data.set, value %in% data.set.sub$value)
  
  # Send the data back
  return(data.set)
}

calculate.pairs <- function(data.set) {
  # Determine descriptor pairs
  data.set <- data.set.cluster
  data.pairs <- data.set %>% 
    pairwise_count(value, manuscriptID, sort = TRUE, upper = FALSE)
  
  #data.set <- subset(data.pairs, n >= cut.min)
  
  # Get rid of any NA rows
  data.set <- na.omit(data.set)
  
  # Send the data back
  return(data.set)
}

calculate.correlation <- function(data.set) {
  # Conduct correlation analysis between descriptors
  data.cors <- data.set %>% 
    group_by(value) %>%
    filter(n() >= cut.min) %>%
    pairwise_cor(value, manuscriptID, sort = TRUE, upper = FALSE)
  
  # Remove low correlations at and below 0.3
  data.cors <- subset(data.cors, correlation >= 0.3 | correlation <= -0.3)
  
  # Send the data back
  return(data.cors)
}

calculate.scale <- function(data.set) {
  #data.set <- data.set.pairs
  # Scale the numeric data and set as a list
  data.list <- as.list(scale(data.frame(data.set$n, center = FALSE)))

  # Send the data back
  return(data.list)
}

prepare.graph <- function(data.set, data.list) {
  
  # Create a matrix from the data
  data.el <- as.matrix(data.set[1:2])
  
  # Create graph object for further use
  data.graph <- simplify(graph_from_edgelist(data.el, directed = FALSE))
  
  # Pull out the weights and set as a column
  E(data.graph)$weight = as.numeric(data.list)
  
  # Send the data back
  return(data.graph)
  
}

calculate.communities <- function(data.graph, data.el, data.scaled, data.title){
#  data.graph <- data.set.graph
#  data.el <- data.set.pairs
#  data.scaled <- data.set.scaled
#  data.title <- "survey"
  data.set.communities.use <- NULL
  data.communities.fc <- cluster_fast_greedy(data.graph, weights = E(data.graph)$weight, modularity = TRUE)
  if(membership(data.communities.fc) > 1 && membership(data.communities.fc) > (length(data.el)/2)) {
    data.communities.lv <- cluster_louvain(data.graph, weights = E(data.graph)$weight)
    data.communities.modularity <- modularity(data.communities.fc) - modularity(data.communities.lv)
    if(data.communities.modularity > 0) {data.communities.use <- data.communities.fc} else {data.communities.use <- data.communities.fc}
    #plot.communities <- plot(data.communities.use, data.graph, col = membership(data.communities.use),
         #mark.groups = communities(data.communities.use), edge.color = c("black", "red"))
    #community.set <<- rbind(data.communities.use, names, membership)
    community.temp <- cbind(descriptor = data.communities.use$names, group = data.title,
                            community = data.communities.use$membership,
                            #method = data.communities.use$algorithm, 
                            mod = data.communities.use$modularity)
    community.set <<- rbind(community.set, community.temp)
    #return(data.communities.use)
  }
}
#  set.seed(123)
#  data.set2 <- prepare.numeric(overall.frame2)
#  d <- dist(data.set2, method = "euclidean")
#  res.hc <- hclust(d, method = "ward.D2" )
#  plot(res.hc, cex = 0.6, hang = -1)
#  colnames(data.el) <- c("V1", "V2", "weight")
#  data.el$weight <- data.scaled
#  data.el <- data.frame(lapply(data.el, as.character), stringsAsFactors=FALSE)
#  g_linkcomm = getLinkCommunities(data.el, directed = FALSE)


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
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = FALSE,
                                     axes = c(1, 2), col.ind = "#01426A",
                                     col.var = "#DC8823", labelsize = 2,
                                     select.var = OK.list.names, addEllipses = FALSE) +
    theme_minimal() +
    theme(text=element_text(size = 10, family = font.choice)) + ggtitle(plot.title12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = bg.color)) +
    geom_hline(yintercept = 0, color = line.color, linetype = "dashed") +
    geom_vline(xintercept = 0, color = line.color, linetype = "dashed")
  output.plot(analysis.biplot, 600, plot.file12)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = FALSE,
                                     axes = c(1, 3), col.ind = "#01426A",
                                     col.var = "#DC8823", labelsize = 2,
                                     select.var = OK.list.names, addEllipses = FALSE) +
    theme_minimal() +
    theme(text=element_text(size = 10, family = font.choice)) + ggtitle(plot.title12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = bg.color)) +
    geom_hline(yintercept = 0, color = line.color, linetype = "dashed") +
    geom_vline(xintercept = 0, color = line.color, linetype = "dashed")
  output.plot(analysis.biplot, 600, plot.file13)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = FALSE,
                                     axes = c(2, 3), col.ind = "#01426A",
                                     col.var = "#DC8823", labelsize = 2,
                                     select.var = OK.list.names, addEllipses = FALSE) +
    theme_minimal() +
    theme(text=element_text(size = 10, family = font.choice)) + ggtitle(plot.title12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = bg.color)) +
    geom_hline(yintercept = 0, color = line.color, linetype = "dashed") +
    geom_vline(xintercept = 0, color = line.color, linetype = "dashed")
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
  data.set$variable <- data.set$value
  data.set <- data.frame(lapply(data.set, as.character), stringsAsFactors=FALSE)
  data.set <- data.set[!duplicated(data.set), ]
  csle <- rle(data.set$manuscriptID)
  csids <- rep(seq_len(length(csle$values)), times=csle$lengths)
  idcs <- as.data.frame(cbind(id=csids, data.set[1:3]))
  idcs$variable <- NULL
  data.matrix <- acast(idcs, manuscriptID ~ value, value.var = "value")
  return(data.matrix)
}

scree.plot <- function(data.set, data.title) {
  plot.title <- paste("Scree plot for", data.title, "manuscripts")
  plot.file <- tolower(paste("results/mca/scree/", data.title, "-screeplot.tif", sep = ""))
  analysis.screeplot <- fviz_screeplot(data.set, addlabels = TRUE, barfill = bg.color, 
                                       barcolor = bg.color, linecolor = line.color) + theme_minimal()
  analysis.screeplot <- analysis.screeplot + labs(title = plot.title) +
    theme(text=element_text(size=10, family=font.choice))
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
  analysis.1.dimplot <- fviz_contrib(data.set, choice = "var", axes = 1, fill = bg.color, 
                                    color = line.color) + theme_minimal() +
    labs(title = plot.1.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family=font.choice))
  analysis.2.dimplot <- fviz_contrib(data.set, choice = "var", axes = 2, fill = bg.color, 
                                     color = line.color) + theme_minimal() +
    labs(title = plot.2.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family=font.choice))
  analysis.3.dimplot <- fviz_contrib(data.set, choice = "var", axes = 3, fill = bg.color, 
                                     color = line.color) + theme_minimal() +
    labs(title = plot.3.title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(text=element_text(size=10, family=font.choice))
  output.plot(analysis.1.dimplot, 300, plot.1.file)
  output.plot(analysis.2.dimplot, 300, plot.2.file)
  output.plot(analysis.3.dimplot, 300, plot.3.file)
  #  }
}

vector.is.empty <- function(x) return(length(x) ==0 )

identify.clusters <- function(data.set, data.title) {
  data.set$variable <- NULL
  data.set[,2][grep("no ", data.set[,2])] <- NA
  data.set <- na.omit(data.set)
  data.set <- as.matrix(data.set)
  data.graph <- simplify(graph_from_edgelist(data.set, directed = FALSE),
                            remove.loops = TRUE, remove.multiple = FALSE)
  #trial.table <- data.frame(trialNo = integer(), noClusters = integer(),
  #                          modularity = integer())
  data.hub <- hub_score(data.graph)
  data.authority <- authority_score(data.graph)
  set.seed(seed.value)
  data.community <- cluster_louvain(data.graph)
  data.modularity <- modularity(data.community)
  data.clusters <<- data.frame(manuscriptID = toupper(data.community$names),
                               tribe = data.community$membership,
                               mod = data.modularity)
  rownames(data.clusters) <- NULL

  new.row <- data.frame(trial = data.title, noClusters = length(data.clusters), 
                        modularity = data.modularity)
  trial.table <<- rbind(trial.table, new.row)
  
  trial1 <- data.clusters[1:2]
  trial2 <- data.clusters[1:2]
  data.el <- merge(trial1, trial2, by = "tribe")
  data.el$tribe <- NULL
  data.el$modularity <- data.modularity
  return(data.el)

  #cluster.focus <- subset(cluster.master.frame, (manuscriptID %in% focus.list$manuscriptID))
  
  #focus2 <- cluster.focus[1:2]
  #focus3 <- cluster.focus[1:2]
  #focus.el <- merge(focus2, focus3, by = "tribe")
  #focus.el$tribe <- NULL
  #focus.el$modularity <- cluster.modularity
  #focus.el <- as.matrix(focus.el)
  #return(focus.el)
  
  #g <- simplify(graph_from_edgelist(focus.el, directed = FALSE), remove.loops = TRUE, remove.multiple = FALSE)
  
  #plot(g)
}
  
  
  
do.cluster <- function(data.set, data.title) {
  clusters.el <- identify.clusters(data.set, data.title)
  community.el <<- rbind(community.el, clusters.el)
}

#  data.set.cluster <- prepare.cluster(data.set)
#  data.set.pairs <- calculate.pairs(data.set.cluster)
#  data.set.scaled <- calculate.scale(data.set.pairs)
#  data.set.graph <- prepare.graph(data.set.pairs, data.set.scaled)
#  data.set.communities <- calculate.communities(data.set.graph, data.set.pairs, data.set.scaled, data.title)
#  if(!vector.is.empty(data.set.communities)) {
    #plot.graph(data.set.graph, data.set.communities, data.title)
#  }
#}

for(i in 1:ncol(data.set.matrix)) {
  data.set.matrix[,i] <- as.integer(data.set.matrix[,i])
}

do.MCA <- function(data.set, data.title){
  ### What is going on here?
  #data.set <- overall.reduced.long
  #data.title <- "reduced"
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
  compare.count[compare.count[6] == 0, "status"] <- "same"
  colnames(compare.count)[6] <- "log"
  compare.title <- paste("Relative pairing of descriptors of", title.1, "compared to", title.2, "manuscripts")
  v <- color.palette
  names(v) <- c(title.1, title.2, "same")
  compare.graph <- ggplot(compare.count, aes(x = reorder(value, log), 
                                             y = log, color = status)) +
    geom_hline(yintercept = 0, color = line.color, linetype = "solid") +
    geom_segment(aes(x = reorder(value, log),
                     y = 0, xend = reorder(value, log), yend = log), color = line.color) +
    geom_point(size = 6) + coord_flip() +
    theme_minimal() +
    scale_color_manual("Manuscript Category", values = v) +
    scale_y_continuous(breaks = -3:3, limits = c(-3, 3), labels=c("8X", "4X","2X","Same","2X","4X", "8X+")) +
    xlab("Descriptor") + ylab("Relative difference (log)") + guides(hide.this = FALSE) +
    theme(text=element_text(size=10, family = font.choice)) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#EEEEEE"))
  output.plot(compare.graph, 300, file.name)
}


#######################
#                     #
#  PREPARE DATA SETS  #
#                     #
#######################


# Load in raw data file from GitHub
descriptors.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-keywords-processed.csv", col_names = TRUE)
methods.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-data_sources-survey_combined.csv", col_names = TRUE)

#year.frame <- descriptors.frame[1:2]
#descriptors.frame[2] <- NULL

focus.list <<- as.data.frame(read_csv("~/Box Sync/lit_review/article-master-crosswalk.csv", col_names = FALSE))
colnames(focus.list) <- c("manuscriptID", "fullRef", "artTitle")

# Temporary exclusion of studies
descriptors.frame <- descriptors.frame[!descriptors.frame$manuscriptID %in% c("TAI2015", "PARKYANG2013", "KOPCHADI2016"), ]
methods.frame <- methods.frame[!methods.frame$manuscriptID %in% c("TAI2015", "PARKYANG2013", "KOPCHADI2016"), ]
focus.list <- focus.list[!focus.list$manuscriptID %in% c("TAI2015", "PARKYANG2013", "KOPCHADI2016"), ]

# Combine frames and merge by the Manuscript ID
overall.frame <- merge(descriptors.frame, methods.frame, by.x = "manuscriptID")

# Set global minimum cut value
cut.min <<- 0
color.palette <<- c("#990000", "#DC8823", "#285C4D", "#01426A", "#512A44", "#83786F", "#191919")
color.palette.light <<- c("#990000", "#F1BE48", "#008264", "#006298", "#66435A", "#ACA39A", "#4A3C31")
#color.palette <<- c("#01426A", "#DC8823", "#285C4D", "#770000")
bg.color <<-"#EEEEEE"
line.color <<- "#83786F"
font.choice <<- "Noto Mono"
seed.value <<- 4.6692


#descriptors.OK.list <- colnames(descriptors.frame)[2:ncol(descriptors.frame)]
#methods.OK.list <- colnames(methods.frame)[2:ncol(methods.frame)]

community.set <<- data.frame(group = character(), descriptor = character(),
                             community = character(), method = character(),
                             mod = numeric(), stringsAsFactors = FALSE)

community.el <<- data.frame(manuscriptID.x = character(),
                            manuscriptID.y = character(),
                            modularity = numeric(),
                            stringsAsFactors = FALSE)

trial.table <<- data.frame(trial = character(),
                           noClusters = integer(),
                          modularity = integer(), stringsAsFactors = FALSE)

# OVERALL
overall.long <- prepare.long(overall.frame)
overall.count <- do.special.count(overall.frame)
####
overall.count <<- subset(overall.count, n > 1)
overall.long <- subset(overall.long, overall.long$variable %in% overall.count$value)
####
overall.count.quant <- quantile(overall.count$n)
overall.reduced.filter <- subset(overall.count, (n >= overall.count.quant[4]))
#overall.reduced.filter <- rbind(overall.reduced.filter, "self efficacy")
overall.reduced.long <- subset(overall.long, (value %in% overall.reduced.filter$value))
count.graph <- ggplot(overall.count, aes(x = reorder(value, -n), y = n)) +
  geom_bar(stat = "identity", color = "gray40") + theme_minimal() +
  geom_hline(yintercept = overall.count.quant[4], color = "black", linetype = "dashed") + theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(text = element_text(size = 6, family = "Fira Code")) + ylab("Count") + xlab("Descriptors")
output.plot(count.graph, 300, "results/descriptor-cutoff.tif")

#overall.reduced.se.filter <- rbind(overall.reduced.filter, "self efficacy")
#overall.reduced.se.long <- subset(overall.long, (value %in% overall.reduced.se.filter$value))


overall.reduced.list <- overall.reduced.long
overall.reduced.list$value <- NULL
overall.reduced.list$variable <- NULL
overall.reduced.list <- as.data.frame(unique(overall.reduced.list))

overall.leftout.long <- subset(overall.long, !(manuscriptID %in% overall.reduced.list$manuscriptID))

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

# Split the overall data into STEM and no-STEM sets for analysis
split.frame <- split(overall.frame, overall.frame$stem_education)
stem.frame <- split.frame$stem_education
stem.frame$stem_education <- NULL
no_stem.frame <- split.frame$no_stem_education
no_stem.frame$stem_education <- NULL

####################################
# Prepare data frames for analysis #
####################################

tpack.long <- prepare.long(tpack.frame)
no_tpack.long <- prepare.long(no_tpack.frame)
attitude.long <- prepare.long(attitudes.frame)
no_attitude.long <- prepare.long(no_attitudes.frame)
survey.long <- prepare.long(survey.frame)
no_survey.long <- prepare.long(no_survey.frame)
stem.long <- prepare.long(stem.frame)
no_stem.long <- prepare.long(no_stem.frame)

descriptors.long <- prepare.long(descriptors.frame)
methods.long <- prepare.long(methods.frame)

######################
#                    #
#  CONDUCT ANALYSES  #
#                    #
######################


############################
# Conduct cluster analysis #
############################

do.cluster(overall.long, "all")
do.cluster(overall.reduced.long, "reduced")
do.cluster(tpack.long, "TPACK")
do.cluster(no_tpack.long, "non-TPACK")
do.cluster(attitude.long, "attitude")
do.cluster(no_attitude.long, "non-attitude")
do.cluster(survey.long, "survey")
do.cluster(no_survey.long, "non-survey")
do.cluster(stem.long, "stem")
do.cluster(no_stem.long, "non-stem")

do.cluster(descriptors.long, "descriptors")
do.cluster(methods.long, "methods")

useable.el <- data.frame(x = community.el$manuscriptID.x, y = community.el$manuscriptID.y)
useable.el <- as.matrix(useable.el)
useable.weights <- data.frame(modularity = community.el$modularity)
useable.weights$modularity <- abs(scale(useable.weights$modularity, center = TRUE))


community.graph <- simplify(graph_from_edgelist(useable.el, directed = FALSE),
                            remove.loops = TRUE, remove.multiple = FALSE)
set.seed(seed.value)
community.clusters <- cluster_louvain(community.graph, weights = useable.weights$modularity)

community.overall <- data.frame(manuscriptID = community.clusters$names, community = community.clusters$membership)

community.authority <- data.frame(authority.score(community.graph)$vector)
community.authority$manuscriptID <- rownames(community.authority)
rownames(community.authority) <- NULL
colnames(community.authority)[1] <- "authority"

community.between <- data.frame(betweenness(community.graph))
community.between$manuscriptID <- rownames(community.between)
rownames(community.between) <- NULL
colnames(community.between)[1] <- "betweenness"
community.between$betweenness <- (scale(community.between$betweenness, center = FALSE)) + 1


community.overall <- merge(community.overall, community.authority)
community.overall <- merge(community.overall, community.between)
#colnames(community.overall)[3] <- "authority"
community.overall$focus <- ifelse(community.overall$manuscriptID %in% focus.list$manuscriptID, "articles", "keywords")


community.focus <- subset(community.overall, (manuscriptID %in% focus.list$manuscriptID))

#tiff("treemap1.tif", width = 11, height = 8.5, units = "in", res = 600)
treemap(community.overall, index = c("community", "focus", "manuscriptID"), vSize = "betweenness",
        palette = color.palette.light, title = "Articles and Descriptors in Clusters", force.print.labels = FALSE,
        aspRatio = 11/8.5)
#dev.off()
treemap(community.focus, index = c("community", "manuscriptID"), vSize = "betweenness",
        palette = color.palette.light, title = "Articles in Clusters", force.print.labels = TRUE,
        aspRatio = 11/8.5)

treegraph(community.overall, index = c("community", "focus", "manuscriptID"), directed = FALSE,
          show.labels = TRUE, rootlabel = "")

treegraph(community.focus, index = c("community", "manuscriptID"), directed = FALSE,
          show.labels = TRUE, rootlabel = "")#, vertex.size = 2.5*as.vector(community.focus$betweenness))


###################################
# Conduct correspondence analysis #
###################################

do.MCA(overall.long, "all")
do.MCA(overall.reduced.long, "reduced")
#do.MCA(overall.reduced.se.long, "reduced with self-efficacy")
do.MCA(tpack.long, "TPACK")
do.MCA(no_tpack.long, "non-TPACK")
do.MCA(attitude.long, "attitude")
do.MCA(no_attitude.long, "non-attitude")
do.MCA(survey.long, "survey")
do.MCA(no_survey.long, "non-survey")
do.MCA(stem.long, "stem")
do.MCA(no_stem.long, "non-stem")

do.MCA(descriptors.long, "descriptors")
do.MCA(methods.long, "methods")

##########################################
# Conduct consensus modeling of clusters #
##########################################

# Prepare the data set for manipulation and analysis
community.weighted <- data.frame(community.set)
community.weighted <- data.frame(lapply(community.weighted, as.character), stringsAsFactors=FALSE)

# Add an ID column to create unique rows
csle <- rle(community.rank$descriptor)
csids <- rep(seq_len(length(csle$values)), times=csle$lengths)
community.weighted <- as.data.frame(cbind(id=csids, community.weighted[1:4]))

# Make sure that the columns are in the correct format for use
community.weighted <- data.frame(lapply(community.weighted, as.character), stringsAsFactors=FALSE)
community.weighted$mod <- as.numeric(community.weighted$mod)

# Aggregate cases in order to make use of modularity as a weight to determine the clusters
# for the descriptors
community.ac <- wcAggregateCases(community.weighted[1:4], weights = community.weighted$mod)

## Retrieve unique cases
unique.clusters <- community.weighted[community.ac$aggIndex, ]

## Create a table
table.orig <- xtabs(community.weighted$mod ~ descriptor + community, data=community.weighted)

# Convert the table back into a data frame for easier manipulation
community.frame <- as.data.frame.matrix(table.orig)

# Identify the highest score in order to identify the most appropriate cluster
community.highest <- colnames(community.frame)[max.col(community.frame, ties.method="first")]

# Create a data frame out of this information
community.descriptors.weighted <- data.frame(descriptor = unique(community.weighted$descriptor), community = community.highest)
colnames(community.descriptors.weighted)[2] <- "weightedcommunity"

# Combine the most appropriate clusters back onto the list of manuscripts based on descriptors
manuscript.consensus.frame <- overall.reduced.long
manuscript.consensus.frame$variable <- NULL
colnames(manuscript.consensus.frame)[2] <- "descriptor"
manuscript.consensus.frame <- merge(manuscript.consensus.frame, community.descriptors.weighted, by.x = "descriptor")
manuscript.consensus.frame <- data.frame(manuscript.consensus.frame)

# Turn this into a matrix in order to run through cultural consensus analysis
cs2 <- reshape(manuscript.consensus.frame, timevar = "manuscriptID", v.names = "weightedcommunity", idvar = "descriptor",
               direction = "wide")
cs2$descriptor <- NULL
colnames(cs2) <- gsub("weightedcommunity\\.", "", colnames(cs2))
cs2 <- as.matrix(sapply(cs2, as.numeric))
cs2[is.na(cs2)] <- 0

# Identify the maximum number of clusters possible
max.community <- max(as.integer(community.descriptors.weighted$weightedcommunity))

# Run through cultural consensus analysis
manuscript.weighted.consensus <- ConsensusPipeline(cs2, max.community, safetyOverride = FALSE, ComreyFactorCheck = FALSE)

# Send output to a file for future reference
sink(file = "results/manuscript-weighted_consensus.txt", append = FALSE)
print(manuscript.weighted.consensus)
sink()

# Compile a list of manuscripts matched to cluster
manuscript.weighted.list <- as.data.frame(manuscript.weighted.consensus$Answers)
manuscript.weighted.list$manuscriptID <- rownames(manuscript.weighted.list)
rownames(manuscript.weighted.list) <- NULL
colnames(manuscript.weighted.list)[1] <- "community"
manuscript.weighted.list <- data.frame(manuscript.weighted.list)
manuscript.weighted.list <- manuscript.weighted.list[order(manuscript.weighted.list[2], manuscript.weighted.list[1]),] 

# Send the list to a file for future reference
sink(file = "results/manuscript-manuscript_weighted-list.txt", append = FALSE)
print(manuscript.weighted.list)
sink()

# Create an descriptor-manuscript consensus frame

# Retrieve the list of manuscripts
temp.manuscript <- overall.reduced.long
temp.manuscript$variable <- NULL
colnames(temp.manuscript)[2] <- "descriptor"

# Merge the list of manuscripts with the descriptor cluster list
overall.consensus <- merge(community.descriptors.weighted, temp.manuscript, by.x = "descriptor")
colnames(overall.consensus)[2] <- "descriptor.community"

# Merge with the list of manuscript clusters
overall.consensus <- merge(overall.consensus, manuscript.weighted.list, by.x = "manuscriptID")
colnames(overall.consensus)[4] <- "manuscript.community"

# Remove any duplicate rows
overall.consensus <- overall.consensus[!duplicated(overall.consensus), ]

#########################
# Create alluvial plots #
#########################

alluvial.frame <- data.frame(descriptor.community = overall.consensus$descriptor.community,
                             descriptor = overall.consensus$descriptor,
                             manuscript = overall.consensus$manuscriptID,
                             manuscript.community = overall.consensus$manuscript.community)

alluvial.all.plot <- ggplot(as.data.frame(alluvial.frame),
       aes(axis1 = descriptor.community,
           axis2 = reorder(descriptor, descriptor.community),
           axis3 = reorder(manuscript, manuscript.community),
           axis4 = manuscript.community)) +
  geom_alluvium(aes(fill = manuscript.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:4, labels = c("descriptor cluster", "descriptor", "manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

alluvial.descriptor.plot <- ggplot(as.data.frame(alluvial.frame),
                            aes(axis1 = descriptor.community,
                                axis2 = reorder(descriptor, descriptor.community),
                                axis3 = manuscript.community)) +
  geom_alluvium(aes(fill = descriptor.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:3, labels = c("descriptor cluster", "descriptor", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

alluvial.manuscript.plot <- ggplot(as.data.frame(alluvial.frame),
                            aes(axis1 = descriptor.community,
                                axis2 = reorder(manuscript, manuscript.community),
                                axis3 = manuscript.community)) +
  geom_alluvium(aes(fill = manuscript.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:3, labels = c("descriptor cluster", "manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

alluvial.manuscript.only.plot <- ggplot(as.data.frame(alluvial.frame),
                                   aes(axis1 = reorder(manuscript, manuscript.community),
                                       axis2 = manuscript.community)) +
  geom_alluvium(aes(fill = manuscript.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:2, labels = c("manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

alluvial.descriptor.only.plot <- ggplot(as.data.frame(alluvial.frame),
                                   aes(axis1 = descriptor.community,
                                       axis2 = reorder(descriptor, descriptor.community))) +
  geom_alluvium(aes(fill = descriptor.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:2, labels = c("descriptor cluster", "descriptor")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

alluvial.community.plot <- ggplot(as.data.frame(alluvial.frame),
                                        aes(axis1 = descriptor.community,
                                            axis2 = manuscript.community)) +
  geom_alluvium(aes(fill = descriptor.community), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:2, labels = c("descriptor cluster", "descriptor")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

output.plot(alluvial.all.plot, 600, "results/alluvial-all-plot.tif")
output.plot(alluvial.descriptor.plot, 600, "results/alluvial-descriptor-plot.tif")
output.plot(alluvial.manuscript.plot, 600, "results/alluvial-manuscript-plot.tif")
output.plot(alluvial.descriptor.only.plot, 600, "results/alluvial-descriptor_only-plot.tif")
output.plot(alluvial.manuscript.only.plot, 600, "results/alluvial-manuscript_only-plot.tif")
output.plot(alluvial.community.plot, 600, "results/alluvial-community-plot.tif")
output.plot(alluvial.wnw.plot, 600, "results/alluvial-wnw-plot.tif")

cluster.1.frame <- subset(alluvial.frame, alluvial.frame$manuscript.community == 1)
alluvial.cluster.1.plot <- ggplot(as.data.frame(cluster.1.frame),
                                   aes(#axis1 = descriptor.community,
                                       axis1 = reorder(descriptor, descriptor.community),
                                       axis2 = manuscript)) +
  geom_alluvium(aes(fill = descriptor), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:3, labels = c("descriptor cluster", "manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

cluster.2.frame <- subset(alluvial.frame, alluvial.frame$manuscript.community == 2)
alluvial.cluster.2.plot <- ggplot(as.data.frame(cluster.2.frame),
                                  aes(#axis1 = descriptor.community,
                                      axis1 = reorder(descriptor, descriptor.community),
                                      axis2 = manuscript)) +
  geom_alluvium(aes(fill = descriptor), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:3, labels = c("descriptor cluster", "manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

cluster.3.frame <- subset(alluvial.frame, alluvial.frame$manuscript.community == 1)
alluvial.cluster.3.plot <- ggplot(as.data.frame(cluster.3.frame),
                                  aes(#axis1 = descriptor.community,
                                      axis1 = reorder(descriptor, descriptor.community),
                                      axis2 = manuscript)) +
  geom_alluvium(aes(fill = descriptor), width = 1/2) +
  geom_stratum(width = 1/2, fill = bg.color, color = line.color) +
  geom_text(aes(family = font.choice), stat = "stratum") +
  theme(text=element_text(size = 13, family = font.choice)) +
  scale_x_continuous(breaks = 1:3, labels = c("descriptor cluster", "manuscript", "manuscript cluster")) +
  theme_minimal() + guides(fill=FALSE, color = FALSE) + 
  ggtitle("Alluvial plot of clusters") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.text.x = element_text(size = 10, family = font.choice)) +
  theme(title = element_text(family = font.choice))

output.plot(alluvial.cluster.1.plot, 600, "results/alluvial-cluster_1-plot.tif")
output.plot(alluvial.cluster.2.plot, 600, "results/alluvial-cluster_2-plot.tif")
output.plot(alluvial.cluster.3.plot, 600, "results/alluvial-cluster_3-plot.tif")

####################################
#                                  #
#  PREPARE WORD COMPARISON GRAPHs  #
#                                  #
####################################


do.compare.graph(tpack.frame, no_tpack.frame, "TPACK", "non-TPACK")
do.compare.graph(attitudes.frame, no_attitudes.frame, "attitude", "non-attitude")
do.compare.graph(survey.frame, no_survey.frame, "survey", "non-survey")
do.compare.graph(artifact.frame, no_artifact.frame, "artifact", "non-artifact")
do.compare.graph(stem.frame, no_stem.frame, "STEM", "non-STEM")

