#########
# TO DO #
#########

# * Functionize
# - split tpack and non-tpack using filter(df, fct %in% vc) see http://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector
# * include all analytic functions from other file :-)
# * 

#############
# LIBRARIES #
#############

# Load libraries to analyze data
library(FactoMineR)
library(factoextra)
library(CAinterprTools)
library(dplyr)
library(readr)
library(ClustOfVar)
#library(data.table)
#library(PCAmixdata)
library(cluster)
library(dendextend)
library(NbClust)


##############################
# LOAD AND PREPARE DATA SETS #
##############################

# Load in data sets from GitHub
keywords.frame <- read.csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/lit_review-keywords-processed.csv", header = TRUE)
methods.frame <- read.csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/lit_review-data_sources-survey_combined.csv", header = TRUE)

# Combine frames and merge by the Manuscript ID
overall.frame <- merge(keywords.frame, methods.frame, by.x = "manuscriptID")

# Set row names to the Manuscript IDs
row.names(overall.frame) <- overall.frame$manuscriptID
overall.frame$manuscriptID <- NULL

overall.frame <- data.frame(lapply(overall.frame, as.character), stringsAsFactors=FALSE)

# Split the overall data into TPACK and non-TPACK sets for analysis
tpack.frame <- overall.frame[overall.frame$tpack %in% "tpack",]
tpack.frame$tpack <- NULL
non_tpack.frame <- overall.frame[overall.frame$tpack %in% "no_tpack",]
non_tpack.frame$tpack <- NULL

prepare.cluster <- function(data.set){
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

overall.cluster <- prepare.cluster(overall.frame)
tpack.cluster <- prepare.cluster(tpack.frame)
non_tpack.cluster <- prepare.cluster(non_tpack.frame)

####################
# GLOBAL VARIABLES #
####################

max.dim <<- 3
OK.list <<- colnames(overall.frame) # List of the column names, as these will be displayed on the biplot

cluster.analysis <- function(data.set) {
  # Dissimilarity matrix
  d <- dist(data.set, method = "euclidean")
  # Hierarchical clustering using Ward's method
  res.hc <- hclust(d, method = "ward.D2" )
  # Plot the obtained dendrogram
  plot(res.hc, cex = 0.6, hang = -1)

  # Compute agnes()
  res.agnes <- agnes(data.set, method = "ward")
  # Agglomerative coefficient
  res.agnes$ac
  
  # Plot the tree using pltree()
  pltree(res.agnes, cex = 0.6, hang = -1,
         main = "Dendrogram of agnes")
  
  # K-means clustering
  set.seed(123)
  km.res <- kmeans(data.set, 3, nstart = 25)
  # k-means group number of each observation
  km.res$cluster
  set.seed(123)
  # Compute and plot wss for k = 2 to k = 15
  k.max <- 15 # Maximal number of clusters
  data <- data.set
  wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=10 )$tot.withinss})
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  abline(v = 3, lty =2)
  
  k.max <- 15
  data <- data.set
  sil <- rep(0, k.max)
  # Compute the average silhouette width for 
  # k = 2 to k = 15
  for(i in 2:k.max){
    km.res <- kmeans(data, centers = i, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(data))
    sil[i] <- mean(ss[, 3])
  }
  # Plot the  average silhouette width
  plot(1:k.max, sil, type = "b", pch = 19, 
       frame = FALSE, xlab = "Number of clusters k")
  cut.data <- 7
  abline(v = cut.data, lty = 2)

  
  # Cut tree into 10 groups
  grp <- cutree(res.hc, k = cut.data)
  # Number of members in each cluster
  print(table(grp))
  
  for(i in 1:cut.data) {
    print(rownames(overall.cluster)[grp == i])
  }
  
  plot(res.hc, cex = 0.6)
  rect.hclust(res.hc, k = 3, border = 2:5)
  
  #num.cluster <- fviz_nbclust(data.set, FUNcluster = hcut, method = "silhouette", k.max = 15)
  #print(num.cluster)

  set.seed(123)
  res.nb <- NbClust(data.set, distance = "euclidean",
                    min.nc = 2, max.nc = 15, 
                    method = "complete", index ="gap") 
  print(res.nb) # print the results
  print(fviz_nbclust(res.nb) + theme_minimal())
  # K-means clustering
  set.seed(123)
  km.res <- kmeans(data.set, 3, nstart = 25)
  # k-means group number of each observation
  print(km.res$cluster)
  
  pam.res <- pam(data.set, 3)
  print(pam.res$cluster)
  print(fviz_cluster(pam.res, stand = FALSE, geom = "point",
               frame.type = "norm"))
}

cluster.analysis(non_tpack.cluster)
cluster.analysis(tpack.cluster)
cluster.analysis(overall.cluster)

# Cut tree into 10 groups
grp <- cutree(res.hc, k = 10)
# Number of members in each cluster
table(grp)

rownames(overall.cluster)[grp == 1]
rownames(overall.cluster)[grp == 2]
rownames(overall.cluster)[grp == 3]

plot(res.hc, cex = 0.6)
rect.hclust(res.hc, k = 9, border = 2:5)

res.cor <- cor(t(overall.cluster), method = "pearson")
d.cor <- as.dist(1 - res.cor)
plot(hclust(d.cor, method = "ward.D2"), cex = 0.6)

library(dendextend)
# Compute distance matrix
res.dist <- dist(overall.cluster, method = "euclidean")
# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Create a list of dendrograms
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

library(cluster)
library(NbClust)


pam.res <- pam(overall.cluster, 3)
pam.res$cluster
fviz_cluster(pam.res, stand = FALSE, geom = "point",
             frame.type = "norm")




overall.scale <- t(overall.scale)

overall.recode <- t(overall.recode)

# run variable clustering excluding the target variable (churn)
variable_tree <- hclustvar(X.quanti = overall.scale)
#plot the dendrogram of variable groups
plot(variable_tree)
part <- cutreevar(variable_tree, 10)
# requesting for 25 bootstrap samplings and a plot
stab <- stability(variable_tree, B = 25)

data(decathlon)
tree <- hclustvar(X.quanti = decathlon[,1:10])
stab <- stability(tree, B = 20)
plot(stab, nmax = 7)


####################
#    FUNCTIONS     #
####################

# Define function for writing plots to a file
output.plot <- function(plot.output, plot.resolution, file.name){
  tiff(file.name, width = 11, height = 8.5, units = "in", res = plot.resolution)
  print(plot.output)
  dev.off()
}

# Contribution to dimension plots function
dimension.plot <- function(analysis.MCA, analysis.type, analysis.dim, dim.filename){
  dim.title <- str_c(analysis.type, " Dimension ", analysis.dim, " Contributions")
  dim.filename <- str_c("output-", analysis.stem, "-dim1.tif")
    dim.plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 1, fill = "gray40", 
                                    color = "black") + theme_minimal()
  dim.plot <- dim.plot + labs(title = dim.title)
  # Output contribution to dimension 1 plot
  output.plot(dim.plot, 300, dim.filename)
}

dimension.summary <- function(analysis.type) {
  analysis.dimsum.filename <- str_c("output-", analysis.type, "-dimsum.txt")
  # Create dimension summary
  analysis.dimsum <- dimdesc(analysis.MCA, axes = 1:max.dim, proba = 0.05)
  # Output dimension summary file
  sink(file = analysis.dimsum.filename, append = FALSE)
  print(analysis.dimsum)
  sink()
}

# Create scree plot function
scree.plot <- function(analysis.MCA, analysis.type) {
  screeplot.title <- str_c("Variances: ", analysis.title)
  screeplot.filename <- 
  # Build the scree plot
  analysis.screeplot <- fviz_screeplot(analysis.MCA, addlabels = TRUE, barfill = "gray40", 
                                     barcolor = "gray40", linecolor = "black") + theme_minimal()
  analysis.screeplot <- analysis.screeplot + labs(title = screeplot.title)
  # Output the scree plot
  output.plot(analysis.screeplot, 300, screeplot.filename)
}

bi.plot <- function(analysis.MCA, analysis.type, max.dim) {
  analysis.biplot.cols.filename <- str_c("output-", analysis.stem, "-biplot_cols.tif")
  analysis.biplot.filename <- str_c("output-", analysis.stem, "-biplot.tif")
  analysis.biplot.cols <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
                                       axes = c(1,2), select.var = list(name = sources.list),
                                       col.var = "black") + theme_minimal()
  analysis.biplot.cols <- analysis.biplot.cols + labs(title = analysis.biplot.cols.title)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(1,2), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = list(name = sources.list)) + theme_minimal()
  analysis.biplot <- analysis.biplot + labs(title = analysis.biplot.title)
  # Output biplots
  output.plot(analysis.biplot.cols, 300, analysis.biplot.cols.filename)
  output.plot(analysis.biplot, 600, analysis.biplot.filename)
}

# Define function for analysis
review.analysis <- function(analysis.frame, analysis.title, analysis.stem, max.dim){
  max.dim <- as.numeric(max.dim)

  # Calculate Multiple Correspondence Analysis
  analysis.MCA <- MCA(analysis.frame, na.method = "NA", graph = FALSE)
  
  # Build titles
  analysis.screeplot.title <- str_c("Variances: ", analysis.title)
  analysis.dim1plot.title <- str_c(analysis.title, " Dimension 1 Contributions")
  analysis.dim2plot.title <- str_c(analysis.title, " Dimension 2 Contributions")
  analysis.biplot.cols.title <- str_c(analysis.title, " Variables")
  analysis.biplot.title <- str_c(analysis.title)
  
  # Build file names
  analysis.screeplot.filename <- str_c("output-", analysis.stem, "-screeplot.tif")
  analysis.dimsum.filename <- str_c("output-", analysis.stem, "-dimsum.txt")
  analysis.dim1.filename <- str_c("output-", analysis.stem, "-dim1.tif")
  analysis.dim2.filename <- str_c("output-", analysis.stem, "-dim2.tif")
  analysis.biplot.cols.filename <- str_c("output-", analysis.stem, "-biplot_cols.tif")
  analysis.biplot.filename <- str_c("output-", analysis.stem, "-biplot.tif")
  
  # Create scree plot
  # Build the scree plot
  analysis.screeplot <- fviz_screeplot(analysis.MCA, addlabels = TRUE, barfill = "gray40", 
                                       barcolor = "gray40", linecolor = "black") + theme_minimal()
  analysis.screeplot <- analysis.screeplot + labs(title = analysis.screeplot.title)
  # Output the scree plot
  output.plot(analysis.screeplot, 300, analysis.screeplot.filename)
  
  # Create dimension summary
  analysis.dimsum <- dimdesc(analysis.MCA, axes = 1:max.dim, proba = 0.05)
  # Output dimension summary file
  sink(file = analysis.dimsum.filename, append = FALSE)
  print(analysis.dimsum)
  sink()
  
  # Create contribution to dimension 1 plot
  analysis.dim1plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 1, fill = "gray40", 
                                    color = "black") + theme_minimal()
  analysis.dim1plot <- analysis.dim1plot + labs(title = analysis.dim1plot.title)
  # Output contribution to dimension 1 plot
  output.plot(analysis.dim1plot, 300, analysis.dim1.filename)
  
  # Create contribution to dimension 2 plot
  analysis.dim2plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 2, fill = "gray40", 
                                    color = "gray40") + theme_minimal()
  analysis.dim2plot <- analysis.dim2plot + labs(title = analysis.dim2plot.title)
  # Output contribution to dimension 2 plot
  output.plot(analysis.dim2plot, 300, analysis.dim2.filename)
  
  #If necessary, create contribution to dimension 3 plot
  if(max.dim == 3){
    analysis.dim3plot.title <- str_c(analysis.title, " Dimension 3 Contributions")
    analysis.dim3.filename <- str_c("output-", analysis.stem, "-dim3.tif")
    analysis.dim3plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 3, fill = "gray40", 
                                      color = "gray40") + theme_minimal()
    analysis.dim3plot <- analysis.dim3plot + labs(title = analysis.dim3plot.title)
    # Output contribution to dimension 2 plot
    output.plot(analysis.dim3plot, 300, analysis.dim3.filename)
  }
  
  # Create biplots
  analysis.biplot.cols <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
                                       axes = c(1,2), select.var = list(name = sources.list),
                                       col.var = "black") + theme_minimal()
  analysis.biplot.cols <- analysis.biplot.cols + labs(title = analysis.biplot.cols.title)
  analysis.biplot <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(1,2), col.ind = "gray40",
                                     col.var = "black", labelsize = 2, 
                                     select.var = list(name = sources.list)) + theme_minimal()
  analysis.biplot <- analysis.biplot + labs(title = analysis.biplot.title)
  # Output biplots
  output.plot(analysis.biplot.cols, 300, analysis.biplot.cols.filename)
  output.plot(analysis.biplot, 600, analysis.biplot.filename)
  if(max.dim == 3) {
    analysis.biplot.cols13.title <- str_c(analysis.title, " Variables for Dimensions 1 & 3")
    analysis.biplot13.title <- str_c(analysis.title, " for Dimensions 1 & 3")
    analysis.biplot.cols13.filename <- str_c("output-", analysis.stem, "-biplot_cols13.tif")
    analysis.biplot13.filename <- str_c("output-", analysis.stem, "-biplot13.tif")
    analysis.biplot.cols23.title <- str_c(analysis.title, " Variables for Dimensions 2 & 3")
    analysis.biplot23.title <- str_c(analysis.title, " for Dimensions 2 & 3")
    analysis.biplot.cols23.filename <- str_c("output-", analysis.stem, "-biplot_cols23.tif")
    analysis.biplot23.filename <- str_c("output-", analysis.stem, "-biplot23.tif")
    analysis.biplot.cols13 <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
                                           axes = c(1,3), select.var = list(name = sources.list),
                                           col.var = "black") + theme_minimal()
    analysis.biplot.cols13 <- analysis.biplot.cols13 + labs(title = analysis.biplot.cols13.title)
    analysis.biplot13 <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                         axes = c(1,3), col.ind = "gray40",
                                         col.var = "black", labelsize = 2, 
                                         select.var = list(name = sources.list)) + theme_minimal()
    analysis.biplot13 <- analysis.biplot13 + labs(title = analysis.biplot13.title)
    output.plot(analysis.biplot.cols13, 300, analysis.biplot.cols13.filename)
    output.plot(analysis.biplot13, 600, analysis.biplot13.filename)
    analysis.biplot.cols23 <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
                                           axes = c(2,3), select.var = list(name = sources.list),
                                           col.var = "black") + theme_minimal()
    analysis.biplot.cols23 <- analysis.biplot.cols23 + labs(title = analysis.biplot.cols23.title)
    analysis.biplot23 <- fviz_mca_biplot(analysis.MCA, geom = c("text"), repel = TRUE,
                                         axes = c(2,3), col.ind = "gray40",
                                         col.var = "black", labelsize = 2, 
                                         select.var = list(name = sources.list)) + theme_minimal()
    analysis.biplot23 <- analysis.biplot23 + labs(title = analysis.biplot23.title)
    output.plot(analysis.biplot.cols23, 300, analysis.biplot.cols23.filename)
    output.plot(analysis.biplot23, 600, analysis.biplot23.filename)
  }
}





############
# ANALYSIS #
############

# Calculate Multiple Correspondence Analysis
analysis.MCA <- MCA(analysis.frame, na.method = "NA", graph = FALSE)

# Create scree plot
# Build the scree plot
analysis.screeplot <- fviz_screeplot(analysis.MCA, addlabels = TRUE, barfill = "gray40", 
                                     barcolor = "gray40", linecolor = "black") + theme_minimal()
analysis.screeplot <- analysis.screeplot + labs(title = analysis.screeplot.title)
# Output the scree plot
output.plot(analysis.screeplot, 300, "results/combined-screeplot.tif")

# Create contribution to dimension 1 plot
analysis.dim1plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 1, fill = "gray40", 
                                  color = "black") + theme_minimal()
analysis.dim1plot <- analysis.dim1plot + labs(title = analysis.dim1plot.title)
# Output contribution to dimension 1 plot
output.plot(analysis.dim1plot, 300, "results/combined-dim1.tif")

# Create contribution to dimension 2 plot
analysis.dim2plot <- fviz_contrib(analysis.MCA, choice = "var", axes = 2, fill = "gray40", 
                                  color = "gray40") + theme_minimal()
analysis.dim2plot <- analysis.dim2plot + labs(title = analysis.dim2plot.title)
# Output contribution to dimension 2 plot
output.plot(analysis.dim2plot, 300, "results/combined-dim2.tif")


# Create biplots
analysis.biplot.cols <- fviz_mca_var(analysis.MCA, geom = c("text"), repel = TRUE,
                                     axes = c(1, 2), select.var = list(name = OK.list),
                                     col.var = "black") + theme_minimal()
analysis.biplot.cols <-
  analysis.biplot.cols + labs(title = analysis.biplot.cols.title)
analysis.biplot <-
  fviz_mca_biplot(
    analysis.MCA,
    geom = c("text"),
    repel = TRUE,
    axes = c(1, 2),
    col.ind = "gray40",
    col.var = "black",
    labelsize = 2,
    select.var = list(name = OK.list)
  ) + theme_minimal()
#analysis.biplot <-
#  analysis.biplot + labs(title = analysis.biplot.title)
#print(analysis.biplot)
output.plot(analysis.biplot, 300, "results/combined-biplot.tif")
