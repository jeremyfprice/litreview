  #######################
  #                     #
  #  LOAD IN LIBRARIES  #
  #                     #
  #######################
  
  
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
  #library(linkcomm)
  #library(AnthroTools)
  #library(ggalluvial)
  #library(WeightedCluster)
  library(treemap)
  library(sna)
  #library(dbscan)
  library(fpc)
  library(cba)
  
  
  ######################
  #                    #
  #  DEFINE FUNCTIONS  #
  #                    #
  ######################
  
  # Filter through the data sets
  filter.descriptors <- function(data.set){
    
    data.set <- descriptors.frame
    
    # Remove stopwords, which will not be used in the analysis
    descriptors.frame <- subset(descriptors.frame, !(variable %in% stop.words))
    
    descriptors.frame <- merge(descriptors.frame, filter.map, by = "variable", all.x = TRUE)
    descriptors.frame$substitute <- ifelse(is.na(descriptors.frame$substitute), descriptors.frame$variable,
                                           descriptors.frame$substitute)
    descriptors.frame$variable <- descriptors.frame$substitute
    descriptors.frame$substitute <- NULL
    
    # Remove any duplicated rows to clean up the data set
    descriptors.frame <- descriptors.frame[!duplicated(descriptors.frame), ]
    
    # Send back the data set
    return(data.set)
    
  }
  
  make.id.list <- function(data.set) {
    data.set$value <- NULL
    data.set$variable <- NULL
    data.set <- as.data.frame(unique(data.set))
    return(data.set)
  }
  
  remove.no <- function(data.set, colno) {
    # Set any no value to NA
    data.set[,colno][grep("no ", data.set[,colno])] <- NA
    # Remove NAs
    data.set <- na.omit(data.set)
    # Return the data set
    return(data.set)
  }
  
  count.long <- function(data.long) {
    data.count <- data.long %>% 
      group_by(value) %>% 
      count(sort = TRUE)
    return(data.count)
  }
  
  # Convert dataframe from wide to long format (necessary for working with the data)
  prepare.long <- function(data.set){
    # Turn off warnings, which interrupt the flow without saying much
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
  
  ego.effective.size <- function(g, ego, ...) {
    egonet <- induced.subgraph(g, neighbors(g, ego, ...))
    n <- vcount(egonet)
    t <- ecount(egonet)
    return(n - (2 * t) / n)
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
    data.set <- overall.long
    data.set$variable <- NULL
    data.set[,2][grep("no ", data.set[,2])] <- NA
    data.set <- na.omit(data.set)
    data.set$manuscriptID <- toupper(data.set$manuscriptID)
    data.set <- as.matrix(data.set)
    data.graph <- simplify(graph_from_edgelist(data.set, directed = FALSE),
                              remove.loops = TRUE, remove.multiple = TRUE)
    data.network <- network(data.set)
    #plot(data.graph, edge.curved=.1, vertex.shape = "none", vertex.label.cex=.7,
    #     layout=layout_with_mds)
    #data.infocent <- infocent(data.network)
    #trial.table <- data.frame(trialNo = integer(), noClusters = integer(),
    #                          modularity = integer())
    #data.hub <- hub_score(data.graph)
    #data.authority <- authority_score(data.graph)
    set.seed(seed.value)
    data.community <- cluster_louvain(data.graph)
    data.modularity <- modularity(data.community)
    data.clusters <<- data.frame(manuscriptID = toupper(data.community$names), 
                                 tribe = data.community$membership)#,
                                 #mod = data.modularity, ic = data.infocent)

    #data.clusters$ic <- 1/abs(scale(data.clusters$ic, center = FALSE))
    for(i in 1:nrow(data.clusters)){
      data.clusters[i,3] <- ego.effective.size(data.graph, data.clusters[i,1])
    }
    colnames(data.clusters)[3] <- "es"
    
    rownames(data.clusters) <- NULL
    
    data.clusters <- merge(data.clusters, cluster.name.frame)
    
    data.clusters$status <- ifelse(data.clusters$manuscriptID %in% focus.list$manuscriptID,
                                   "articles", "descriptors")
    
    data.clusters$manuscriptID <- ifelse(data.clusters$status == "descriptors",
                                         tolower(data.clusters$manuscriptID),
                                         toupper(data.clusters$manuscriptID))
    
    just.articles <<- subset(data.clusters, manuscriptID %in% focus.list$manuscriptID)
    
    
    #new.row <- data.frame(trial = data.title, noClusters = max(data.community$membership), 
    #                      modularity = data.modularity)
    #trial.table <<- rbind(trial.table, new.row)
    
    article.list <- merge(just.articles, focus.list) %>%
      arrange(tribe, desc(es), manuscriptID)
    
    write_csv(article.list, "cluster-articles.csv")
    
    library(RColorBrewer)
    #library(colorspace)
    palette.brewed <- brewer.pal(7, "Dark2")
    #palette.brewed <- palette_pander(10, random_order = TRUE)
    
    tiff("overall-treemap.tif", width = 9, height = 6.5, units = "in", res = 300)
      treemap(data.clusters, c("tribe_name", "status", "manuscriptID"), vSize = "es",
              fontsize.labels = c(12, 8, 4),
              fontfamily.title = font.choice,
              fontfamily.labels = font.choice,
              title = "",
              force.print.labels = TRUE,
              aspRatio = 9/6.5,
              position.legend = "none",
              palette = palette.brewed,
              overlap.labels = 1, type = "index")
    dev.off()
    tiff("overall-treemap-articles.tif", width = 9, height = 6.5, units = "in", res = 600)
      treemap(article.list, c("tribe_name", "fullRef"), vSize = "es",
              fontsize.labels = c(18, 5),
              fontfamily.title = font.choice,
              fontfamily.labels = font.choice,
              palette = palette.brewed,
              title = "",
              overlap.labels = 1)
    dev.off()
    
    #View(data.el)

    V(data.graph)$name <- subset(data.graph, V(data.graph)$name %in% tolower(focus.list$manuscriptID))
    
    trial1 <- data.clusters[1:2]
    trial2 <- data.clusters[1:2]
    data.el <- merge(trial1, trial2, by = "tribe")
    data.el$tribe <- NULL
    data.el <- subset(data.el, manuscriptID.x %in% focus.list$manuscriptID)
    data.el <- subset(data.el, manuscriptID.y %in% focus.list$manuscriptID)
    #data.el$modularity <- data.modularity
    
    V(data.graph)$type <- as.character(data.clusters$status[match(V(data.graph)$name, data.clusters$manuscriptID)]) 
    V(data.graph)$community <- as.numeric(data.clusters$tribe[match(V(data.graph)$name, data.clusters$manuscriptID)])
 
    V(data.graph)$label <- ""
    V(data.graph)$label <- ifelse(V(data.graph)$type == "articles", V(data.graph)$name, "")
    V(data.graph)$shape <- ifelse(V(data.graph)$type == "articles", "circle", "square")
    V(data.graph)$size <- as.numeric(data.clusters$es[match(V(data.graph)$name, data.clusters$manuscriptID)])
    V(data.graph)$size <- ifelse(V(data.graph)$type == "articles", V(data.graph)$size/2, 1)
    V(data.graph)$label.size <- ifelse(V(data.graph)$type == "articles", 0.75, 0.5)
    V(data.graph)$label.dist <- ifelse(V(data.graph)$type == "articles", 0.25, 0.05)
    V(data.graph)$label.color <- ifelse(V(data.graph)$type == "articles", "black", "gray50")

    #n<-6
    #size_vec<-seq_len(n)
    #sizeCut<-cut(data.clusters$es,n)
    #vertex.size<-size_vec[sizeCut]
    #V(data.graph)$size <- vertex.size
    #V(data.graph)$size <- ifelse(V(data.graph)$type == "articles", V(data.graph)$size * 3, 1)
    
    tiff("overall-network-plot.tif", width = 18, height = 15, units = "in", res = 300)
      plot(data.graph, vertex.label = V(data.graph)$name, layout = layout_with_mds,
           vertex.color=adjustcolor(palette.brewed[V(data.graph)$community], alpha.f = 0.5),
           vertex.frame.color = palette.brewed[V(data.graph)$community],
           vertex.shape = V(data.graph)$shape,
           vertex.label.family = font.choice, vertex.label.cex = V(data.graph)$label.size,
           vertex.label.dist = V(data.graph)$label.dist,
           edge.color = adjustcolor("gray87", alpha.f = 0.5),
           vertex.label.color = V(data.graph)$label.color)
        text(-0.25, 0, "Designing Opportunities to Engage\nPreservice Teachers with Technology",
             col = palette.brewed[1], cex = 0.7,
             family = font.choice)
        text(-0.08, -0.22, "Impacting Self-Efficacy for\nTeaching with Technology",
             col = palette.brewed[2], cex = 0.7,
             family = font.choice)
        text(1, 0.2, "Understanding the Beliefs and Attitudes of\nPerservice Teachers About Technology",
             col = palette.brewed[3], cex = 0.7,
             family = font.choice)
        text(0.95, -0.22, "Aligning Preparation for Teaching with\nTechnology with Standards",
             col = palette.brewed[4], cex = 0.7,
             family = font.choice)
        text(0.7, 0.27, "Improving Teaching with\nTechnology Effectiveness",
             col = palette.brewed[5], cex = 0.7,
             family = font.choice)
        text(-0.1, 0.24, "Leveraging Technology for\nAcademic Learning",
             col = palette.brewed[6], cex = 0.7,
             family = font.choice)
        text(-0.2, -0.5, "Educational Innovation and Change in\nPreservice Teacher Education with Technology",
             col = palette.brewed[7], cex = 0.7,
             family = font.choice)
        #scaled <- 1 + ((2-1) * (size_vec - min(size_vec) ) / (  max(size_vec) - min(size_vec) ) * 3)
        #legend('topleft', legend=levels(sizeCut), pt.cex=scaled, col='black', pch=42, pt.bg='skyblue')
    dev.off()
    #output.plot(data.plot, 300, "results/overall-network-plot.tif")
    
    
    overall.positive <- overall.long[grep("no ",overall.long$value, invert = TRUE),]
    overall.positive$variable <- NULL
    overall.positive$manuscriptID <- toupper(overall.positive$manuscriptID)
    
    
    # ACTOR MEASURES
    
    data.degree <- centralization.degree(data.graph, mode = "all", loops = FALSE)
    data.net.degree <- data.degree$centralization
    data.betweenness <- centralization.betweenness(data.graph, directed = FALSE)
    data.net.betweenness <- data.betweenness$centralization
    data.closeness <- centralization.closeness(data.graph, mode = "all")
    data.net.closeness <- data.closeness$centralization
    data.eigen <- centralization.evcent(data.graph, directed = FALSE)
    data.prestige <- prestige(data.network)
    data.coreness <- coreness(data.graph, mode = "all")
    data.power <- power_centrality(data.graph, loops = FALSE)
    library(info.centrality)
    data.info <- info.centrality.vertex(data.graph)
    
    data.degree.frame <- data.frame(manuscriptID = V(data.graph)$name, degree = data.degree$res) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      arrange(desc(degree))
    
    data.betweenness.frame <- data.frame(manuscriptID = V(data.graph)$name, betweenness = data.betweenness$res) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(betweenness >= quantile(betweenness, 0.75)) %>%
      arrange(desc(betweenness))
    
    data.closeness.frame <- data.frame(manuscriptID = V(data.graph)$name, closeness = data.closeness$res) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(closeness >= quantile(closeness, 0.75)) %>%
      arrange(desc(closeness))
    
    data.eigen.frame <- data.frame(manuscriptID = V(data.graph)$name, eigen = data.eigen$vector) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(eigen >= quantile(eigen, 0.75)) %>%
      arrange(desc(eigen))
    
    data.prestige.frame <- data.frame(manuscriptID = V(data.graph)$name, prestige = data.prestige) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(prestige >= quantile(prestige, 0.75)) %>%
      arrange(desc(prestige))
    
    data.coreness.frame <- as.data.frame(unlist(data.coreness))
    data.coreness.frame$manuscriptID <- rownames(data.coreness.frame)
    rownames(data.coreness.frame) <- NULL
    colnames(data.coreness.frame) <- c("coreness", "manuscriptID")
    data.coreness.frame <- subset(data.coreness.frame, !(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(coreness >= quantile(coreness, 0.75)) %>%
      arrange(desc(coreness))
    
    data.power.frame <- as.data.frame(unlist(data.power))
    data.power.frame$manuscriptID <- rownames(data.power.frame)
    rownames(data.power.frame) <- NULL
    colnames(data.power.frame) <- c("power", "manuscriptID")
    data.power.frame <- subset(data.power.frame, !(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(power >= quantile(power, 0.75)) %>%
      arrange(desc(power))
    
    data.info.frame <- data.frame(manuscriptID = V(data.graph)$name, info = data.info) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(info >= quantile(info, 0.75)) %>%
      arrange(desc(info))
    
    data.es.frame <- data.frame(manuscriptID = data.clusters$manuscriptID, es = data.clusters$es) %>%
      subset(!(manuscriptID %in% focus.list$manuscriptID)) %>%
      #subset(es >= quantile(es, 0.75)) %>%
      arrange(desc(es))
    
    list.df <- list(data.degree.frame, data.betweenness.frame, data.closeness.frame, data.eigen.frame,
                    data.prestige.frame, data.coreness.frame, data.power.frame, data.info.frame,
                    data.es.frame)

    data.measures.frame <- Reduce(function(x, y) merge(x, y, all=TRUE), list.df)
    
    data.measures.scaled <- data.measures.frame
    data.measures.scaled[2:10] <- round(scale(data.measures.scaled[2:10], center = FALSE), 3)
    data.measures.scaled$measure.mean <- (rowMeans(data.measures.scaled[,2:10], na.rm = TRUE))
    data.measures.scaled$measure.mean <- round(data.measures.scaled$measure.mean, 3)
    data.measures.scaled <- arrange(data.measures.scaled, desc(measure.mean))
    data.measures.scaled.75 <- subset(data.measures.scaled, (measure.mean >= quantile(measure.mean, 0.75)))
    data.measures.frame.75 <- subset(data.measures.frame, manuscriptID %in% data.measures.scaled.75$manuscriptID)
    #data.measures.scaled$quart <- quantile(data.measures.scaled$measure.mean)
    #test <- quantile(data.measures.scaled$measure.mean, c(0, 0.25, 0.50, 0.75, 1))
    # GRAPH MEASURES
    
    data.density <- graph.density(data.graph, loops = FALSE)
    data.reciprocity <- reciprocity(data.graph, ignore.loops = TRUE)
    data.transitivity <- transitivity(data.graph, type = "undirected")
    data.connectedness <- connectedness(data.network)
    data.efficiency <- efficiency(data.network)
    data.hierarchy <- hierarchy(data.network, measure = "krackhardt")
    data.lubness <- lubness(data.network)

    data.sub <- data.clusters[1:2]
    
    overall.positive <- merge(overall.positive, data.sub)
    
    cluster.split <- split(overall.positive, overall.positive$tribe)
    cluster.count <- data.frame(manuscriptID = as.character(),
                                n = as.integer(), tribe = as.integer())
    for(i in 1:max(data.clusters$tribe)) {
      file.name <- paste("split", i, ".csv", sep = "")
      data.set.count <- cluster.split[[i]] %>% 
        group_by(value) %>% 
        count(sort = TRUE)
#      write_csv(data.set.count, file.name)
      data.set.count$tribe <- i
      data.set.count$n2 <- round((data.set.count$n) / sum(data.set.count$n), digits = 3)
      data.set.count$n3 <- as.numeric(scale(data.set.count$n2, center = FALSE))
      # TRY JUST TAKING THE 3RD QUARTILE
      cluster.count <- rbind(data.frame(cluster.count), data.frame(data.set.count))
    }
    cluster.count <- merge(cluster.count, cluster.name.frame)
    
    
#    library(vcd)
#    library(vcdExtra)
#    cluster.count.table <- aperm(as.matrix(cluster.count), c(2, 1))
#    cluster.count.ftable <- ftable(cluster.count.table)
#    cluster.count.structable <- structable(tribe + n ~ value, cluster.count)
#    oddsratio(cluster.count.structable, log = FALSE)
    #vcd::mosaic(cluster.count.structable)
#    cluster.count$n2 <- as.numeric(scale(cluster.count$n2, center = FALSE))
    
    cluster.count <- arrange(cluster.count, tribe, desc(n3), value)
    write_csv(cluster.count, "cluster_count.csv")
    
    treemap(cluster.count, c("tribe", "value"), vSize = "n3")
    
    #return(data.el)
  
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

  do.MCA <- function(data.set, data.title){
#    data.set <- overall.long
#    data.title <- "overall"
    data.set.matrix <- prepare.matrix(data.set)
    data.set.OK.list <- as.list(colnames(data.set.matrix))
    data.set.MCA <- MCA(data.set.matrix, na.method = "NA", graph = FALSE)
    bi.plot(data.set.MCA, data.set.OK.list, data.title)
    scree.plot(data.set.MCA, data.title)
    contribution.plot(data.set.MCA, data.title)
    do.dbscan(data.set.MCA, data.title)
    #return(data.set.MCA)
  }
  
  do.special.count <- function(data.set) {
    for(i in 1:ncol(data.set)){
      data.set[,i][grep("no ", data.set[,i])] <- NA
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
  
  do.command <- function(command.text) {
    #print(command.text)
    return(eval(parse(text = command.text)))
  }
  
  
  #######################
  #                     #
  #  PREPARE DATA SETS  #
  #                     #
  #######################
  
  # Load in raw data file from GitHub
  descriptors.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-keywords-processed.csv", col_names = TRUE)
  methods.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-data_sources-survey_combined.csv", col_names = TRUE)
  purposes.supported.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-purpose_supported.csv", col_names = TRUE)
  purposes.stated.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-purpose_stated.csv", col_names = TRUE)
  genres.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-research_genre.csv", col_names = TRUE)
  traditions.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-teacher_education_traditions.csv", col_names = TRUE)
  cluster.name.frame <- read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/lit_review-cluster_names.csv", col_names = TRUE)
  
  
  #year.frame <- descriptors.frame[1:2]
  #descriptors.frame[2] <- NULL
  
  focus.list <<- as.data.frame(read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/article-master-crosswalk.csv", col_names = FALSE))
  colnames(focus.list) <- c("manuscriptID", "fullRef", "artTitle")
  
  filter.map <<- as.data.frame(read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/descriptors-filter_map.csv", col_names = TRUE))
  colnames(filter.map) <- c("variable", "substitute")
  stop.words <- as.data.frame(read_csv("~/Box Sync/Research (jfprice@iupui.edu)/litreview/data/descriptors-stop_words.csv", col_names = FALSE))
  colnames(stop.words) <- "words"
  
  # Temporary exclusion of studies
  #temp.exclude <- c("TAI2015", "PARKYANG2013", "KOPCHADI2016")
  #descriptors.frame <- descriptors.frame[!descriptors.frame$manuscriptID %in% temp.exclude, ]
  #methods.frame <- methods.frame[!methods.frame$manuscriptID %in% temp.exclude, ]
  #focus.list <- focus.list[!focus.list$manuscriptID %in% temp.exclude, ]
  
  # Article citations
  article.el = as.data.frame(read_csv("~/Box Sync/lit_review/article_citation-el-reduced.csv", col_names = FALSE))
  colnames(article.el) <- c("from", "to")
  article.el <- article.el[!article.el$from %in% temp.exclude, ]
  article.el <- as.matrix(article.el)
  
  # Set global minimum cut value
  cut.min <<- 0
  color.palette <<- c("#990000", "#DC8823", "#285C4D", "#01426A", "#512A44", "#83786F", "#191919")
  color.palette.light <<- c("#990000", "#F1BE48", "#008264", "#006298", "#66435A", "#ACA39A", "#4A3C31")
  new.palette <<- c("#7790b9", "#acdc3d", "#8e4dd9", "#74be52", "#cc53b7", "#7db671",
                    "#4d2b78", "#cd9c36", "#7881d5", "#c55334", "#64bbaa", "#be4165",
                    "#a9b0b1", "#4c3b3d", "#bf7b9c", "#977f54")
  tol15rainbow <<- c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB",
                     "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777",
                     "#771144", "#AA4477", "#DD77AA") #https://www.r-bloggers.com/the-paul-tol-21-color-salute/
  #color.palette <<- c("#01426A", "#DC8823", "#285C4D", "#770000")
  shadesOfGrey <- colorRampPalette(c("grey0", "grey90"))
  gray.palette <- shadesOfGrey(15)
  bg.color <<-"#EEEEEE"
  line.color <<- "#83786F"
  font.choice <<- "Noto Mono"
  seed.value <<- 4.6692
  cut.quant <<- 4
  
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
  
  descriptors.long <- prepare.long(descriptors.frame)
  # Remove stopwords, which will not be used in the analysis
  descriptors.long <- subset(descriptors.long, !(variable %in% stop.words$words))
  
  descriptors.long <- merge(descriptors.long, filter.map, by = "variable", all.x = TRUE)
  descriptors.long$substitute <- ifelse(is.na(descriptors.long$substitute), descriptors.long$variable, descriptors.long$substitute)
  descriptors.long[,3][grep("no ", descriptors.long[,3])] <- NA
  descriptors.long$value <- ifelse(!is.na(descriptors.long$value), descriptors.long$substitute, NA)
  descriptors.long$variable <- descriptors.long$substitute
  descriptors.long$substitute <- NULL
  
  descriptors.long <- descriptors.long[, c("manuscriptID", "variable", "value")] %>%
    arrange(variable, manuscriptID, value)
  
  descriptors.long <- descriptors.long[!duplicated(descriptors.long[1:2]), ]
  
  
  
  #descriptors.long[,3][grep("no ", descriptors.long[,3])] <- NA
  descriptors.long$value <- ifelse(is.na(descriptors.long$value), paste("no", descriptors.long$variable), descriptors.long$variable)
  
  descriptors.count <- remove.no(descriptors.long, 3) %>%
    subset(select = value) %>%
    count.long()
  
  retain.rows <- subset(descriptors.count, (value %in% c("multicultural education", "urban schools")))
  
  descriptors.keep <- subset(descriptors.count, (n > 1))
  descriptors.keep <- rbind(descriptors.keep, retain.rows)
  
  descriptors.long <- subset(descriptors.long, variable %in% descriptors.keep$value)
  
  #descriptors.frame <- reshape(descriptors.long, idvar = "manuscriptID", timevar = "variable", direction = "wide")
  
  methods.long <- prepare.long(methods.frame)
  genres.long <- prepare.long(genres.frame)
  purposes.supported.long <- prepare.long(purposes.supported.frame)
  purposes.stated.long <- prepare.long(purposes.stated.frame)
  traditions.long <- prepare.long(traditions.frame)
  
  # Combine frames and merge to create an "overall" frame
  overall.long <- rbind(descriptors.long, methods.long, genres.long,# purposes.supported.long,
                        purposes.stated.long, traditions.long)
  
  # OVERALL
  
  overall.count <- remove.no(overall.long, 3) %>%
    count.long()
  overall.count.quant <- quantile(overall.count$n)
  overall.reduced.filter <- subset(overall.count, (n >= overall.count.quant[cut.quant]))
  overall.reduced.long <- subset(overall.long, (value %in% overall.reduced.filter$value))
  count.graph <- ggplot(overall.count, aes(x = reorder(value, -n), y = n)) +
    geom_bar(stat = "identity", color = "gray40") + theme_minimal() +
    geom_hline(yintercept = overall.count.quant[cut.quant], color = "black", linetype = "dashed") +
    #geom_hline(yintercept = overall.count.quant[cut.quant + 1], color = "black", linetype = "dashed") +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    theme(text = element_text(size = 6, family = "Fira Code")) + ylab("Count") + xlab("Descriptors")# +
    #coord_flip()
  output.plot(count.graph, 300, "results/descriptor-cutoff.tif")
  
  #overall.reduced.se.filter <- rbind(overall.reduced.filter, "self efficacy")
  #overall.reduced.se.long <- subset(overall.long, (value %in% overall.reduced.se.filter$value))
  
  ####################################
  # Prepare data frames for analysis #
  ####################################
  
  overall.reduced.list <- make.id.list(overall.reduced.long)
  
  overall.leftout.long <- make.id.list(subset(overall.long,
                                              !(manuscriptID %in% overall.reduced.list$manuscriptID)))
  
  overall.reduced.list <- rbind(overall.reduced.list, overall.leftout.long)
  
  overall.frame <- reshape(overall.long, idvar = "manuscriptID", timevar = "variable", direction = "wide")
  
  colnames(overall.frame) <- gsub("value\\.", "", colnames(overall.frame))
  
  split.track.frame <- data.frame(frames = character(), descriptor.name = character(),
                                  stringsAsFactors = FALSE)
  
  split.count <- length(overall.reduced.filter$value)
  
  for(i in 1:split.count) {
    frame.name <- overall.reduced.filter[i, 1]
    frame.name <- gsub(" ", "_", frame.name)
    paste.name <- paste("split.frame.", i, sep = "")
    split.command <- paste("split.frame",
                     " <- split(overall.frame, overall.frame$`", overall.reduced.filter[i, 1],
                     "`)", sep = "")
    positive.sep.command <- paste(paste.name, " <- split.frame$`", overall.reduced.filter[i, 1],
                                "`", sep = "")
    negative.sep.command <- paste(paste.name, "_no <- split.frame$`no ", overall.reduced.filter[i, 1],
                                  "`", sep = "")
    new.row <- data.frame(frames = paste.name, descriptor.name = frame.name)
    track.command <- "split.track.frame <- rbind(split.track.frame, new.row)"
    positive.long.command <- paste(paste.name, ".long <- prepare.long(", paste.name, ")", sep = "")
    negative.long.command <- paste(paste.name, "_no.long <- prepare.long(", paste.name, "_no)", sep = "")
    eval(parse(text = split.command))
    eval(parse(text = positive.sep.command))
    eval(parse(text = negative.sep.command))
    eval(parse(text = track.command))
    eval(parse(text = positive.long.command))
    eval(parse(text = negative.long.command))
  }
  
  ######################
  #                    #
  #  CONDUCT ANALYSES  #
  #                    #
  ######################
  
  # change "do.MCA" to "do.analysis"
  
  do.MCA(overall.long, "all")
  identify.clusters(overall.long, "all")
  #da.list$list <- c("human capital", "effects", "survey", "tpack")
  #reduce.list <- data.frame(words = c("human capital", "effects", "survey", "tpack"))
  #overall.reduced.long2 <- subset(overall.reduced.long, !variable %in% reduce.list$words)
  
#do.MCA(overall.reduced.long2, "reduced")
#do.MCA(overall.reduced.se.long, "reduced with self-efficacy")
#do.MCA(tpack.long, "TPACK")
#do.MCA(no_tpack.long, "non-TPACK")
#do.MCA(attitude.long, "attitude")
#do.MCA(no_attitude.long, "non-attitude")
#do.MCA(survey.long, "survey")
#do.MCA(no_survey.long, "non-survey")
#do.MCA(stem.long, "stem")
#do.MCA(no_stem.long, "non-stem")

#do.MCA(descriptors.long, "descriptors")
#test.mca.frame <- as.data.frame(test.mca$ind$coord[,1:2])
#do.MCA(methods.long, "methods")

#overall.positive <- overall.long[overall.long$value[grep("no ", overall.long$value)]]

#overall.positive <- overall.long[grep("no ",overall.long$value, invert = TRUE),]

#overall.cors <- calculate.correlation(overall.positive)

####################################
#                                  #
#  PREPARE WORD COMPARISON GRAPHs  #
#                                  #
####################################


#do.compare.graph(tpack.frame, no_tpack.frame, "TPACK", "non-TPACK")
#do.compare.graph(attitudes.frame, no_attitudes.frame, "attitude", "non-attitude")
#do.compare.graph(survey.frame, no_survey.frame, "survey", "non-survey")
#do.compare.graph(artifact.frame, no_artifact.frame, "artifact", "non-artifact")
#do.compare.graph(stem.frame, no_stem.frame, "STEM", "non-STEM")

