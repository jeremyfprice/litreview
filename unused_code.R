
do.cluster(overall.long, "all")
#do.cluster(overall.reduced.long, "reduced")
do.cluster(descriptors.long, "descriptors")
do.cluster(methods.long, "methods")

split.track.matrix <- as.matrix(split.track.frame)

for(i in 1:split.count) {
  original.frame <- paste("split.frame.", i, sep = "")
  positive.frame <- paste(original.frame, ".long", sep = "")
  positive.name <- split.track.matrix[i,2]
  negative.frame <- paste(original.frame, "_no.long", sep = "")
  negative.name <- paste("no ", positive.name, sep = "")
  positive.cluster.command <- paste("do.cluster(", positive.frame, ", \"",
                                    positive.name, "\")", sep = "")
  negative.cluster.command <- paste("do.cluster(", negative.frame, ", \"",
                                    negative.name, "\")", sep = "")
  #do.command(positive.cluster.command)
  #do.command(negative.cluster.command)
}

article.graph <- simplify(graph_from_edgelist(article.el, directed = FALSE),
                          remove.loops = TRUE, remove.multiple = FALSE)
set.seed(seed.value)
article.community <- cluster_louvain(article.graph)
article.modularity <- modularity(article.community)
article.clusters <- data.frame(manuscriptID = toupper(article.community$names),
                               tribe = article.community$membership,
                               mod = article.modularity)
rownames(article.clusters) <- NULL

trial1 <- article.clusters[1:2]
trial2 <- article.clusters[1:2]
cites.el <- merge(trial1, trial2, by = "tribe")
cites.el$tribe <- NULL
cites.el$modularity <- article.modularity

#new.row <- data.frame(trial = "citations", noClusters = length(article.clusters), 
#                      modularity = article.modularity)
#trial.table <<- rbind(trial.table, new.row)

#community.el <<- rbind(community.el, cites.el)

useable.el <- data.frame(x = community.el$manuscriptID.x, y = community.el$manuscriptID.y)
useable.el <- as.matrix(useable.el)
useable.weights <- data.frame(modularity = community.el$modularity)
useable.weights$modularity <- abs(scale(useable.weights$modularity, center = TRUE))


community.graph <- simplify(graph_from_edgelist(useable.el, directed = FALSE),
                            remove.loops = TRUE, remove.multiple = FALSE)
community.network <- network(community.el, matrix.type="edgelist", directed=FALSE)
set.seed(seed.value)
community.clusters <- cluster_louvain(community.graph, weights = useable.weights$modularity)

community.overall <- data.frame(manuscriptID = community.clusters$names, community = community.clusters$membership)

community.authority <- data.frame(authority.score(community.graph)$vector)
community.authority$manuscriptID <- rownames(community.authority)
rownames(community.authority) <- NULL
colnames(community.authority)[1] <- "authority"

community.between <- data.frame(igraph::betweenness(community.graph))
community.between$manuscriptID <- rownames(community.between)
rownames(community.between) <- NULL
colnames(community.between)[1] <- "betweenness"
#community.between$betweenness <- (scale(community.between$betweenness, center = FALSE)) + 1

community.prestige <<- (prestige(community.network))
community.prestige <<- cbind(community.between$manuscriptID, community.prestige)
colnames(community.prestige) <- c("manuscriptID", "prestige")

community.overall <- merge(community.overall, community.authority)
community.overall <- merge(community.overall, community.between)
community.overall <- merge(community.overall, community.prestige)
community.overall$authority <- as.numeric(community.overall$authority)
community.overall$betweenness <- as.numeric(community.overall$betweenness)
community.overall$prestige <- as.numeric(community.overall$prestige)

community.overall[,3:5] <- scale(community.overall[,3:5], center = FALSE) + 1
community.overall$betweenness <- 1/community.overall$betweenness
article.el.frame <- data.frame(article.el)

#if(community.overall$manuscriptID %in% article.el.frame$X2){community.overall$focus <- "citations"}
community.overall$focus <- ifelse(community.overall$manuscriptID %in% focus.list$manuscriptID, "articles",
                                  ifelse(community.overall$manuscriptID %in% article.el.frame$to, "citations", "keywords"))

community.overall <- arrange(community.overall, community, focus, manuscriptID)

community.overall$manuscriptID <- ifelse(community.overall$manuscriptID %in% focus.list$manuscriptID,
                                         toupper(community.overall$manuscriptID),
                                         tolower(community.overall$manuscriptID))

community.focus <- subset(community.overall, (manuscriptID %in% focus.list$manuscriptID))

community.overall.nocites <- subset(community.overall, !(focus == "citations"))
community.overall.nocites <- subset(community.overall.nocites, (community %in% community.focus$community))

community.modularity <- modularity(community.clusters)

tiff("treemap1.tif", width = 11, height = 8.5, units = "in", res = 600)
treemap(community.overall, index = c("community", "focus", "manuscriptID"), vSize = "authority", vColor = "focus",
        palette = tol15rainbow, title = "Articles and Descriptors in Clusters", force.print.labels = TRUE,
        aspRatio = 11/8.5)
dev.off()

tiff("treemap1b.tif", width = 11, height = 8.5, units = "in", res = 600)
treemap(community.overall.nocites, index = c("community", "focus", "manuscriptID"), vSize = "betweenness",
        vColor = "focus", palette = tol15rainbow, title = "Articles and Descriptors in Clusters",
        force.print.labels = TRUE, aspRatio = 8.5/11)
dev.off()

tiff("treemap2.tif", width = 11, height = 8.5, units = "in", res = 600)
treemap(community.focus, index = c("community", "manuscriptID"), vSize = "prestige",
        palette = shadesOfGrey(4), title = "Articles in Clusters", force.print.labels = TRUE,
        aspRatio = 11/8.5)
dev.off()
tiff("treegraph1.tif", width = 11, height = 8.5, units = "in", res = 600)
treegraph(community.overall, index = c("community", "focus", "manuscriptID"), directed = FALSE,
          show.labels = FALSE, rootlabel = "")
dev.off()
tiff("treegraph2.tif", width = 11, height = 8.5, units = "in", res = 600)
treegraph(community.focus, index = c("community", "manuscriptID"), directed = FALSE,
          show.labels = TRUE, rootlabel = "")#, vertex.size = 2.5*as.vector(community.focus$betweenness))
dev.off()

tiff("treegraph-semireduced.tif", width = 22, height = 22, units = "in", res = 600)
treegraph(community.overall.nocites, index = c("community", "focus", "manuscriptID"), directed = FALSE,
          show.labels = TRUE, rootlabel = "")#, vertex.size = 2.5*as.vector(community.focus$betweenness))
dev.off()

write_csv(community.overall.nocites, "results/community_overall_nocites.csv")
write_csv(community.overall, "results/community_overall.csv")
write_csv(community.focus, "results/community_focus.csv")

library(treemapify)
ggplot(community.overall, aes(area = prestige, fill = authority, label = manuscriptID, 
                              subgroup = focus)) +
  geom_treemap() + geom_treemap_subgroup_border() +
  facet_wrap( ~ community) +
  geom_treemap_subgroup_text(
    place = "centre",
    grow = FALSE,
    alpha = 0.5,
    colour = "black",
    #fontface = "italic",
    family = font.choice,
    min.size = 0
  ) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    family = font.choice,
    grow = FALSE,
    reflow = TRUE
  )



community.split <- split(community.overall, community.overall$community)
communities.length <<- length(community.split)
for(i in 1:communities.length) {
  assign(paste0("community.", i), community.split[[i]])
}

community.matrix <- as.sociomatrix.sna(community.network)
plot.sociomatrix(community.matrix)

focus.el <- subset(community.el, (manuscriptID.x %in% focus.list$manuscriptID & manuscriptID.y %in% focus.list$manuscriptID))
focus.el <- subset(focus.el, !(manuscriptID.x == manuscriptID.y))
#focus.el$modularity <- NULL
focus.el <- as.matrix(focus.el)

focus.network=network(focus.el[,1:2], matrix.type="edgelist", directed=FALSE)

focus.cc<-clique.census(focus.network, clique.comembership="sum")
summary(focus.cc)

focus.brokerage <- brokerage(focus.network, 1:14)
summary(focus.brokerage)

overall.network=network(community.el[,1:2], matrix.type="edgelist", directed=FALSE)

overall.cc<-clique.census(overall.network, clique.comembership="sum")
summary(overall.cc)

overall.brokerage <- brokerage(overall.network, 1:14)
summary(overall.brokerage)



calculate.cliques <- function(data.el) {
  data.el$variable <- NULL
  data.el[,2][grep("no ", data.el[,2])] <- NA
  data.el <- as.matrix(na.omit(data.el))
  data.network <- network(data.el, matrix.type="edgelist", directed=FALSE)
  data.graph <- simplify(graph_from_edgelist(as.matrix(data.el)))
  data.cc<-clique.census(data.network, clique.comembership="sum")
  print("****************")
  print(summary(data.cc))
  print("****************")
  
  data.brokerage <- brokerage(data.network, 1:length(data.cc$cliques))
  print("****************")
  print(summary(data.brokerage))
  print("****************")
  data.authority <- authority.score(data.graph)
  print("****************")
  print(data.authority)
  print("****************")
  data.betweenness <- betweenness(data.network)
  print("****************")
  print(data.betweenness)
  print("****************")
  data.prestige <- prestige(data.network)
  print("****************")
  print(data.prestige)
  print("****************")
  data.degree <- degree(data.network)
  print("****************")
  print(data.degree)
  print("****************")
}

calculate.cliques(descriptors.long)
calculate.cliques(methods.long)
calculate.cliques(overall.long)

descriptors.network <- network(descriptors.long, matrix.type="edgelist", directed=FALSE)
descriptors.graph <- simplify(graph_from_edgelist(as.matrix(descriptors.long)))
descriptors.kcore <- kcores(descriptors.network)
descriptors.cc<-clique.census(descriptors.network, clique.comembership="sum")
summary(descriptors.cc)
descriptors.brokerage <- brokerage(descriptors.network, 1:2)
summary(descriptors.brokerage)
descriptors.authority <- authority.score(descriptors.graph)
descriptors.betweenness <- betweenness(descriptors.network)
descriptors.prestige <- prestige(descriptors.network)
descriptors.degree <- degree(descriptors.network)

methods.network = network(methods.long[,1:2], matrix.type="edgelist", directed=FALSE)
methods.cc<-clique.census(methods.network, clique.comembership="sum")
summary(methods.cc)



focus.matrix <- as.sociomatrix(focus.network, simplify = TRUE)

focus.weighted.graph=graph.adjacency(focus.matrix,mode="undirected",weighted=TRUE,diag=FALSE)


focus.graph <- graph.edgelist(focus.el[,1:2], directed = FALSE, simplify = TRUE)
E(focus.graph)$weight=scale(as.numeric(focus.el[,3]), center = FALSE)

plot.igraph(focus.graph,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(focus.graph)$weight*5)

#set.seed(seed.value)

#res.opt <- optics(test.mca.frame, 0.1, minPts = 2)
#test.opt <- extractDBSCAN(res.opt, 0.1)

#fviz_cluster(test.opt, test.mca.frame, geom = "text", ellipse.type = "convex",
#             palette = new.palette, ggtheme = theme_minimal())



#res.fpc <- fpc::dbscan(test.mca.frame, eps = 0.1, MinPts = 3)
#res.db <- dbscan::dbscan(test.mca.frame, 0.1, 3)
#res.hdb <- dbscan::hdbscan(test.mca.frame, minPts = 2)
#all(res.fpc$cluster == res.db$cluster)
#all(res.db$cluster == res.hdb$cluster)

#fviz_cluster(res.fpc, test.mca.frame, geom = "text", ellipse.type = "norm",
#             palette = new.palette, ggtheme = theme_minimal())

#cluster.frame <- data.frame(manuscriptID = rownames(test.mca.frame), cluster = res.fpc$cluster)
#cluster.hdb.frame <- data.frame(manuscriptID = rownames(test.mca.frame),
#                                cluster = res.hdb$cluster)
#write.csv(cluster.frame, "results/dbscan_clusters.csv")
#write.csv(cluster.hdb.frame, "results/hdbscan_clusters.csv")

#fpc::cluster.stats(test.mca.frame, cl$cluster)

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





do.cluster <- function(data.set, data.title) {
  data.set <- overall.long
  data.title <- "overall"
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

#for(i in 1:ncol(data.set.matrix)) {
#  data.set.matrix[,i] <- as.integer(data.set.matrix[,i])
#}

do.dbscan <- function(data.set, data.title){
  #data.set <- data.set.MCA
  #data.title <- "overall"
  cluster.file <- paste("results/dbscan-", data.title, ".csv", sep = "")
  cluster.stats.file <- paste("results/dbscan_stats-", data.title, ".txt", sep = "")
  cluster.plot.file <- paste("results/dbscan_plot-", data.title, ".tif", sep = "")
  data.coordinates <- as.data.frame(data.set$ind$coord[,1:2])
  
  set.seed(seed.value)
  
  data.dbscan <- fpc::dbscan(data.coordinates, eps = 0.1, MinPts = 3)
  
  ### FIX THIS
  #library(dbscan)
  #dbscan::kNNdistplot(data.coordinates, k =  3)
  #abline(h = 0.15, lty = 2)
  ### FIX THIS ABOVE
  data.dist <- as.matrix(dist(data.coordinates))
  
  data.cluster.plot <- fviz_cluster(data.dbscan, data.coordinates, geom = "point", ellipse.type = "convex",
                                    palette = new.palette, ggtheme = theme_minimal())
  
  output.plot(data.cluster.plot, 300, cluster.plot.file)
  data.graph <- simplify(graph_from_adjacency_matrix(data.dist, mode = "undirected"))
  data.ev <- data.frame(igraph::evcent(data.graph)$vector)
  data.ev$manuscriptID <- toupper(rownames(data.ev))
  rownames(data.ev) <- NULL
  colnames(data.ev)[1] <- "eigenvector"
  cluster.frame <- data.frame(manuscriptID = toupper(rownames(data.set$ind$coord)), cluster = data.dbscan$cluster)
  #cluster.frame$manuscriptID <- toupper(cluster.frame$manuscriptID)
  ##### !!!!!!
  # Decide on prestige, authority, or betweenness for vSize, and make sure that it gets calculated
  # based on the data passed, not "stock" data
  
  cluster.frame <- merge(cluster.frame, data.ev) %>%
    arrange(cluster, manuscriptID, eigenvector)
  #cluster.frame$between <- as.numeric(cluster.frame$between)
  #  cluster.hdb.frame <- data.frame(manuscriptID = rownames(test.mca.frame),
  #                                  cluster = res.hdb$cluster)
  #write.csv(cluster.frame, cluster.file)
  View(cluster.frame)
  data.stats <- cluster.stats(data.coordinates, data.dbscan$cluster)
  #sink(file = cluster.stats.file, append = FALSE)
  #  print(data.stats)
  #sink()
  # look into a statistic that might be useful for vSize below
  treemap(cluster.frame, c("cluster", "manuscriptID"), vSize = "eigenvector",
          force.print.labels = TRUE)
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

plot.graph <- function(data.graph, data.communities, data.title){
  cluster.file <- tolower(paste("results/cluster/cluster-", data.title,".tif", sep = ""))
  dendro.file <- tolower(paste("results/cluster/dendro-", data.title, ".tif", sep = ""))
  V(data.graph)$color <- data.communities$membership + 1
  graph.plot <- plot(data.graph, edge.width = E(data.graph)$weight * 4, layout = layout.auto)
  output.plot(graph.plot, 300, cluster.file)
  dendro.plot <- plot_dendrogram(data.communities, mode = "auto")
  output.plot(dendro.plot, 300, dendro.file)
}


