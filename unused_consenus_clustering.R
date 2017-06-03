
community.temp <- data.frame(community.set)
community.temp$mod <- NULL

community.temp <- data.frame(lapply(community.temp, as.character), stringsAsFactors=FALSE)

csle <- rle(community.temp$descriptor)
csids <- rep(seq_len(length(csle$values)), times=csle$lengths)
idcs <- as.data.frame(cbind(id=csids, community.temp[1:3]))

cs2 <- dcast(idcs, group ~ descriptor, value.var = "community")
cs2[is.na(cs2)] <- 0
cs2$group <- NULL
cs2 <- as.matrix(sapply(cs2, as.numeric))

max.community <- max(as.integer(community.set$community))

community.consensus <- ConsensusPipeline(cs2, max.community, safetyOverride = FALSE, ComreyFactorCheck = FALSE)
sink(file = "results/descriptors-community_consensus.txt", append = FALSE)
print(community.consensus)
sink()

community.consensus.list <- as.data.frame(community.consensus$Answers)
community.consensus.list$descriptor <- rownames(community.consensus.list)
rownames(community.consensus.list) <- NULL
colnames(community.consensus.list)[1] <- "community"
community.consensus.list <- community.consensus.list[order(community.consensus.list[1], community.consensus.list[2]),] 

sink(file = "results/descriptors-community_consensus-list.txt", append = FALSE)
print(community.consensus.list)
sink()



manuscript.consensus.frame <- overall.reduced.long
manuscript.consensus.frame$variable <- NULL
colnames(manuscript.consensus.frame)[2] <- "descriptor"
manuscript.consensus.frame <- merge(manuscript.consensus.frame, community.consensus.list, by.x = "descriptor")
manuscript.consensus.frame <- data.frame(manuscript.consensus.frame)

cs2 <- reshape(manuscript.consensus.frame, timevar = "manuscriptID", v.names = "community", idvar = "descriptor",
               direction = "wide")
cs2[is.na(cs2)] <- 0
cs2$descriptor <- NULL
cs2 <- as.matrix(sapply(cs2, as.numeric))
colnames(cs2) <- gsub("community\\.", "", colnames(cs2))

manuscript.community.consensus <- ConsensusPipeline(cs2, 3, safetyOverride = FALSE, ComreyFactorCheck = FALSE)
sink(file = "results/manuscript-community_consensus.txt", append = FALSE)
print(manuscript.community.consensus)
sink()

manuscript.consensus.list <- as.data.frame(manuscript.community.consensus$Answers)
manuscript.consensus.list$manuscriptID <- rownames(manuscript.consensus.list)
rownames(manuscript.consensus.list) <- NULL
colnames(manuscript.consensus.list)[1] <- "community"
manuscript.consensus.list <- data.frame(manuscript.consensus.list)
manuscript.consensus.list <- manuscript.consensus.list[order(manuscript.consensus.list[2], manuscript.consensus.list[1]),] 

sink(file = "results/manuscript-community_consensus-list.txt", append = FALSE)
print(manuscript.consensus.list)
sink()

temp.manuscript <- overall.reduced.long
temp.manuscript$variable <- NULL
colnames(temp.manuscript)[2] <- "descriptor"

overall.consensus <- merge(community.consensus.list, temp.manuscript, by.x = "descriptor")
colnames(overall.consensus)[2] <- "descriptor.community"

overall.consensus <- merge(overall.consensus, manuscript.consensus.list, by.x = "manuscriptID")
colnames(overall.consensus)[4] <- "manuscript.community"
overall.consensus <- overall.consensus[!duplicated(overall.consensus), ]

#community.consensus.list.reduced <- subset(community.consensus.list, (descriptor %in% overall.reduced.filter$value))
#colnames(community.consensus.list.reduced)[1] <- "community"


descriptor.prob <- community.consensus$Probs
descriptor.prob.max <- as.data.frame(apply(descriptor.prob,2,max))
descriptor.prob.max$descriptor <- rownames(descriptor.prob.max)
rownames(descriptor.prob.max) <- NULL
colnames(descriptor.prob.max) <- c("probability", "descriptor")
descriptor.consensus.max <- merge(community.consensus.list, descriptor.prob.max, by.x = "descriptor")
sink(file = "results/descriptors-community_consensus-probs.txt", append = FALSE)
print(descriptor.consensus.max)
sink()

#write.csv(descriptor.prob, file = "results/descriptor-consensus_probs.csv")
manuscript.prob <- manuscript.community.consensus$Probs
manuscript.prob.max <- as.data.frame(apply(manuscript.prob,2,max))
manuscript.prob.max$manuscriptID <- rownames(manuscript.prob.max)
rownames(manuscript.prob.max) <- NULL
colnames(manuscript.prob.max) <- c("probability", "manuscriptID")
manuscript.consensus.max <- merge(manuscript.consensus.list, manuscript.prob.max, by.x = "manuscriptID")
sink(file = "results/manuscripts-community_consensus-probs.txt", append = FALSE)
print(manuscript.consensus.max)
sink()
#write.csv(descriptor.prob, file = "results/manuscript-consensus_probs.csv")

