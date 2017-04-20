# Load libraries to analyze data
library(FactoMineR)
library(factoextra)
#library(stringr)
library(CAinterprTools)
#library(readr)

# Define function for writing plots to a file
output.plot <- function(plot.output, plot.resolution, file.name){
  tiff(file.name, width = 11, height = 8.5, units = "in", res = plot.resolution)
  print(plot.output)
  dev.off()
}

keywords.frame <- read.csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/lit_review-keywords-processed.csv", header = TRUE)
methods.frame <- read.csv("https://raw.githubusercontent.com/jeremyfprice/litreview/master/data/lit_review-data_sources-survey_combined.csv", header = TRUE)
analysis.frame <- merge(keywords.frame, methods.frame, by.x = "manuscriptID")
#keywords.frame[,1] <- NULL
#x <- read_csv(file_loc, col_names = FALSE)

# Set row names to the Manuscript IDs
row.names(analysis.frame) <- analysis.frame$manuscriptID
analysis.frame$manuscriptID <- NULL

# Calculate Multiple Correspondence Analysis
analysis.MCA <- MCA(analysis.frame, na.method = "NA", graph = FALSE)

OK.list <- colnames(analysis.frame)

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
output.plot(analysis.biplot, 900, "results/combined-biplot.tif")
