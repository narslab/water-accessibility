library(readxl)
library(NbClust)
library(dendextend)
library(factoextra) # fviz_nbclust
library(writexl)

##### ISSUE with fviz_nblucst function 
# fix from: https://stackoverflow.com/a/73573260 
# fix the functions
fviz_nbclust <- function (x, FUNcluster = NULL, method = c("silhouette", "wss", 
                                                           "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), 
                          barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", 
                          print.summary = TRUE, ...) 
{
  set.seed(123)
  if (k.max < 2) 
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("data.frame", "matrix")) & !("Best.nc" %in% 
                                                  names(x))) 
    stop("x should be an object of class matrix/data.frame or ", 
         "an object created by the function NbClust() [NbClust package].")
  if (inherits(x, "list") & "Best.nc" %in% names(x)) {
    best_nc <- x$Best.nc
    if (any(class(best_nc) == "numeric") ) 
      print(best_nc)
    else if (any(class(best_nc) == "matrix") )
      .viz_NbClust(x, print.summary, barfill, barcolor)
  }
  else if (is.null(FUNcluster)) 
    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  else if (!is.function(FUNcluster)) {
    stop("The argument FUNcluster should be a function. ", 
         "Check if you're not overriding the specified function name somewhere.")
  }
  else if (method %in% c("silhouette", "wss")) {
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    if (is.null(diss)) 
      diss <- stats::dist(x)
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_ave_sil_width(diss, clust$cluster)
      }
    }
    else if (method == "wss") {
      for (i in 1:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_withinSS(diss, clust$cluster)
      }
    }
    df <- data.frame(clusters = as.factor(1:k.max), y = v, 
                     stringsAsFactors = TRUE)
    ylab <- "Total Within Sum of Square"
    if (method == "silhouette") 
      ylab <- "Average silhouette width"
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1, 
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k", 
                        main = "Optimal number of clusters")
    if (method == "silhouette") 
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2, 
                          color = linecolor)
    return(p)
  }
  else if (method == "gap_stat") {
    extra_args <- list(...)
    gap_stat <- cluster::clusGap(x, FUNcluster, K.max = k.max, 
                                 B = nboot, verbose = verbose, ...)
    if (!is.null(extra_args$maxSE)) 
      maxSE <- extra_args$maxSE
    else maxSE <- list(method = "firstSEmax", SE.factor = 1)
    p <- fviz_gap_stat(gap_stat, linecolor = linecolor, 
                       maxSE = maxSE)
    return(p)
  }
}

.viz_NbClust <- function (x, print.summary = TRUE, barfill = "steelblue", 
                          barcolor = "steelblue") 
{
  best_nc <- x$Best.nc
  if (any(class(best_nc) == "numeric") )
    print(best_nc)
  else if (any(class(best_nc) == "matrix") ) {
    best_nc <- as.data.frame(t(best_nc), stringsAsFactors = TRUE)
    best_nc$Number_clusters <- as.factor(best_nc$Number_clusters)
    if (print.summary) {
      ss <- summary(best_nc$Number_clusters)
      cat("Among all indices: \n===================\n")
      for (i in 1:length(ss)) {
        cat("*", ss[i], "proposed ", names(ss)[i], 
            "as the best number of clusters\n")
      }
      cat("\nConclusion\n=========================\n")
      cat("* According to the majority rule, the best number of clusters is ", 
          names(which.max(ss)), ".\n\n")
    }
    df <- data.frame(Number_clusters = names(ss), freq = ss, 
                     stringsAsFactors = TRUE)
    p <- ggpubr::ggbarplot(df, x = "Number_clusters", 
                           y = "freq", fill = barfill, color = barcolor) + 
      labs(x = "Number of clusters k", y = "Frequency among all indices", 
           title = paste0("Optimal number of clusters - k = ", 
                          names(which.max(ss))))
    return(p)
  }
}
# assign them to the factoextra namespace
environment(fviz_nbclust) <- asNamespace("factoextra")
assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
environment(.viz_NbClust) <- asNamespace("factoextra")
assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")

######################################################################
#library(cluster) #Clustering algorithms
#library(clustree)


df.scores <- read_excel("../../results/df-seven-scores.xlsx",sheet=1)
head(df.scores,7)
summary(df.scores)


# Explore dendrograms of various hierarchical clustering methods
options(repr.plot.width=15, repr.plot.height=7)
plot(hclust(dist(df.scores[,c(2:8)]), method ="ward.D2"), main="Countries", xlab="", ylab="", cex=.9, labels=df.scores$Country)
plot(hclust(dist(df.scores[,c(2:8)]), method ="ward.D"), main="Countries", xlab="", ylab="", cex=.9, labels=df.scores$Country)
plot(hclust(dist(df.scores[,c(2:8)]), method ="complete"), main="Countries", xlab="", ylab="", cex=.9, labels=df.scores$Country)
plot(hclust(dist(df.scores[,c(2:8)]), method ="single"), main="Countries", xlab="", ylab="", cex=.9, labels=df.scores$Country)




##### Finding consensus optimal cluster number via various methods with NBclust function

# Ward (D2) - technically correct implemenation of Ward method
optimal.ward = NbClust(data = df.scores[,c(2:8)], distance = "euclidean", min.nc = 3, max.nc = 7, method = "ward.D2", index = "alllong" ,alphaBeale = 0.1)

# Ward (D)
optimal.ward.d = NbClust(data= df.scores[,c(2:8)], distance = "euclidean", min.nc = 3, max.nc = 7, method = "ward.D", index = 'alllong',alphaBeale = 0.1)

# Complete Linkage
optimal.complete = NbClust(data= df.scores[,c(2:8)], distance = "euclidean", min.nc = 3, max.nc = 7, method = "complete", index = 'alllong',alphaBeale = 0.1)

# K-Means
optimal.kmeans = NbClust(data= df.scores[,c(2:8)], distance = "euclidean", min.nc = 3, max.nc = 7, method = "kmeans", index = 'alllong',alphaBeale = 0.1)




######################################################################
# Frequency plot of optimal number of clusters via Ward D2
par(mar=c(1,1,1,1))
# png("../../images/png-image/optimal-clusters-wardd2.png", width = 10, height = 5, units = "in", res = 700,)
pdf(file = "../../images/pdf-images/optimal-clusters-wardd2.pdf", width = 10, height = 5 )
factoextra::fviz_nbclust(optimal.ward) + theme_minimal() + ggtitle("")
dev.off()


# Analysis of selected indices (with 3 clusters as optimal) for various cluster numbers
list(optimal.ward$Best.nc[,c(2,14,15,17,20,22)])
list(optimal.ward$All.index[,c(2,14,15,17,20,22)])
ward.indices <- optimal.ward$All.index[,c(2,14,15,17,20,22)]
#options(repr.plot.width=15, repr.plot.height=10)
par(mar=c(1,1,1,1))
# png("../../images/png-images/optimal-cuts.png", width = 10, height = 5, units = "in", res = 700,)
pdf(file = "../../images/pdf-images/optimal-cuts.pdf", width = 10, height = 5 )
barplot(ward.indices, beside = TRUE, main = "", grid=TRUE,
        col = c( "green","yellow","orange","red","purple"),
        ylab = "Value Index", legend = c("3 clusters","4 clusters", "5 clusters", "6 clusters","7 clusters"),        
        args.legend = list(title = "Number of Clusters", cex = .7, x = "topright", bty = "n",inset=c(0, 0) ))
dev.off()
# Duda - Smallest number of clusters such that index > criticalValue
# Ratkowsky - Maximum value of the index
#22. "mcclain" or "all" or "alllong" - Minimum value of the index



#Dendrogram for the 73 different countries with different cuts. 
#dfsimple <- read_excel("../../results/dfsimple.xlsx",sheet=1)
#dfsimple[is.na(dfsimple)] <- 0


ward.tree = hclust(dist(df.scores[,c(2:8)]), method ="ward.D2")
clusters.ward <- cutree(ward.tree, 3)
df.clusters <- cbind(df.scores[, 1], clusters.ward)
colnames(df.clusters)[2] = 'Cluster'
write_xlsx(df.clusters , '../../results/clusters.xlsx')



pdf(file = "../../images/pdf-images/dendrogram.pdf", width  = 7, height = 14)
par(mar = c(1, 1, 1, 15), cex.axis = 1, cex.lab = 1.4) 
par(cex=1.2)
typology.colors = c('#66c2a5','#fc8d62', '#8da0cb')
dend <- as.dendrogram(ward.tree ,cex = 1 )
dend <- color_labels(dend, k=3, col = typology.colors )
dend <- color_branches(dend, k=3, col = typology.colors)
labels(dend) <- df.scores$Country[ward.tree$order]
dend <- set(dend, "branches_lwd", 2)
# dend %>% set("branches_k_color", value = c('#66c2a5','#fc8d62', '#8da0cb'), k = 3) %>% 
plot(dend, horiz = TRUE, axes = FALSE ) 
legend("topleft", legend=c("Centralized" , "Hybrid", "Decentralized"), fill=rev(typology.colors), bty="n")
dev.off()

