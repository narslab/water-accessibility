## Collection of the library for the code ###
library(readxl)
library(RColorBrewer)
library(corrplot)
#install.packages("psych")
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(psych)
library(mvtnorm) # col.norm
library(tilting) # col.norm
library(ggplot2)
library("np") #npreg: you may need to install it
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library(tidyverse) #data Manipulation
library(cluster) #Clustering algorithms
library(factoextra)#Clustering Algorithms & Visualization
library(gridExtra)
#install.packages("dendextend")
#install.packages("ggplot2")
#install.packages("rlang")
library(dendextend) # For fancy deprograms
library(ggplot2) # ggplot
library(rlang) # clustering cutting
#install.packages('NbClust')
library(NbClust)
#installed.packages('fmsb')
library(fmsb) #For Spider Plots
### PRE-PROCESSING  ###
df <- read_excel("Data/Cumulative_Data.xlsx",sheet=1)
head(df)
### Cleaning Column Years ###
for (i in seq(1, nrow(df))) {
    if (grepl("-", df[i,'Year'])) {
        yy1 = substr(df[i,"Year"], start=1,stop=2)
        yy2 = substr(df[i,"Year"], start=6,stop=7)
        yy3 = paste(yy1,yy2,sep = "")
        #yy3 = as.integer(yy3)
        df[i,"Year"] = yy3
    }
    }
### Finding the Max Years ###
df$Year = as.numeric(df$Year)
df1 <- matrix(NA, nrow = 0, ncol = length(colnames(df))) #create new empty DF
colnames(df1) = colnames(df) # assign column names to empty data frame
for (country in unique(df$Country)) {
    maxyear = max(df[df$Country==country,'Year'])
    df1 = rbind( df1, df[(df$Country==country) & (df$Year==maxyear),])
}        
### Removing Population Variables (Removal of duplication) ###
dfh = df1[,!grepl("^P_",names(df1))]
### NA and zeros Analysis ###
na_count = colSums(is.na(dfh)) #Counting all your NA in each data frames
na_count
### Removing column for household data frame if NA is greater than 50% ###
dfsimple = dfh[, which(colMeans(!is.na(dfh)) > 0.5)]
head(dfsimple)
na_count_cleaned = colSums(is.na(dfsimple)) #Counting all your NA in each data frames
na_count_cleaned
hist(na_count_cleaned)
zeros = colSums(dfsimple != 0) # Counting all your zeros in each data frame
zeros # No zeros in the data frame which has been simplified
### Preliminary Data Visualization ###
hist(dfsimple$ 'Year', main="Year of surveys",
     xlab="Years",
     ylab="Frequency",
     col.main="red", col.lab="blue")
summary(dfsimple$uiws ) # House hold with unimproved water source
hist(dfsimple$uiws, main="Households using an unimproved water source",
     xlab="Total Percentage",
     ylab="Frequency",
     col.main="red", col.lab="blue")
plot(dfsimple$uiws , main="Households using an unimproved water source",
     xlab="Index",
     ylab="Total Percentage",
     col.main="red", col.lab="blue")
summary(dfsimple$bicyc) # house hold with possession of bicycle. 
hist(dfsimple$bicyc, main="Households possessing a bicycle",
     xlab="Total Percentage",
     ylab="Frequency",
     col.main="red", col.lab="blue")
plot(dfsimple$bicyc, main="Households possessing a bicycle",
     xlab="Index",
     ylab="Total Percentage",
     col.main="red", col.lab="blue")
### Correlation Plots ###
#Data Frame 1
head(dfsimple)
#Created all the NA to zeros to have a numerical study of the data
dfsimple[is.na(dfsimple)] <- 0 # Convert all your NA to 0 to keep numeric values
my_data <- dfsimple[, c(4:31)]
head(my_data, 31)
res <- cor(my_data)
round(res, 2)
M<-cor(my_data)
head(round(M,2))
corrplot(M, type="upper", tl.cex = 1)
### Advanced Correlation Graphs ###
#sample for accessibility
my_data_acc <- dfsimple[, c(6:13,15:21)]
chart.Correlation(my_data_acc, histogram=TRUE, pch=19 , tl.cex = .7 )
#sample for location/premise
my_data_loc <- dfsimple[, c(21:25)]
chart.Correlation(my_data_loc, histogram=TRUE, pch=19 , tl.cex = .5 )
#sample of wealth
my_data_wealth <- dfsimple[, c(30:31)]
chart.Correlation(my_data_wealth, histogram=TRUE, pch=19 , tl.cex = .5 )
#sample for transportation
my_data_move <- dfsimple[, c(26:30)]
chart.Correlation(my_data_move, histogram=TRUE, pch=19 , tl.cex = .5 )
#Comparison with Transportation and Time it gets to the source of water
my_data_time <- dfsimple[, c(26:30,21:25)]
chart.Correlation(my_data_time, histogram=TRUE, pch=19 , tl.cex = .5 )
#sample for transportation compared with wealth
my_data_money <- dfsimple[, c(26:31)]
chart.Correlation(my_data_money, histogram=TRUE, pch=19 , tl.cex = .5 )
#sample for transportation, water retrieving part 1 comparison
my_data_water1 <- dfsimple[, c(26:30, 6:13)]
chart.Correlation(my_data_water1, histogram=TRUE, pch=19 , tl.cex = .5 )
#sample for transportation, water retrieving part 2 comparison
my_data_water2 <- dfsimple[, c(26:30, 15:21)]
chart.Correlation(my_data_water2, histogram=TRUE, pch=19 , tl.cex = .5 )
### Principal Component Analysis ###
# PCA Scree plot without standardizing data
hpca_dfsimple <- prcomp(my_data, scale=FALSE) # Scale to 0 to 1 # We can extract the information summarized above (and much more) # from the attributes of the object hpca_dfsimple

standard_deviation_of_each_component <- hpca_dfsimple$sdev
var_per_dim <- standard_deviation_of_each_component^2
var_tot <- sum(var_per_dim)
var_tot
var_per_dim/var_tot

var_prop <- var_per_dim / sum(var_per_dim)
var_prop

cum_var <- cumsum(var_prop)
cum_var
plot(cum_var,xlab="Principal component", 
     ylab="Proportion of variance explained", ylim=c(0,1), type='b')
apply(my_data, 2, mean)
apply(my_data, 2, var)
hpca_cor <- prcomp(my_data, scale=TRUE) #Using the correlation matrix to obtain the eigenvalue #Single decomposition of the variance matrix.
standard_deviation_of_each_component <- hpca_cor$sdev
var_per_dim <- standard_deviation_of_each_component^2
var_tot <- sum(var_per_dim)
var_prop <- var_per_dim / sum(var_per_dim)
cum_var <- cumsum(var_prop)
plot(cum_var,xlab="Principal component", 
     ylab="Proportion of variance explained", ylim=c(0,1), type='b')
eigenvectors <- hpca_cor$rotation
col.norm(eigenvectors)
eigenvectors

PC_contr <- eigenvectors[,c("PC1")] # Let us plot the contribution of the original dimension to the 1st PCA # PC_contr
ord <- order( -abs(PC_contr) )# We order by the magnitude of the contribution # We use the - sign because we want a descending order
PC_contr <- PC_contr[ord] #PC_contr
### Loading Bars for 12 Dimensions ###
PC_contr1 <- PC_contr[1:7] # We just select the 7 highest contributing dimensions (highest loading)
PC_contr1
barplot(PC_contr1, main="Contribution to the 1st component", xlab="Original Dimensions") 

# Second principal component vector
PC_contr <- eigenvectors[,c("PC2")] # We order by the magnitude of the contribution
ord <- order( -abs(PC_contr) ) # We use the - sign because we want a descending order
PC_contr <- PC_contr[ord]
PC_contr2 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr2, main="Contribution to the 2nd component",xlab="Original Dimensions") 
# Third principal component vector
PC_contr <- eigenvectors[,c("PC3")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr3 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr3, main="Contribution to the 3rd component",xlab="Original Dimensions") 
# 4 principal component vector
PC_contr <- eigenvectors[,c("PC4")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr4 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr4, main="Contribution to the 4th component",xlab="Original Dimensions") 
# 5 principal component vector
PC_contr <- eigenvectors[,c("PC5")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr5 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr5, main="Contribution to the 5th component",xlab="Original Dimensions") 
# 6 principal component vector
PC_contr <- eigenvectors[,c("PC6")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr6 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr6, main="Contribution to the 6th component",xlab="Original Dimensions") 
# 7 principal component vector
PC_contr <- eigenvectors[,c("PC7")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr7 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr7, main="Contribution to the 7th component",xlab="Original Dimensions") 
# 8 principal component vector
PC_contr <- eigenvectors[,c("PC8")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr8 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr8, main="Contribution to the 8th component",xlab="Original Dimensions") 
# 9 principal component vector
PC_contr <- eigenvectors[,c("PC9")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr9 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr9, main="Contribution to the 9th component",xlab="Original Dimensions") 
# 10 principal component vector
PC_contr <- eigenvectors[,c("PC10")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr10 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr10, main="Contribution to the 10th component",xlab="Original Dimensions") 
# 11 principal component vector
PC_contr <- eigenvectors[,c("PC11")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr11 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr11, main="Contribution to the 11th component",xlab="Original Dimensions") 
# 12 principal component vector
PC_contr <- eigenvectors[,c("PC12")]
ord <- order( -abs(PC_contr) )
PC_contr <- PC_contr[ord]
PC_contr12 <- PC_contr[1:7]
options(repr.plot.width=12, repr.plot.height=8)
barplot(PC_contr12, main="Contribution to the 12th component",xlab="Original Dimensions")
scatter.smooth(PC_contr1,PC_contr2)
#### Basics PCA with Color Visualization ####
res.pca <- PCA(my_data, graph = FALSE)
print(res.pca)
eig.val <- get_eigenvalue(res.pca) #The object that is created using the function PCA() contains many information found in many different lists and matrices. These values are described in the next section.
eig.val
#An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in
#standardized data. This is commonly used as a cutoff point for which PCs are retained. This holds true only when
#the data are standardized.
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 76))
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord, 36)
# Cos2: quality on the factor map
head(var$cos2,36)
# Contributions to the principal components
head(var$contrib, 36)
#For all the 36 variables.
fviz_pca_var(res.pca, col.var = "black")
#The plot above is also known as variable correlation plots. It shows the relationships between all variables. It can be interpreted as follow:
#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. Variables that
#are away from the origin are well represented on the factor map.
corrplot(var$cos2, is.corr=FALSE, tl.cex = 1 )
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2, tl.cex = 1)
#A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is
#positioned close to the circumference of the correlation circle.
#A low cos2 indicates that the variable is not perfectly represented by the PCs.
#In this case the variable is close to the center of the circle.
#The cos2 values are used to estimate the quality of the representation
#The closer a variable is to the circle of correlations,
#the better its representation on the factor map (and the more important it is to interpret these components)
#Variables that are closed to the center of the plot are less important for the first components.
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping, 
    )
#variables with low cos2 values will be colored in "white"
#variables with mid cos2 values will be colored in "blue"
#variables with high cos2 values will be colored in red
corrplot(var$contrib, is.corr=FALSE,tl.cex = 1) 
#The larger the value of the contribution, the more the variable contributes to the component.
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
#It can be seen that the variables H IWS P and H W - contribute the most to the dimensions 1 and 2.
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2
ind <- get_pca_ind(res.pca)
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2, tl.cex = .5)
### Clustering ###
# To apply clustering, we want to only use water accessibility variables
df.wa <- dfsimple[,c('ptap','boreh','pspr', 'uspr', 'pwell', 'uwell','surw','30less','30more','rainw','pdwel' )]
head(df.wa)
sapply(df.wa, class)
###linkage: single or average or complete and Different Methods ###
hc=hclust(dist(df.wa), method ="single")

options(repr.plot.width=15, repr.plot.height=7)
plot(hc, main="Countries", xlab="", 
     ylab="", cex=.9, labels=df1$Country)

hc=hclust(dist(df.wa), method ="average")

options(repr.plot.width=15, repr.plot.height=7)
plot(hc, main="Countries", xlab="", 
     ylab="", cex=.9, labels=df1$Country)

hc.complete =hclust(dist(df.wa), method ="complete")

options(repr.plot.width=15, repr.plot.height=7)
plot(hc.complete, main="Countries", xlab="", 
     ylab="", cex=.9, labels=df1$Country)

hc.ward=hclust(dist(df.wa), method ="ward.D")

options(repr.plot.width=15, repr.plot.height=7)
plot(hc.ward, main="Countries", xlab="", 
     ylab="", cex=.9, labels=df1$Country)

hc.ward2=hclust(dist(df.wa), method ="ward.D2")

options(repr.plot.width=15, repr.plot.height=7)
plot(hc.ward2, main="Countries", xlab="", 
     ylab="", cex=.9, labels=df1$Country)
### Determining the Best Cut for different methods ###
optimald2 = NbClust(data= df.wa, distance = "euclidean", min.nc = 3, max.nc = 7, method = "ward.D2", index = 'all',alphaBeale = 0.1)
optimald = NbClust(data= df.wa, distance = "euclidean", min.nc = 3, max.nc = 7, method = "ward.D", index = 'all',alphaBeale = 0.1)
optimalcomplete = NbClust(data= df.wa, distance = "euclidean", min.nc = 3, max.nc = 7, method = "complete", index = 'all',alphaBeale = 0.1)
optimal.kmeans = NbClust(data= df.wa, distance = "euclidean", min.nc = 3, max.nc = 7, method = "kmeans", index = 'all',alphaBeale = 0.1)

list(optimald2$Best.nc)
list(optimald$Best.nc)
list(optimalcomplete$Best.nc)
list(optimal.kmeans$Best.nc)

list(optimald2$All.index)
index.d2 <- optimald2$All.index[,c(1 , 3:4 , 10,17 )]
dev.off()
barplot(index.d2, beside = TRUE, main = "Optimal Number of Clusters for Ward.D2",
        col = c("yellow", "green", "yellow","pink","red"),
        ylab = "Value Index", legend = c("3 clusters", "4 clusters", "5 clusters","6 clusters","7 clusters"), 
        args.legend = list(title = "Number of Clusters", cex = .7))

### Cutting at different tree levels 4 which is the optimal for Ward.D2 ###
dfsimple$clusters4 <- cutree(hc.ward2,4)
dfsimple$clusters4

clusters_wa <- cutree(hc.ward2,4)
### Fancy Clustering Graph Majority ### 
dend <- as.dendrogram(hc.ward2 )
dend <- color_branches(dend, k=4, col = c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c"))
labels(dend) <- df1$Country[hc.ward2$order]
options(repr.plot.width=17, repr.plot.height=7)
par(mar = c(3,5,1,10))
par(cex=0.6)
plot(dend, main="Cluster dendrogram", xlab="height", 
     ylab="",horiz=TRUE )


df.wa.d2.avg <- mutate(df.wa, cluster = clusters_wa)
count(df.wa.d2.avg, cluster)

sapply(unique(clusters_wa),function(g)dfsimple$Country[clusters_wa == g])

dend <- as.dendrogram(hc.ward)
dend <- color_branches(dend, k=4)
labels(dend) <- df1$Country[hc.ward$order]
options(repr.plot.width=17, repr.plot.height=7)
plot(dend, main="Countries(Ward.D)", xlab="", 
     ylab="", cex=.5,  )

dend <- as.dendrogram(hc.complete)
dend <- color_branches(dend, k=4)
labels(dend) <- df1$Country[hc.complete$order]
options(repr.plot.width=17, repr.plot.height=7)
plot(dend, main="Countries(Complete)", xlab="", 
     ylab="", cex=.5,  )

### K-Mean Clustering in R ###
distance <- get_dist(df.wa)
labels(distance) <- df1$Country
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high="#FC4E07"), lab_size = 7 )
k2 <- kmeans(df.wa, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df.wa ,  )
# it is often advantageous to use several different values of k and examine the differences in the results.
# We can execute the same process for 3, 4, and 5 clusters, and the results are shown in the figure:
k3 <- kmeans(df.wa, centers = 3, nstart = 25)
k4 <- kmeans(df.wa, centers = 4, nstart = 25)
k5 <- kmeans(df.wa, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df.wa) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df.wa) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df.wa) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df.wa) + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)
str(k3)
k3
fviz_cluster(k3, data = df.wa ,  )
### Gap Stat Method ###
set.seed(123)
gap_stat <- clusGap(df.wa, hcut, hc_method ="ward.D2" ,
                    K.max = 10, B = 1000,  method = c('Tibs2001SEmax'))
fviz_gap_stat(gap_stat)


set.seed(123)
gap_stat <- clusGap(df.wa, hcut, hc_method ="ward.D" ,
                    K.max = 10, B = 1000, method ='Tibs2001SEmax')
fviz_gap_stat(gap_stat)
set.seed(123)
gap_stat <- clusGap(df.wa, hcut, hc_method ="complete" ,
                    K.max = 10, B = 1000)
fviz_gap_stat(gap_stat)
### Fancy Clustering Graph for WARD D2 Gap ### 
dend <- as.dendrogram(hc.ward2)
dend <- color_branches(dend, k=8)

labels(dend) <- df1$Country[hc.ward2$order]
options(repr.plot.width=17, repr.plot.height=7)
plot(dend, main="Countries", xlab="", 
     ylab="", cex=.3,  )
### Ward Gap ###
dend <- as.dendrogram(hc.ward)
dend <- color_branches(dend, k=8)

labels(dend) <- df1$Country[hc.ward$order]
options(repr.plot.width=17, repr.plot.height=7)
plot(dend, main="Countries", xlab="", 
     ylab="", cex=.3,  )

### Complete Gap ###
dend <- as.dendrogram(hc.complete)
dend <- color_branches(dend, k=7)

labels(dend) <- df1$Country[hc.complete$order]
options(repr.plot.width=17, repr.plot.height=7)
plot(dend, main="Countries", xlab="", 
     ylab="", cex=.3,  )
### Histogram of the 11 Water Accessibility variables ###
summary(df.wa)
chart.Correlation(df.wa, histogram=TRUE, pch=19 , tl.cex = .7 )
### Spider Plots of the variables ###
# Create data: Water Accessibility Aggregate
df.wa.aggregate <- aggregate(df.wa, list(clusters=dfsimple$clusters4), mean)
df.wa.aggregate <- df.wa.aggregate[,2:12]
df.wa.aggregate

### Extract table of cluster centroids. ###
centroid <- apply(df.wa.aggregate, 1, function(x) sqrt(sum(x^2)) ) #How close is each country to the cen-troid of the cluster
centroid
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
dev.off()
bp <- barplot(centroid,
        main = "Centroid of the Clusters", xlab = "Clusters", ylab = "fitness of Centroid",
        col = c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c"),
        ylim = c(0, 100), names.arg = c("A", "B", "C","D"))
text(bp, 0, round(centroid, 1),cex=1,pos=3)
dev.off()
barplot(as.matrix(df.wa.aggregate),
        beside = TRUE,
        main = "Typology average of Clusters", xlab = "Water variables", ylab = "Average",
        col = c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c"),
        legend = c("A", "B", "C","D"), 
        args.legend = list(title = "Clusters", x = "topleft", cex = .7), ylim = c(0, 70))


centroiddf.wa <- apply(df.wa, 1, function(x) sqrt(sum(x^2)) ) #How close is each country to the cen-troid of the cluster
centroiddf.wa

### Radar Plot for the aggregated data ###
# To use the fmsb package, I have to add 2 lines to the data frame: the max and min of each topic to show on the plot!
max.aggregate = apply(df.wa.aggregate,2,max)
apply(df.wa.aggregate,2,min)
max.aggregate

norm.df.wa <- sweep(df.wa.aggregate, 2, max.aggregate, FUN = '/')
data.spider <- rbind(rep(70,70) , rep(0,10) , df.wa.aggregate)
colnames(data.spider) <- c("water 30 minutes or < away round trip" , "Water > 30 minutes away round trip" , "surface water" , "protected well" , "unprotected well water", "borehole" , "public tap" , "unprotected spring", "protected spring", "rainwater" ,"piped into dwelling")


# The default radar chart for First Cluster
r.c. <- radarchart(data.spider,
           #custom polygon
           pcol= c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c") ,  plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels= seq(0,80,20), cglwd=0.8, axistype = 4,
           title = 'Cluster Centroid',
           vlcex=1,
           calcex = 1
           )
colnames(norm.spider) <- c("water 30 minutes or < away round trip" , "Water > 30 minutes away round trip" , "surface water" , "protected well" , "unprotected well water", "borehole" , "public tap" , "unprotected spring", "protected spring", "rainwater" ,"piped into dwelling")

r.c <- radarchart(norm.spider,
           #custom polygon
           pcol= c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c")  ,  plwd=5 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels= seq(0,1,.25), cglwd=0.8, axistype = 4,
           title = '(Households) Four Cluster of Ward.D2 Normative value',
           vlcex =.8
)



