library(readxl)
library(randomForest)
library(tidyverse)
library(dplyr)
library(Rcpp)


library(rpart)# Popular decision tree algorithm
library(rattle)# Fancy tree plot
library(rpart.plot)# Enhanced tree plots
library(RColorBrewer)# Color selection for fancy tree plot
library(party) # Alternative decision tree algorithm
library(partykit)# Convert rpart object to BinaryTree

library(tree)
library(multcomp)


library(Metrics)
library(scales)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(stats4) #Load package stats
library(splines) #Load package splines
library(VGAM) #Load package VGAM


library(corrplot)
library(PerformanceAnalytics)
library(psych)

library(data.table)
library(writexl)



df.wa = read_excel( "../../results/df-water-access.xlsx" ,sheet=1)
df.exp =read_excel("../../results/df-water-explore.xlsx" ,sheet=1)
df.cluster = read_excel("../../results/df-fa-seven-cluster-rank.xlsx" ,sheet=1)
df.wb = read_excel("../../results/df-wb.xlsx" ,sheet=1 )
df.exp$clusters <- as.factor(df.cluster$clusters)
df <- merge(x = df.exp,
            y = df.wb,
            by = c("Country"))

df <- df[, c(1:13, 17,21)]

df$gdpp <- df$cgdp/df$tpop  ## GDP per capita

df <- subset(df, select = -cgdp)

#scaling the world bank data similar to DHS aggregation out of 100
# df.wb <- df[,c(10:16)]
# df.wb <- data.frame(lapply(df.wb, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100)))
# df.scale <- cbind(df, df.wb)
# df.scale <- df.scale[,c(1:8,15:21)]


#Giving unique names for the typology
# "Decentralized" , "Hybrid", "Centralized"
df <- df%>%
    mutate(clusters=case_when(
        .$clusters=="1" ~ "Decentralized",
        .$clusters=="2" ~ "Hybrid",
        .$clusters=="3" ~ "Centralized",
    ))

# df.scale <- df.scale%>%
#     mutate(clusters=case_when(
#         .$clusters=="1" ~ "Decentralized",
#         .$clusters=="2" ~ "Hybrid",
#         .$clusters=="3" ~ "Centralized",
#     ))

write_csv(df, '../../data/tidy/explanatory-variables.csv')

## Model estimation
form <- as.formula(clusters ~ . - Country)
tree.fwa <- rpart(form,data=df,control=rpart.control(minsplit=4,cp=0.01, xval = nrow(df), maxsurrogate = 0, minbucket = 4 )
)

par(mar=c(1,1,1,1))
pdf(file = "../../docs/manuscript/pdf-image/complexity-par-plot.pdf",
    width     = 5,
    height    = 5 )
plotcp(tree.fwa)
dev.off()

printcp(tree.fwa)


tree.pru <- prune(tree.fwa, cp=0.017) # interactively trim the tree

# Development of fancy plots 
#size of the plot 
options(repr.plot.width=10, repr.plot.height=10)
par(mar = c(1,1,1,1))
par(cex=1)
pdf(file = "../../docs/manuscript/pdf-image/decision-tree.pdf",
    width     = 7,
    height    = 7 )
fancyRpartPlot(tree.pru, main ='', sub ='', caption='' ,palettes=c("Blues","Greens", "Reds" ))
dev.off()

summary(tree.pru)

# Variable importance plot
var.imp <- data.frame(tree.fwa$variable.importance)
var.imp <- data.frame(Variable = row.names(var.imp), var.imp)
rownames(var.imp) = NULL
colnames(var.imp)[2] = 'Importance'

var.imp$Variable = c('Private Car Ownership (%)', 'Population Density (/sq. km)', 
          'GDP per capita (2018 US$)', 
          'Wealth Index (Gini Coefficient)'
          )

#var.imp  = read_excel( "../../results/variable-importance.xlsx" ,sheet=1)
s <- ggplot(var.imp , aes(x= reorder(Variable, + Importance), y= Importance)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    coord_flip() +
    theme(text = element_text(size=24))+  #Font size
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=24)) + #Adjusting the tick sizes
    xlab("")

pdf(file = "../../docs/manuscript/pdf-image/variable-importance.pdf",
    width     = 14,
    height    = 7 )

par(mar=c(1,1,1,10))
s
dev.off()
