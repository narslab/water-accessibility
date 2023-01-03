library(readxl)
library(rpart)# rpart
library(rattle)# fancyRpartPlot
#library(rpart.plot)# Enhanced tree plots
library(RColorBrewer)# Color selection for fancy tree plot
#require(ggplot2)
library(reshape2)
library(data.table)
library(writexl)


### Modify plotcp
plotcp <- function(x, minline = TRUE, lty = 3, col = 1,
                   upper = c("size", "splits", "none"), ...)
{
  dots <- list(...)
  if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")
  upper <- match.arg(upper)
  p.rpart <- x$cptable
  if (ncol(p.rpart) < 5L)
    stop("'cptable' does not contain cross-validation results")
  xstd <- p.rpart[, 5L]
  xerror <- p.rpart[, 4L]
  nsplit <- p.rpart[, 2L]
  ns <- seq_along(nsplit)
  cp0 <- p.rpart[, 1L]
  cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
  if (! "ylim" %in% names(dots))
    dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + xstd) + 0.1)
  do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "Cost-complexity parameter", 
                       ylab = "Cross-validation error rate", type = "o"), dots))
  box()
  axis(2, ...)
  segments(ns, xerror - xstd, ns, xerror + xstd)
  axis(1L, at = ns, labels = as.character(signif(cp, 2L)), ...)
  switch(upper,
         size = {
           axis(3L, at = ns, labels = as.character(nsplit + 1), ...)
           mtext("Size of tree (number of terminal nodes)", side = 3, line = 3)
         },
         splits = {
           axis(3L, at = ns, labels = as.character(nsplit), ...)
           mtext("number of splits", side = 3, line = 3)
         })
  minpos <- min(seq_along(xerror)[xerror == min(xerror)])
  if (minline) abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
  invisible()
}


## Data
df.exp =read_excel("../../data/tidy/df-explanatory-variables.xlsx" ,sheet=1)
df.typologies = read_excel("../../results/df-typologies.xlsx" ,sheet=1)
df.exp$Typology <- df.typologies$Typology

df.exp <- df.exp %>% 
  rename(
    'Bicycle (%)' = bicy ,
    'A Cart (%)' = cart,
    'Motorcycle (%)' = mcyc,
    'Car (%)'=pcar,
    'Boat (%)'=boat,
    'Gini' =wigc,
    'GDP p.c. ($)'= gdpp,
    'Pop'=tpop,
    'Urban Pop (%)'=upop,
    'Pop. Den. (/sq. km)' =popd,
    'Area (sq. km)'=land,
    'RW (p.c. cu. m)'=rifr,
    'Prec (mm/yr)' =prec
  )

## Model estimation
set.seed(1)
form <- as.formula(Typology ~ . - Country)
tree.fwa <- rpart(form,data=df.exp,
                  control=rpart.control(minsplit=10, cp=0.01, xval = 5, maxsurrogate = 0, minbucket = 4 )
)

# Plot cross-val error vs cost complexity
pdf(file = "../../images/pdf-images/complexity-par-plot.pdf",
    width     = 5,
    height    = 5 )
par(mar=c(5,5,5,5))
plotcp(tree.fwa)
#title(xlab = "Cost-complexity parameter", ylab = "Cross-validation error rate")
dev.off()

printcp(tree.fwa)

write_xlsx(as.data.frame(tree.fwa$cptable), "../../results/decision-tree-cost-complexity-table.xlsx")

tree.pru <- prune(tree.fwa, cp=0.030) #cp=0.017 # prune tree
summary(tree.pru)

# Decision tree plot
par(cex=1)
pdf(file = "../../images/pdf-images/decision-tree.pdf",
    width     = 7,
    height    = 5 )
par(mar=c(0,0,0,0))
fancyRpartPlot(tree.pru, main ='', sub ='', caption='' ,palettes=c("Blues","Greens", "Reds" ), ycompact=TRUE)
dev.off()


### Variable importance
var.imp <- data.frame(tree.pru$variable.importance)
var.imp <- data.frame(Variable = row.names(var.imp), var.imp)
rownames(var.imp) = NULL
colnames(var.imp)[2] = 'Importance'

var.imp[var.imp =='Car (%)'] = "HH Private Car Ownership (%)"
var.imp[var.imp =='Pop. Den. (/sq. km)'] = "Population Density (/sq. km)"
var.imp[var.imp =='GDP p.c. ($)'] = "GDP per capita (2018 US$)"
#var.imp[var.imp =='mcyc'] = "Household Motorcycle Ownership (%)"


write_xlsx(var.imp, "../../results/variable-importance.xlsx")

# Plot
s <- ggplot(var.imp , aes(x= reorder(Variable, + Importance), y= Importance)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    coord_flip() +
    theme(text = element_text(size=20))+  #Font size
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20)) + #Adjusting the tick sizes
    ylab("Importance Score") + xlab("")

pdf(file = "../../images/pdf-images/variable-importance.pdf",
    width     = 13,
    height    = 3 )

par(mar=c(1,1,1,15))
s
dev.off()
