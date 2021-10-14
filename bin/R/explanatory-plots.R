library(readxl)
library(ggplot2)
library(writexl)
library(dplyr)
#install.packages("gridExtra")
library(gridExtra)
library(ggrepel)
#library(reshape2)
library(tidyverse)

df.ex = read.csv("../../data/tidy/explanatory-variables.csv")
df.ex$popd = log(df.ex$popd)
df.wa = read_excel( "../../results/df-water-access.xlsx" ,sheet=1)
df.fa <- read_excel("../../results/seven-scores-no-scale.xlsx",sheet=1)
# df.fa <- df.fa %>% 
#     rename(
#         'Far Well' = ML1,
#         'Vended'= ML2,
#         'Piped Indoors'= ML3,
#         'Nearby Improved'= ML4,
#         'Piped Outdoors' = ML5,
#         'Far Spring' = ML6,
#         'Nearby Surface'= ML7
#     )
df <- merge(x = df.ex,
            y = df.fa,
            by = c("Country"))
df$clusters <- as.factor(df$clusters)

df <- df[, c('Country', 'clusters', 'pcar','popd','gdpp',  colnames(df)[16:22])] # top 3 important explanatory variables from classification decision tree
head(df)

melted.df <- melt(as.data.table(df), id.vars = c('Country','clusters',  'pcar', 'popd', 'gdpp'), measure.vars = colnames(df)[6:12], 
                  variable.name = 'Factor', value.name = 'Score')
head(melted.df)

melted.df2 <- melt(melted.df, id.vars = c('Country','clusters','Factor','Score'), measure.vars = c( 'pcar', 'popd', 'gdpp'), 
                  variable.name = 'Variable', value.name = 'Value')
melted.df2


variable_names <- c(`pcar` = 'Private Car Ownership (%)', 
                    `popd` = 'Population Density (/sq. km)', 
                    `gdpp` = 'GDP per capita (2018 US$)'
                    )

factor_names <-  c( 
    `ML1` = 'Far Well',
    `ML2` = 'Vended',
    `ML3` = 'Piped Indoors',
    `ML4` = 'Nearby Improved',
    `ML5` = 'Piped Outdoors',
    `ML6` = 'Far Spring',
    `ML7` = 'Nearby Surface'
)

par(mar = c(1,1,1,1))
par(cex=1.4)


p <- ggplot(melted.df2, aes(Value, Score, color = clusters)) + 
    geom_point() +
    geom_smooth( method = 'gam' ,color = 'steelblue' ) +
    scale_color_manual(values=c('#8da0cb','#66c2a5', '#fc8d62')) +
    labs(title=, color = 'Typology') +
    # geom_text_repel(
    #     aes(label = Country), size = 3,
    #     force= 1,
    #     force_pull = 1,
    #     angle = 0,
    #     fontface = 2,
    #     segment.linetype = 5,
    #     max.overlaps = Inf) +     
    theme(text = element_text(size=20)) + #Font size
    theme(axis.text = element_text(size = 20)) + #All tick size  
    theme(axis.title = element_text(size = 20))  # Adjusting Axis Title  

pdf(file = "../../docs/manuscript/pdf-image/explanatory-plots-grid.pdf",
    width     = 12,
    height    = 12 )

p + facet_grid(rows = vars(Factor), cols = vars(Variable), scales = 'free_x', 
               labeller = labeller(Factor = as_labeller(factor_names), Variable = as_labeller(variable_names)) )

dev.off()
