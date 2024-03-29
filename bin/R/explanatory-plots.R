library(readxl)
library(ggplot2)
library(writexl)
library(dplyr)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(ggpubr)
library(reshape2)
library(data.table)

df.ex = read_excel("../../data/tidy/df-explanatory-variables.xlsx", sheet=1)
df.ex$popd = log10(df.ex$popd)
df.wai = read_excel( "../../data/tidy/df-water-accessibility-indicators.xlsx" ,sheet=1)
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
df.typologies = read_excel("../../results/df-typologies.xlsx" ,sheet=1)

df <- merge(x = df.ex,
            y = df.fa,
            by = c("Country"))

#df$clusters <- as.factor(df$clusters)
df <- merge(x = df, y = df.typologies, by= c("Country"))

df <- df[, c('Country', 'Typology', 'pcar','popd','gdpp', "ML1", "ML2", "ML3", "ML4", "ML5", "ML6","ML7")] # top 3 important explanatory variables from classification decision tree
head(df)

melted.df <- melt(as.data.table(df), id.vars = c('Country','Typology',  'pcar', 'popd', 'gdpp'), 
                  measure.vars = c("ML1", "ML2", "ML3", "ML4", "ML5", "ML6","ML7"), 
                  variable.name = 'Factor', value.name = 'Score')
head(melted.df)

melted.df2 <- melt(melted.df, id.vars = c('Country','Typology','Factor','Score'), measure.vars = c( 'pcar', 'popd', 'gdpp'), 
                  variable.name = 'Variable', value.name = 'Value')
head(melted.df2)


variable_names <- c(`pcar` = 'HH Private Car Ownership (%)', 
                    `popd` = 'Population Density (log /sq. km)', 
                    `gdpp` = 'GDP per capita (2018 US$)'
                    #`mcyc` = 'Motorcycle Ownerhsip (%)'
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
p <- ggplot(melted.df2, aes(Value, Score, color = Typology)) + 
    geom_point(alpha=.85) +
    geom_smooth(method = 'loess', color = 'steelblue', span = 1.2 ) + #formula = y ~ s(x, bs = "cs")
    scale_color_manual(values=c('#8da0cb','#66c2a5', '#fc8d62')) +
    theme_bw() +
    labs(title=, color = 'Typology') +
    geom_text_repel(
        aes(label = Country), size = 3.5,
        force= .2,
        force_pull = 2,
        angle = 0,
        fontface = 2,
        min.segment.length = 0.5,
        segment.linetype = 1,
        max.overlaps = 4,show.legend=FALSE) +
    theme(text = element_text(size=16), strip.text = element_text(size=18)) + #Font size
    theme(axis.text = element_text(size = 13), legend.text = element_text(size=16)) + #All tick size  
    theme(axis.title = element_text(size = 18)) +  # Adjusting Axis Title  
    theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=1), color = guide_legend(override.aes = list(size = 5)))

pdf(file = "../../images/pdf-images/explanatory-plots-grid.pdf",
    width     = 12,
    height    = 16 )

p + facet_grid(rows = vars(Factor), cols = vars(Variable), scales = 'free_x', 
               labeller = labeller(Factor = as_labeller(factor_names), Variable = as_labeller(variable_names)))

dev.off()

