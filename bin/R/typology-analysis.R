library(readxl)
library(dplyr) # rename
library(ggplot2)
library(PerformanceAnalytics) #chart.Correlation
library(RColorBrewer) #brewer.pal
typology.colors = brewer.pal(n = 3, name = "Set2") # Best color scheme is Set2 for color blind friendly 

library(fmsb) #radarchart
library(rworldmap) #joinCountryData2Map
library(tidyr) #drop_na
require(mapproj) # required for coord_map function (otherwise map won't display)

library(reshape2)

### Data
df.clusters <- read_excel("../../results/clusters.xlsx")
df.scores <- read_excel("../../results/df-seven-scores.xlsx",sheet=1)
df.scores <- df.scores %>% 
  rename(
    'Far Well' = ML1,
    'Vended'= ML2,
    'Piped Indoors'= ML3,
    'Nearby Improved'= ML4,
    'Piped Outdoors' = ML5,
    'Far Spring' = ML6,
    'Nearby Surface'= ML7
  )
df.scores.only <- df.scores[,c(2:8)]


### Plot factor correlations
options(repr.plot.width=10, repr.plot.height=10)                  
par(mar=c(1,1,1,1))
pdf(file = "../../images/pdf-images/factor-cor.pdf", width = 10, height = 10 )
chart.Correlation(df.scores.only, histogram=TRUE, pch=19 , tl.cex = .7 )
dev.off()


### Typology factor averages and centroids
df.scores.ave <- aggregate(df.scores.only, list(Cluster=df.clusters$Cluster), mean)
df.scores.ave
centroids <- apply(df.scores.ave, 1, function(x) sqrt(sum(x^2)) ) 

#options(repr.plot.width=10, repr.plot.height=7)                  
#old.par <- par(mar = c(0, 0, 0, 0))
#par(old.par)

# Centroid plot
pdf(file = "../../images/pdf-images/typology-centroids.pdf", width = 5, height = 5 )
par(cex.axis=1.3, cex.lab=1.3)
barplot(centroids,
              main = "", xlab = "", ylab = "Centroid of factor averages", ylim = c(0,4),
              col = typology.colors,
              names.arg = c("Decentralized" , "Hybrid", "Centralized"))
#grid(lwd =1, lty='solid', order=1)
dev.off()       


# Factor averages plot
pdf(file = "../../images/pdf-images/typology-factor-averages.pdf", width = 15, height = 10 )
par(cex.axis=1.3, cex.lab=1.3)
barplot(as.matrix(df.scores.ave[,c(2:8)]),
        beside = TRUE,
        main = "", xlab = "", ylab = "Average factor score",
        col = typology.colors,
        legend = c("Decentralized" , "Hybrid", "Centralized"), 
        args.legend = list(title = "Typology", x = "topright", cex = 1.2), ylim = c(0, 1))
dev.off()                         


# Spider plot of factor score averages by typology
data.spider <-   df.scores.ave[,c(2:8)]    
rownames(data.spider) <- c("Decentralized" , "Hybrid", "Centralized")   
data.spider <- rbind(rep(1,1) , rep(0,1) , df.scores.ave[,c(2:8)])
head(data.spider)

#par(cex=1.4)
pdf(file = "../../images/pdf-images/spider-plot.pdf", width = 15, height = 9 )
par(mar = c(0,0,0,0))                       
radarchart(data.spider,
           pcol= typology.colors,
           plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels= seq(0,1,.2), 
           cglwd=0.8, axistype = 4,
           title = '',
           vlcex= 1.5,
           calcex = 1.4
           ) 
legend(x=1.5, y=1.25, legend = c("Decentralized" , "Hybrid", "Centralized"), bty = "n", pch='-' , 
       col=typology.colors , text.col = "black", cex=1.5, pt.cex=3)                       
dev.off()


plotTypologyWorldMap <- function(df){
  colnames(df) = c('region', 'value') # change column names for ggplot
  
  # This is to convert country names in df to match those in the worldMap dataframe
  coords <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "region")
  coords = data.frame(coords)
  colnames(coords) = tolower(colnames(coords))
  colnames(coords)
  coords = drop_na(coords, value)
  print(coords$name)
  setdiff(df$region,coords$name)
  
  # Correct the country names in DF
  df[df=='Timor-Leste'] = "East Timor"
  df[df=='Central African Republic'] = "Central African Rep."
  df[df=='Cote d\'Ivoire'] = "Ivory Coast"
  df[df=='Dominican Republic'] = "Dominican Rep."
  df[df=='Kyrgyz Republic'] = "Kyrgyzstan"
  df[df=='Eswatini'] = "Swaziland"
  df[df=='Congo'] = "Congo (Brazzaville)"
  df[df=='Congo Democratic Republic'] = "Congo (Kinshasa)"
  
  print(head(df))
  # Get world map data (lat/long)
  countrynames = df$region
  worldMap <- getMap()
  country_indices <- which(worldMap$NAME%in%countrynames)
  
  # Get coordinates for all countries to plot base map
  allCoords <- lapply(seq(1,243), function(i){
    dfn <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    dfn$region =as.character(worldMap$NAME[i])
    colnames(dfn) <- list("long", "lat", "region")
    return(dfn)
  })
  allCoords <- do.call("rbind", allCoords)

  # Get coordinates strictly for those in water accessibility dataset
  waterCoords <- lapply(country_indices, function(i){
    dfn <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    dfn$region =as.character(worldMap$NAME[i])
    colnames(dfn) <- list("long", "lat", "region")
    return(dfn)
  })
  
  waterCoords <- do.call("rbind", waterCoords)
  waterCoords$value <- df$value[match(waterCoords$region,df$region)]
  waterCoords$value <- factor(waterCoords$value)
  
  # Plot
  #options(repr.plot.width=10, repr.plot.height=7)
  par(mar = c(0,0,0,0))
  par(cex=1)
  m <- ggplot() +
    geom_sf(color = "black", fill= 'antiquewhite') +
    xlab("") + ylab("") +
    geom_polygon(data= allCoords, mapping = aes(x =long, y=lat, group = region), color="grey", fill=NA) + 
    geom_polygon( data = waterCoords, mapping = aes(x = long, y = lat, group = region, fill=value), linewidth=.3,color="black") +
    expand_limits(x = waterCoords$long, y = waterCoords$lat)  + 
    scale_fill_brewer(palette='Set2', name="", #"Water Accessibility Typologies", 
                      na.value = "white",
                      labels = c("Decentralized" , "Hybrid", "Centralized") )  + 
    theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "white")
    ) + 
    theme(legend.position = c(.8,-0.05), legend.direction = 'horizontal',
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(), 
          axis.text=element_text(size=18),
          axis.title=element_text(size=18)) +
    theme(legend.key.size = unit(.7, 'cm'), #change legend key size
          #legend.key.height = unit(.7, 'cm'), #change legend key height
          #legend.key.width = unit(.7, 'cm'),
          legend.title = element_text(size=18,face=),
          legend.text = element_text(size=18)) +
    scale_x_continuous(limits = c(-145, 190)) +
    scale_y_continuous(limits = c(-55, 100)) +  
    #     #guides(fill = guide_colorbar(barwidth = 10, barheight = .5))
  pdf(file = "../../images/pdf-images/typology-world-map.pdf",width=15, height=7)
  print(m + coord_map(xlim = c(-85, 145),ylim = c(-35, 50))) #print required to generate pdf within function
  dev.off()
}

plotTypologyWorldMap(df.clusters)


############################################################################
### Violin plot of factor scores by typology

df.typologies <- df.clusters
df.typologies$Typology <- df.typologies$Cluster
df.typologies <- df.typologies%>%
  mutate(Typology=case_when(
    .$Typology=="1" ~ "Decentralized",
    .$Typology=="2" ~ "Hybrid",
    .$Typology=="3" ~ "Centralized",
  ))

write_xlsx(df.typologies, "../../results/df-typologies.xlsx")
df.typology.scores <- merge(x = df.typologies, y = df.scores, by = c("Country"))

# Convert the variable dose from a numeric to a factor variable
melted.df.typology.scores <- melt(df.typology.scores, id="Typology", 
                 measure=sort(c(    
                   'Far Well',
                   'Nearby Improved',
                   'Nearby Surface',
                   'Vended' ,
                   'Piped Indoors' ,
                   'Piped Outdoors' ,
                   'Far Spring' )), 
                 variable.name="Factor", value.name="Weight")
#Sizing
par(mar = c(1,1,1,1))
#par(cex=5)

# Basic violin plot for all seven factors
p <- ggplot(data = melted.df.typology.scores, aes(x=Factor, y=Weight, fill=Typology)) + 
  geom_violin(trim=FALSE) +
  theme_minimal() + 
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", color="black") +
  labs(title="",x="", y = "Factor Score") +
  scale_fill_manual(
    name = 'Typology', 
    #labels = c("Centralized" , "Hybrid", "Decentralized"),
    values=c('#8da0cb','#66c2a5','#fc8d62')) +
  facet_wrap(~ factor(Typology,levels=c("Centralized" , "Hybrid", "Decentralized")) , nrow=1) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(strip.text=element_text(size=20),legend.text=element_text(size=20),
        axis.text = element_text(size=20), text = element_text(size=20),
        legend.position = "none")

pdf(file = "../../images/pdf-images/typology-violinplot-factors.pdf"
    ,
    width     = 15,
    height    = 10 )

p 
dev.off()


##################################################
### Boxplot of water accessibility indicators by typology

df.wai <- read_excel("../../data/tidy/df-water-accessibility-indicators.xlsx",sheet=1)
df.wai['Typology'] <- df.typologies$Typology
df.wai <- df.wai %>% 
  rename(
    'Piped (Dwelling)' = phom ,
    'Piped (Yard)' = pipy,
    'Public Tap' = ptap,
    'Borehole'=bore,
    'Protected Well'=pwel,
    'Protected Spring' =pspr,
    'Rain'= rain,
    'Unprotected Well'=uwel,
    'Unprotected Spring'=uspr,
    'Truck' =truc,
    'Tanker Cart'=ctan,
    'Bottled'=bott,
    'Other' =othw,
    'Surface' =surw,
    'T<30min'=tles,
    'T>30min' =tmor,
    'On-Premises' =watp,
  )

head(df.wai)

metled.df.wai <- melt(df.wai, id="Typology", measure=sort(c(
                'Piped (Dwelling)', 'Piped (Yard)','Public Tap','Borehole','Protected Well','Protected Spring','Rain','Unprotected Well','Unprotected Spring'
                ,'Truck','Tanker Cart','Bottled','Other','Surface','T<30min','T>30min','On-Premises', 
                decreasing=TRUE)), 
              variable.name="Indicator", value.name="Value")
#Sizing
par(mar = c(1,1,1,1))
#par(cex=5)

# Box plot of the 17 water accesisibility variables
b<-ggplot(metled.df.wai, aes(x=Indicator, y= Value, fill = Typology)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(name = 'Typology', 
                    values=c( '#8da0cb','#66c2a5','#fc8d62')) +
  facet_wrap(~ factor(Typology,levels=c("Centralized" , "Hybrid", "Decentralized")) , nrow=1) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  theme(legend.position="none") +
  theme(text = element_text(size=20)) + #Font size
  theme(legend.title = element_blank(), legend.position = "none") + #No need of legend 
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        strip.text=element_text(size=20)) +
  labs(title="",x="", y = "% Households", element_text(size=17)) 


pdf(file = "../../images/pdf-images/typology-boxplot-water-accessibility-indicators.pdf"
    ,
    width     = 14,
    height    = 10 )

par(mar = c(1, 1, 1, 20), cex.axis = 1, cex.lab = .7) 

b

dev.off()
#########################################################################

## TO FINISH
### Ranking
df.wa <- read_excel("../../results/df-seven-scores.xlsx",sheet=1)
# clean df at next save (2 cluster columns - only 1 needed; etc) #df also needs to be only water accessibility variable
df.wa["norm"] <- apply(df.wa[2:8], 1, function(x) sqrt(sum(x^2)) ) #assuming columns 5:10 are the water accessibility variables
# Ideally, df should just be the countries and the water access. variables ONLY. This, way, the code should then be:
# df["norm"] <- apply(df, 1, function(x) sqrt(sum(x^2)) )          
df.wa["distToCentroid"] = 0 # initialize           
df.wa["clusters"] <- as.factor(dfsimple$clusters)
#Turn into numeric to find centroids             
# assuming centroiddf.wa is your vector of cluster centroids
# copied this from your earlier notebook
df.wa.aggregate <- aggregate(df.wa[,c(2:8)], list(clusters=dfsimple$clusters), mean)
df.wa.aggregate <- df.wa.aggregate[,c(1:7)]
centroiddf.wa <- apply(df.wa.aggregate, 1, function(x) sqrt(sum(x^2)) ) #How close is each country to the cen-troid of the cluster                        
df <- df.wa            
# Then here, you compute the distance to centroid cluster by cluster
df[df$clusters == 1, "distToCentroid"] = abs(df[df$clusters == 1, "norm"] - centroiddf.wa[1])
df[df$clusters == 2, "distToCentroid"] = abs(df[df$clusters == 2, "norm"] - centroiddf.wa[2])
df[df$clusters == 3, "distToCentroid"] = abs(df[df$clusters == 3, "norm"] - centroiddf.wa[3])
#df[df$clusters == 4, "distToCentroid"] = abs(df[df$clusters == 4, "norm"] - centroiddf.wa[4])
#df[df$clusters == 5, "distToCentroid"] = abs(df[df$clusters == 5, "norm"] - centroiddf.wa[5])
#df[df$clusters == 6, "distToCentroid"] = abs(df[df$clusters == 6, "norm"] - centroiddf.wa[6])                       
write_xlsx(df , '../../results/df-fa-seven-cluster-rank.xlsx')

# You can then sort each subset by the size of "distToCentroid"
df1 = df %>% 
  arrange_at("Country", desc) %>%
  arrange_at("distToCentroid") %>%
  arrange_at("clusters") %>%
  select(1, 10, 11)
cluster.one <- subset(df1, clusters == 1 )
cluster.two <- subset(df1, clusters == 2 )
cluster.three <- subset(df1, clusters == 3 )
cluster.four <- subset(df1, clusters == 4 )
cluster.five <- subset(df1, clusters == 5 )
#cluster.six <- subset(df1, clusters == 6 )
summary(cluster.one)            
cluster.one %>% 
  arrange_at("Country", desc) %>%
  arrange_at("distToCentroid") %>%
  arrange_at("clusters") %>%
  select(1:3)


summary(cluster.two)                         
cluster.two %>% 
  arrange_at("Country", desc) %>%
  arrange_at("distToCentroid") %>%
  arrange_at("clusters") %>%
  select(1:3)


summary(cluster.three) 
cluster.three %>% 
  arrange_at("Country", desc) %>%
  arrange_at("distToCentroid") %>%
  arrange_at("clusters") %>%
  select(1:3)


# cluster.four %>% 
#   arrange_at("Country", desc) %>%
#    arrange_at("distToCentroid") %>%
#    arrange_at("clusters") %>%
#  select(1:3)
#cluster.five %>% 
# arrange_at("Country", desc) %>%
#  arrange_at("distToCentroid") %>%
# arrange_at("clusters") %>%
# select(1:3)
#cluster.six %>% 
#arrange_at("Country", desc) %>%
# arrange_at("distToCentroid") %>%
# arrange_at("clusters") %>%
# select(1:3)