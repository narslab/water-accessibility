library(psych)
library(readxl)
library(GPArotation)

df.wai <- read_excel("../../data/tidy/df-water-accessibility-indicators.xlsx",sheet=1)
#df.wai[is.na(df.wai)] <- 0
df.wai.nocountry <- df.wai[,c(2:18)]
head(df.wai)

## SEARCH FOR OPTIMAL EXTRACTION METHOD AND NUMBER OF FACTORS
#For loop for factors of 4-7 fm = "minchi"
for (i in 4:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "promax", fm = "minchi" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  #print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)
  print(iteration$BIC)
  print(iteration$TLI)
  print(iteration$rms)
  print(iteration$STATISTIC)
  print(iteration$PVAL)
  print(iteration$objective)
  print(iteration$EBIC )
  print(iteration$dof)
  print(iteration$chi)
  print(iteration$RMSEA)
} 

#For loop for factors of 4-7 fm = "minrank" 
for (i in 4:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "promax", fm = "minrank" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$BIC)
  print(iteration$TLI)
  print(iteration$rms)
  print(iteration$STATISTIC)
  print(iteration$PVAL)
  print(iteration$objective)
  print(iteration$EBIC )
  print(iteration$dof)
  print(iteration$chi)
  print(iteration$RMSEA)
} 

#For loop for factors of 4-7 fm = "minrank" 
for (i in 4:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "promax", fm = "minrank" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}

#For loop for factors of 4-7 fm = "ml"
for (i in 4:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "promax", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$BIC) 
  print(iteration$TLI)
  print(iteration$rms)
  print(iteration$STATISTIC)
  print(iteration$PVAL)
  print(iteration$objective)
  print(iteration$EBIC )
  print(iteration$dof)
  print(iteration$chi)
  print(iteration$RMSEA)
} 

# possible oblique transformations of the solution for better interpreation
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "oblimin", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "bentlerQ", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 2000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "promax", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}
#  are orthogonal rotations for stronger interpreation
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "varimax", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "bentlerT", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}
for (i in 2:7) {
  iteration <- fa(df.wai.nocountry, nfactors = i, rotate = "geominT", fm = "ml" , scores= "tenBerge", impute ="mean", max.iter = 1000)
  print(iteration$loadings, digits=2, cutoff=.2, sort=TRUE)}

##############################################################################
#Best method: ml

# # Exploring 5 scores
# solution.five <- fa(df.wai.nocountry, nfactors = 5, rotate = "promax", fm = "ml", scores="tenBerge", impute ="mean", max.iter = 1000 )
# scores.five.factors = solution.five$scores
# scores.five.factors = as.data.frame.matrix(scores.five.factors) #df.wa multiply by scores matrix
# write_xlsx(scores.five.factors, '../../results/scores-5-factors.xlsx')
# print(solution$loadings, digits=2, cutoff=.2, sort=TRUE) #sort out by the low numbers
# df.wai.nocountry <- df.wai[,c(2:18)]
# df.wai.nocountry <- as.matrix(df.wai.nocountry)
# scores <- as.matrix(scores.five.factors)
# df.five.scores <-  scores
# df.five.scores = as.data.frame(apply(as.matrix(df.five.scores[,c(1:5)]), 2, rescale))
# df.five.scores <- cbind(df.wa,df.five.scores)
# df.five.scores <- df.five.scores[,c(1,19:23)]
# head(df.five.scores,7)
# summary(df.five.scores)
# write_xlsx(df.five.scores, '../../archive-explore/df-five-scores.xlsx')
# 
# 
# # Exploring 6 scores
# solution.six <- fa(df.wai.nocountry, nfactors = 6, rotate = "promax", fm = "ml", scores="tenBerge", impute ="mean", max.iter = 1000 )
# scores.six.factors = solution.six$scores
# scores.six.factors = as.data.frame.matrix(scores.six.factors) #df.wa multiply by scores matrix
# write_xlsx(scores.six.factors, '../../results/scores-6-factors.xlsx')
# print(solution.six$loadings, digits=2, cutoff=.2, sort=TRUE) #sort out by the low numbers
# df.wai.nocountry <- df.wai[,c(2:18)]
# df.wai.nocountry <- as.matrix(df.wai.nocountry)
# scores <- as.matrix(scores.six.factors)
# df.six.scores <-  scores
# df.six.scores = as.data.frame(apply(as.matrix(df.six.scores[,c(1:6)]), 2, rescale))
# df.six.scores <- cbind(df.wa,df.six.scores)
# df.six.scores <- df.six.scores[,c(1,19:24)]
# head(df.six.scores,7)
# summary(df.six.scores)
# write_xlsx(df.six.scores, '../../archive-explore/df-six-scores.xlsx')


## SELECTED SOLUTION - 7 factors
solution <- fa(df.wai.nocountry, nfactors = 7, rotate = "promax", fm = "ml", missing=TRUE, scores="tenBerge", impute ="mean", max.iter = 1000 )
scores.seven.factors = solution$scores
scores.seven.factors = as.data.frame.matrix(scores.seven.factors) #df.wa multiply by scores matrix
write_xlsx(scores.seven.factors, '../../results/scores-7-factors.xlsx') #unscaled, no countries
print(solution$loadings, digits=2, cutoff=.2, sort=TRUE) #sort out by the low numbers


df.wai.nocountry <- as.matrix(df.wai.nocountry)
scores <- as.matrix(scores.seven.factors)
df.seven.scores <-  scores
df.seven.scores = as.data.frame(apply(as.matrix(df.seven.scores[,c(1:7)]), 2, rescale))
df.seven.scores <- cbind(df.wai,df.seven.scores)
df.seven.scores <- df.seven.scores[,c(1,19:25)]
head(df.seven.scores,7)
summary(df.seven.scores)
write_xlsx(df.seven.scores, '../../results/df-seven-scores.xlsx')
# write_xlsx(score.no.scale, '../../results/seven-scores-no-scale.xlsx') ## if unscaled



## SCREE PLOT
solution10 <- fa(df.wai.nocountry, nfactors = 10, rotate = "promax", fm = "ml", scores="tenBerge", impute ="mean", max.iter = 1000 )

scree     <- data.frame(
  Factor_n =  as.factor(1:10), 
  Eigenvalue = solution10$e.values[1:10])

pdf(file = "../../images/pdf-images/scree-plot.pdf",
    width     = 5,
    height    = 5 )

ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 7 , linetype = "dashed", color='red',  linewidth=1) + 
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18)) +
  xlab("Number of factors") +
  ylab("Eigenvalue") 

dev.off()

## FACTOR LOADINGS
df.fl <- read_excel("../../results/factor-7-loadings.xlsx",sheet=1) #customized excel

df.fl <- as.data.frame(df.fl)

df.fl[,c(1)] <- as.factor(df.fl[,c(1)])
head(df.fl)

loadings.m <- melt(df.fl, id="Variable", # if the name is changed, then change the name in the excel as well
                   measure=c(    
                     'Far Spring',
                     'Far Well',
                     'Nearby Improved',
                     'Nearby Surface',
                     'Piped Indoors' ,
                     'Piped Outdoors' ,
                     'Vended'
                   ), 
                   variable.name="Factor", value.name="Loading")

#options(repr.plot.width=15, repr.plot.height=7)
options(repr.plot.width=16, repr.plot.height=8)
par(mar = c(1,1,1,1))
par(cex=1)

g <- ggplot(data = loadings.m, aes(x = Variable, y = abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1,  strip.position="top") +
  geom_bar(stat="identity" , aes(), color='gray') + 
  coord_flip() + #flip the axes so the test names can be horizontal  
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  xlab("Water accessibility indicator") + 
  ylab("Loading Strength") + 
  theme_bw(base_size= 20) +  
  scale_x_discrete(limits = rev) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size=16))

g + theme(legend.position="bottom")

png(
  "../../images/png-images/fa-7-loadings.png",
  width     = 14,
  height    = 5,
  units     = "in",
  res       = 700,
  #pointsize = 6 
)
g + theme(legend.position="bottom")
dev.off()

pdf(file = "../../images/pdf-images/fa-7-loadings.pdf"
    ,
    width     = 16,
    height    = 8 
)
g + theme(legend.position="bottom")
dev.off()