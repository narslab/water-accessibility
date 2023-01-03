library(readxl)
library(GGally)
library(tidyr)

df.wai = read_excel( "../../data/tidy/df-water-accessibility-indicators.xlsx" ,sheet=1)

df.exp <- read_excel("../../data/tidy/df-explanatory-variables.xlsx")

df.exp <- df.exp %>% 
  rename(
    'HH Bicycle (%)' = bicy ,
    'HH A Cart (%)' = cart,
    'HH M-cycle (%)' = mcyc,
    'HH Car (%)'=pcar,
    'HH Boat (%)'=boat,
    'Gini' =wigc,
    'GDP p.c. ($)'= gdpp,
    'Pop'=tpop,
    'Urban Pop (%)'=upop,
    'Pop Den (/sq. km)' =popd,
    'Land Area (sq. km)'=land,
    'RIFW (p.c. cu. m)'=rifr,
    'Prec (mm/yr)' =prec
  )
df.exp = df.exp[, c(1, 1+order(names(df.exp)[2:14]))]

#options(repr.plot.width=14, repr.plot.height=14)
par(mar = c(1,1,1,4), cex.axis = 1, cex.lab = .8) 

pdf(file = "../../images/pdf-images/matrix-plot-explanatory-variables.pdf"
    ,
    width     = 10,
    height    = 10 )

ggpairs(df.exp, columns = 2:14, 
        diag = list(continuous = "barDiag", binwidth=0.5), 
        lower = list(continuous = wrap("points", alpha = 0.7, size=0.4)),
        labeller = label_wrap_gen(10)) +
  theme(axis.text.x = element_text(size = 8, angle = 90,hjust=0.95,vjust=0.2), axis.text.y = element_text(size = 8)) + 
  theme(strip.text.x = element_text(size = 10, angle = 45) , strip.text.y = element_text(size = 10, angle = 45))
        
dev.off()

## correlation heat map
ggcorr(df.exp[,c(2:14)])

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



# ggpairs(df.wai, columns = 2:18)

countries_gathered <- df.wai %>% gather(Measure, Weight, 'Piped (Dwelling)':'On-Premises') 

## Histograms - same scale
pdf(file = "../../images/pdf-images/water-accessibility-histograms.pdf"
    ,
    width     = 11,
    height    = 10 ) 

ggplot(countries_gathered, aes(Weight)) +
  theme_bw() +
  geom_histogram() +
  xlab("Percentage of households") +
  ylab("Counts") +
  facet_wrap(~Measure, nrow=4) + #, scales="free")  + 
  theme(text = element_text(size=20) ) + #Font size
  theme(axis.text = element_text(size = 17)) + #All tick size  
  theme(axis.title = element_text(size = 20))  # Adjusting Axis Title   

dev.off()

## Histograms - different scales
pdf(file = "../../images/pdf-images/water-accessibility-histograms-diff-scales.pdf"
    ,
    width     = 11,
    height    = 10 ) 

ggplot(countries_gathered, aes(Weight)) +
  theme_bw() +
  geom_histogram() +
  xlab("Percentage of households") +
  ylab("Counts") +
  facet_wrap(~Measure, nrow=4, scales="free")  + 
  theme(text = element_text(size=18) ) + #Font size
  theme(axis.text = element_text(size = 17)) + #All tick size  
  theme(axis.title = element_text(size = 20))  # Adjusting Axis Title   

dev.off()

