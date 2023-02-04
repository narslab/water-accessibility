library(readxl)
library(writexl)
library(dplyr)


df.dhs <- read_excel("../../data/tidy/DHS-Data-Dictionary.xlsx",sheet=1) #DHS
df.se = read_excel("../../data/raw/Data_Extract_From_World_Development_Indicators.xlsx",sheet=1, n_max=218) # world bank socioeconomic explanatory variables (gdp, pop, etc)
df.rain = read_excel("../../data/raw/Data_Extract_From_World_Development_Indicators-Average precipitation in depth (mm per year).xlsx",sheet=1,range="C1:E218")
df.co2 = read_excel("../../data/raw/Data_Extract_From_World_Development_Indicators-CO2-2014.xlsx",sheet=1, n_max=218) # carbon dioxide emissions
df.fw = read_excel("../../data/raw/Renewable_internal_freshwater_resources_per_capita_2017.xlsx",sheet=1, n_max=218) # renewable internal freshwater resources
df.wb.mort.gas = read_excel("../../data/raw/Data_Extract_From_WDI.xlsx",sheet=1,n_max=218) # mortality rate, gas prices; year 2016

# merge dataframes
df.exp <- merge(df.fw, df.co2, by="Country Name", all.x=T)
df.exp <- merge(df.exp, df.wb.mort.gas, by="Country Name", all.x=T)
df.exp <- df.exp[,c(1:3,8,10,12)]
df.exp <- merge(df.exp,df.rain, by="Country Name", all.x=T)
df.exp <- df.exp[,c(1:6,8)]

#rename variable titles for succinctness
df.exp <- df.exp %>% 
  rename(
    Country = "Country Name",
    prec = "2014 [YR2014]",
    rifr = "2017 [YR2017] - Renewable internal freshwater resources per capita (cubic meters) [ER.H2O.INTR.PC]",
    cotw = "CO2 emissions from transport (% of total fuel combustion) [EN.CO2.TRAN.ZS] - 2014 [YR2014]",
    moru = "Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population) [SH.STA.WASH.P5] - 2016 [YR2016]",
    ppfg = "Pump price for gasoline (US$ per liter) [EP.PMP.SGAS.CD] - 2016 [YR2016]"
  )

# count
count(df.exp)
count(df.se)

#Investiagate NAs
#df.exp$cotw <- as.numeric(df.exp$cotw)
#df.exp[is.na(df.exp)] <- 0
#df.exp[,c(3:7)][is.na(df.exp[,c(3:7)])] <- 0
df.exp[, c(3:7)] <- sapply(df.exp[, c(3:7)], as.numeric)
#df.exp[,3][is.na(df.exp[,3])] <- 0
head(df.exp,10)

#Renaming the column variables for the world bank data to be consistent with the other explanatory variables
df.se <- df.se %>% 
  rename(
    Year = Time,
    Country = "Country Name",
    cgdp = "GDP (current US$) [NY.GDP.MKTP.CD]",
    tpop = "Population, total [SP.POP.TOTL]",
    upop = "Urban population (% of total population) [SP.URB.TOTL.IN.ZS]",
    popd = "Population density (people per sq. km of land area) [EN.POP.DNST]",
    land ="Land area (sq. km) [AG.LND.TOTL.K2]", 
    lita = "Literacy rate, adult total (% of people ages 15 and above) [SE.ADT.LITR.ZS]",  
    lity = "Literacy rate, youth (ages 15-24), gender parity index (GPI) [SE.ADT.1524.LT.FM.ZS]"  ,
    mori = "Mortality rate, under-5 (per 1,000 live births) [SH.DYN.MORT]" 
  )

# turn character into numbers and numeric
df.se[, c(1, 5:12)] <- sapply(df.se[, c(1, 5:12)], as.numeric)


## DHS data
#Cleaning Column Years
for (i in seq(1, nrow(df.dhs))) {
  if (grepl("-", df.dhs[i,'Year'])) {
    yy1 = substr(df.dhs[i,"Year"], start=1,stop=2)
    yy2 = substr(df.dhs[i,"Year"], start=6,stop=7)
    yy3 = paste(yy1,yy2,sep = "")
    #yy3 = as.integer(yy3)
    df.dhs[i,"Year"] = yy3
  }
}

# Select obs corresponding to the Max Years
df.dhs$Year = as.numeric(df.dhs$Year)
df.dhs.max <- matrix(NA, nrow = 0, ncol = length(colnames(df.dhs))) #create new empty DF
colnames(df.dhs.max) = colnames(df.dhs) # assign column names to empty data frame
for (country in unique(df.dhs$Country)) {
  maxyear = max(df.dhs[df.dhs$Country==country,'Year'])
  df.dhs.max = rbind( df.dhs.max, df.dhs[(df.dhs$Country==country) & (df.dhs$Year==maxyear),])
} 

#Removing Population Variables (only want to keep household-based indicators)
df.dhs.max.hh = df.dhs.max[,!grepl("^P_",names(df.dhs.max))]

#NA and zeros Analysis
na_count = colSums(is.na(df.dhs.max.hh)) #Counting all your NA in each data frames
na_count

na.count.se = colSums(is.na(df.se))
na.count.se

# Removing column for household data frame if NA is greater than 50%`
df.dhs.clean = df.dhs.max.hh[, which(colMeans(!is.na(df.dhs.max.hh)) > 0.5)]
na_count_cleaned = colSums(is.na(df.dhs.clean)) #Counting all your NA in each data frames
na_count_cleaned
hist(na_count_cleaned)
zeros = colSums(df.dhs.clean != 0) # Counting all your zeros in each data frame


# remove observations from years earlier than 2000
head(df.dhs.clean$Year , 7)
sum(df.dhs.clean$Year < 2000)
df.dhs.clean[df.dhs.clean$Year < 2000, ]
df.dhs.clean = filter(df.dhs.clean, Year >=2000 & Year < 2020)
count(df.dhs.clean)

# Clean DHS data
write_xlsx(df.dhs.clean, '../../data/tidy/df-dhs.xlsx') ## formerly dfsimple

# Clean DHS water accessibility indicator data
wai <- c("phom", "pipy", "ptap", "bore", "pwel", "pspr", "rain", "uwel", "uspr", 
        "truc", "ctan", "bott", "othw", "surw", "tles", "tmor", "watp")
df.wai <- df.dhs.clean[, c('Country', wai)] # select only water accessibility indicators (wai)
df.wai[is.na(df.wai)] <- 0 #zero the NAs for factor analysis
write_xlsx(df.wai, '../../data/tidy/df-water-accessibility-indicators.xlsx')

pdf(file = "../../images/pdf-images/survey-years.pdf",
    width     = 10,
    height    = 10 )

hist(df.dhs.clean$Year, main="",
     xlab="Years",
     ylab="Counts",
)#col.main="red", col.lab="blue")

dev.off()


## Processing explanatory variables
# remove Year and `Time Code` columns from world bank df
df.se <- df.se[,c(3:12)]

setdiff(df.se$Country, df.exp$Country)
df.wb <- merge(x = df.se, y = df.exp, by = c("Country"))
df.wb <- subset(df.wb, select = -c(`Country Code`,`Country Code.x`))
head(df.wb)

# Correct the country names in DF
df.wb [df.wb =='namibia'] = "Namibia"
df.wb [df.wb =='Yemen, Rep.'] = "Yemen" 
df.wb [df.wb =='Gambia, The'] = "Gambia"
df.wb [df.wb =='Egypt, Arab Rep.'] = "Egypt"
df.wb [df.wb =='Congo, Dem. Rep.'] = "Congo Democratic Republic"
df.wb [df.wb =='Congo, Rep.'] = "Congo"



write_xlsx(df.wb, '../../data/tidy/df-wb-combined.xlsx') #formerly df-wb.xlsx


## Final explanatory variables for use in decision tree
mobi.indicators <- c('bicy', 'cart', 'mcyc', 'pcar', 'boat', 'wigc')
setdiff(df.dhs.clean$Country, df.wb$Country) # Double Check

df.expfinal <- merge(x = df.dhs.clean[, c('Country',mobi.indicators)],
               y = df.wb,
               by = c("Country"))

df.expfinal$gdpp <- df.expfinal$cgdp/df.expfinal$tpop  ## GDP per capita
df.expfinal <- subset(df.expfinal, select = -c(cgdp, lita, lity, mori, cotw, moru, ppfg))


df.expfinal <- df.expfinal[, c('Country',sort(colnames(df.expfinal[2:14])))]

# Fix missing Eritrea data:
# Year: 2011; Source: WB via https://datacommons.org/tools/timeline#place=country%2FERI&statsVar=Amount_EconomicActivity_GrossDomesticProduction_Nominal_PerCapita__Count_Person__Count_Person_Urban
eritrea.land.area = df.expfinal[df.expfinal$Country=='Eritrea','land']
df.expfinal[df.expfinal$Country=='Eritrea',c("gdpp", "popd", "tpop", "upop")] =
  c(643, 3210000/eritrea.land.area, 3210000, (1.15/3.21)*100)
head(df.expfinal)
write_xlsx(df.expfinal, '../../data/tidy/df-explanatory-variables.xlsx')


            
            