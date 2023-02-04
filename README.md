# water-accessibility
This repository contains the data and code used to conduct the analyses and generate the results published in the article, [Drinking water accessibility typologies in low- and middle-income countries](https://doi.org/10.1088/1748-9326/acb662).

Code-Map
- `preprocessing.R`: combine datasets from Demographic and Health Surveys (DHS) and World Bank

- `exploratory-plots.R`: generate correlation plots and histograms of water accessibility indicators and explanatory variables

- `factor-analysis.R`: conduct exploratory factor analysis based on 17 water accessibility indicators

- `clustering.R`: cluster the countries using the extracted factors

- `decision-tree.R`: estimate a decision tree to predict/explain typologies based on explanatory variables.
