# water-accessibility
This project will focus on quantifying water accessibility and understanding the relevant factors. It will also involve an analysis of the transportation modal splits for water access across various countries. Data sources will include the Demographic and Health Surveys (DHS) and the Malaria Indicator Cluster Surveys (MIS). It will initially be piloted on a few countries in which prior relevant work has been conducted, such as Peru. Data will be integrated and machine learning models will be used to investigate patterns and explain water accessibility and serve as a tool for policymakers and planners.

Code-Map
- `preprocessing.R`: combine datasets from Demographic and Health Surveys (DHS) and World Bank

- `exploratory-plots.R`: generate correlation plots and histograms of water accessibility indicators and explanatory variables

- `factor-analysis.R`: conduct exploratory factor analysis based on 17 water accessibility indicators

- `clustering.R`: cluster the countries using the extracted factors

- `decision-tree.R`: estimate a decision tree to predict/explain typologies based on explanatory variables.
