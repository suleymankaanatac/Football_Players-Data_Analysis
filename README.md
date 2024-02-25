# Football Players Data Analysis using R
Statistical analyses and visualizations related to football players traits.

## 1-Data Preprocessing

- Reading and cleaning the dataset, ensuring there are no missing values.
- Calculating additional variables like BMI and adjusting values for analysis.

## 2-Data Analyses and Normality Test
- Calculating summary statistics and creating histograms for weight, height, BMI, and market value of football players.
- Using some normality tests to see distributions.
- Analysing other traits and their relations with values of football players.

## 3-Point Estimations and Confidence Intervals

- For point estimations for whole data, sampled data is used.
- 3 different confidence intervals constructed to estimate what values can be our potential values for mean, variance and standard deviation.

- ## 4-Linear Regression

- To understand the relation between the overall_stats of a player and their values I created a scatter plot.
- In order to create a linear regression from this graph, I took the logarithm of the values in the value feature.
  

- ## 5-Goodness of Fit Test

A statistical test called the goodness-of-fit test, sometimes referred to as the chi-square test of goodness-of-fit, is used to examine whether an observed categorical dataset fits an anticipated frequency distribution or theoretical distribution. It aids in determining whether there is a  significant  difference between the observed and predicted data in light of a given hypothesis.
- Performing a chi-square goodness-of-fit test to compare the distribution of work rates in the sample to the population.

- ## 6-ANOVA

- Conducting an ANOVA test to check if there is a significant difference in market values among players from different countries.


- ## 7-Nonparametric Tests

- Performing sign tests and Wilcoxon signed-rank test for median and paired-sample differences.
- Conducting a Spearman rank correlation test for correlation between overall stats and market values.
