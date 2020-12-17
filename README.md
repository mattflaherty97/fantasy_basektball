**IN PROGRESS**

# Introduction

I created this repository because I have relied heavily on "experts" year in and year out. This year, I thought about doing my own analysis using data from 2018-2020. Thus, I can make my own interpretations on what features are key to finding players that will perform well for my fantasy team and my motivating question revolves around this. What features impact a player's fantasy performance the most? Having found key features, I should be able to develop a well-rounded draft that is data driven and potentially choose a league-winning team. In this repository, I want to create my own data set, visualize features, and predict the features with highest importance for fantasy points.

# Feature Engineering

*Describe how I got the data set*

# [Feature Selection](https://github.com/mattflaherty97/fantasy_basketball/blob/main/feature_selection.md)

There are about 50 features in this data set after the feature engineering. I want to reduce the dimensions so that I can see which features are similar to `fpts_100`. Therefore, I use Principal Component Analysis (PCA) to achieve this. I reduce the dimension down to four dimensions and find the dimension that contains `fpts_100`.

![Caption: Dimension 1](https://github.com/mattflaherty97/fantasy_basketball/blob/main/feature_selection_files/figure-gfm/component_contribution-1.png)

The variables closest to `fpts_100` are player efficiency rating (PER) and box plus-minus (BPM). Thus, I believe that these variables can also be used to pick players as they are both highly correlated with `fpts_100`.

![Caption: Fantasy Points vs PER](https://github.com/mattflaherty97/fantasy_basketball/blob/main/feature_selection_files/figure-gfm/per_fpts-1.png)

![Caption: Fantasy Points vs BPM](https://github.com/mattflaherty97/fantasy_basketball/blob/main/feature_selection_files/figure-gfm/bpm_fpts-1.png)

# Conclusion

*which variables can fantasy managers use to draft players?*
