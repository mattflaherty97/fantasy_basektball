**IN PROGRESS**

# Introduction

I created this repository because I have relied heavily on "experts" year in and year out. This year, I thought about doing my own analysis using data from 2018-2020. Thus, I can make my own interpretations on what features are key to finding players that will perform well for my fantasy team and my motivating question revolves around this. What features impact a player's fantasy performance the most? Having found key features, I should be able to develop a well-rounded draft that is data driven and potentially choose a league-winning team. In this repository, I want to create my own data set, visualize features, and predict the features with highest importance for fantasy points.

# [Feature Engineering](https://github.com/mattflaherty97/fantasy_basketball/blob/main/00_feature_engineering/import_data.Rmd)

This data was collected from [Basketball Reference](https://www.basketball-reference.com/). I use data from 2018-2020 by copying and pasting the data into a .csv file. In order to get the most accurate results, I use player data per 100 possessions as well as advanced statistics from the website. I use data per 100 possessions because games will have varying paces of play. Thus, per 100 possessions will put all players onto the same scale. For example, the Golden State Warriors have been known to play at the fastest pace so their players per game statistics will be inflated as compared to the Memphis Grizzlies who have been known for their slow pace of play. Therefore, looking at player statistics for both teams on a per 100 possessions basis will negate any disparity in the number of possession either team has during the course of a game. I also use advanced statistics because these statistics can determine how a player impacts his team. For example, box plus/minus (BPM) will tell the estimate of the points per 100 possessions a player contributed above a league-average player, translated to an average team. I believe that this will have an impact on fantasy points because the better players in the league usually have higher a BPM.

# [Feature Selection](https://github.com/mattflaherty97/fantasy_basketball/blob/main/10_exploration/feature_selection.md)

There are about 50 features in this data set after the feature engineering. I want to reduce the dimensions so that I can see which features are similar to `fpts_100`. Therefore, I use Principal Component Analysis (PCA) to achieve this. I reduce the dimension down to four dimensions and find the dimension that contains `fpts_100`.

![Caption: Dimension 1](https://github.com/mattflaherty97/fantasy_basketball/blob/main/10_exploration/feature_selection_files/figure-gfm/component_contribution-1.png)

The variables closest to `fpts_100` are player efficiency rating (PER) and box plus-minus (BPM). Thus, I believe that these variables can also be used to pick players as they are both highly correlated with `fpts_100`.

![Caption: Fantasy Points vs PER](https://github.com/mattflaherty97/fantasy_basketball/blob/main/10_exploration/feature_selection_files/figure-gfm/per_fpts-1.png)

![Caption: Fantasy Points vs BPM](https://github.com/mattflaherty97/fantasy_basketball/blob/main/10_exploration/feature_selection_files/figure-gfm/bpm_fpts-1.png)

# Cluster Analysis

Having found dimensions that are similar to each other, I can perform a cluster analysis to split the players into tiers. This will be beneficial to managers because they might be able to trade an overhyped player in a low tier for an underrated player in higher tier.

# Conclusion

From this brief analysis, fantasy managers should consider drafting players that are more efficient over players with high usage because the more efficient a player is, then the more fantasy points he ought to have.
