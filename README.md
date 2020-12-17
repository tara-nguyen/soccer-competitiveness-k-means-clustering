# Analyzing the Competitive Balance of Different Soccer Leagues

- AUTHOR: [**TARA NGUYEN**](https://www.linkedin.com/in/nguyenthuyanh/)
- Project for the course *Exploratory Data Analysis and Visualization* at UCLA Extension
- Completed in December 2020

## Abstract

**Background**: *Competitive balance*, which refers to the degree of uncertainty regarding the outcome of a competition, is frequently debated among soccer fans and has received considerable attention both [in](../tree/main/References) and [outside](https://www.fearthewall.com/2019/7/8/20685467/the-state-of-the-league-comparing-the-bundesliga-with-its-competitors) academia.

**Data and research question**: In this project I analyzed team performances (points per game, win proportions, etc.) in the four soccer leagues from the 2015/2016 season to the 2019/2020 season. The main research question was: **_Which soccer league is the most competitive?_**

**Method and findings**: The entire project was done in R. Through exploratory data analysis and k-means clustering, I found that, in general, the Major League Soccer was the more competitive than the Bundesliga, the La Liga, and the Premier League.

For a complete report, see the [wiki page](https://github.com/tara-nguyen/soccer-competitiveness-k-means-clustering/wiki)

## List of files and directory in the repo

[`Plots`](Plots) - directory for plots created during data visualization

[`References`](References) - directory for academic articles on competitive balance

`README.md` - this document you are currently reading

[`all-form-leaguetables.csv`](all-form-leaguetables.csv) - final data set

[`all-leaguetables.xlsx`](all-leaguetables.xlsx) - season-end league tables in all 5 seasons of all 4 leagues

[`form-bundesliga.csv`](form-bundesliga.csv), [`form-epl.csv`](form-epl.csv), [`form-laliga.csv`](form-laliga.csv), [`form-mls.csv`](form-mls.csv) - form tables in all 5 seasons of the Bundesliga, the EPL, the La Liga, and the MLS, respectively

[`leaguescompetitiveness_Analysis.R`](leaguescompetitiveness_Analysis.R) - main R script for data wrangling, visualization, and statistical analyses

[`leaguesfinaldat_DataWrangling.R`](leaguesfinaldat_DataWrangling.R) - R script for creating the final data set

## Usage Note

The dataset and R scripts are free for download and use, **provided that proper credit is given**.

If you mention or use any part of my research report, please provide a link to this repo.
