## Analyzing the competitiveness of 4 soccer leagues over 5 seasons
##
## AUTHOR: TARA NGUYEN
## Part of group project for UCLA Extension course: 
## Exploratory Data Analysis and Visualization
## Completed in December 2020

########## DATA IMPORT AND CLEANING ##########

## data for for 5 seasons (up to season 2019/20) of 4 soccer leagues:
## - the English Premier League,
## - the La Liga,
## - the Bundesliga, and
## - the Major League Soccer

library(readr)
alldat <- read_csv('all-form-leaguetables.csv')
alldat
unique(alldat$season)

########## DATA TRANSFORMATION ##########

## add seasonstart column to denote the year a season started

alldat$seasonstart <- as.numeric(substr(alldat$season, 1, 4))

## turn league and seasonstart into factors

alldat$league <- as.factor(alldat$league)
alldat$seasonstart <- as.factor(alldat$seasonstart)

leagues <- levels(alldat$league)
seasons <- levels(alldat$seasonstart)

## numbers of leagues and seasons

n_leagues <- length(leagues)
n_seasons <- length(seasons)

## total number of teams across all seasons of each league

(n_teams_bl <- aggregate(team ~ league, alldat,
	function(x) length(unique(x))))

## number of teams in each season of each league

(n_teams_bs_bl <- xtabs(~ league + seasonstart, alldat))

## points per game and proportions of wins

alldat$ppg <- alldat$points / alldat$matches
alldat$winprop <- alldat$wins / alldat$matches

## whether or not a team finished in the top 4

alldat$top4 <- ifelse(alldat$position < 5, 1, 0)

## differences from immediately-below-rank team in win proportion and points per game

alldat$diffppg <- NA
alldat$diffwinprop <- NA

for (l in leagues) {
	for (s in seasons) {
		## obtain data
		
		rows <- which(alldat$league == l & alldat$seasonstart == s)
		tab <- as.matrix(alldat[rows, c('winprop', 'ppg')])
		
		## differences from immediately below-rank team
		
		for (i in (seq_along(rows) - 1)) {
			alldat$diffppg[rows[i]] <- tab[i, 2] - tab[i+1, 2]
			alldat$diffwinprop[rows[i]] <- tab[i, 1] - tab[i+1, 1]
		}
	}
}

## remove redundant columns and rearrange columns

names(alldat)
alldat <- subset(alldat, select = c(league, seasonstart, position, top4,
	team, matches, ppg, winprop, diffppg, diffwinprop, week1:week38))
names(alldat)
summary(alldat)

##### LEAGUE AVERAGES #####

## average points per game grouped by league and seasonstart combined

(ppg_league_ss <- aggregate(ppg ~ league + seasonstart, alldat, mean))

## average points per game and win proportion grouped by position and league combined

pos_league_avg <- aggregate(cbind(ppg, winprop) ~ position + league, alldat, 
	mean)
head(pos_league_avg)

## average differences from immediately-below-rank team in points per game and in win proportion
## grouped by position and league combined

pos_league_diffavg <- aggregate(cbind(diffppg, diffwinprop) ~ 
	position + league, alldat, mean)
head(pos_league_diffavg)

## average points after each week grouped by position and league combined

weeks <- paste0('week', seq(1, 38))
weeks_pos_league <- aggregate(alldat[, weeks], 
	list(position = alldat$position, league = alldat$league), mean)
head(weeks_pos_league)

##### SUBSET OF DATA FOR TEAMS THAT FINISHED IN THE TOP 4 POSITIONS #####

names(alldat)
(top4dat <- subset(alldat, position < 5, 
	select = c(league, position, team, ppg, winprop)))
summary(top4dat)

## numbers of times teams have finished in the top 4

top4counts <- aggregate(position ~ team + league, top4dat, length)
top4counts
top4counts[, 1:2] <- top4counts[, 2:1]
colnames(top4counts) <- c('league', 'team', 'count')
## order by league and decreasing count
top4counts <- top4counts[with(top4counts, 
	order(league, count, decreasing = c(F, T))), ]
top4counts

## number of teams that finished in the top 4 in each league

(n_top4 <- xtabs(~ league, top4counts))

## average points per game and average win proportions
## grouped by position and league combined

(top4means <- aggregate(cbind(ppg, winprop) ~ position + league, top4dat, 
	mean))

########## DATA VISUALIZATION ##########

## function for getting colors for plots
## n: number of colors needed
## i: index of the color palette listed in hcl.pals('qualitative')
## i = 1: "Pastel 1"
## i = 2: "Dark 2"
## i = 3: "Dark 3"
## i = 4: "Set 2"
## i = 5: "Set 3"
## i = 6: "Warm"
## i = 7: "Cold"
## i = 8: "Harmonic"
## i = 9: "Dynamic"
## i > 9: the list of palettes gets recycled
## alpha: color transparency; a single number or a vector of numbers between 0 and 1

getcol <- function(n, i, alpha = NULL) {
	if (i %% 9 != 0) {
		i <- i %% 9
	}
	hcl.colors(n, hcl.pals('qualitative')[i], alpha)
}

## colors to distinguish between leagues

col_league <- getcol(n_leagues, 9)
col_league2 <- getcol(n_leagues, 9, .5)

## colors to distinguish between seasons

col_season <- getcol(n_seasons, 9)

## function for saving plots as png files
## name: a descriptive name for the file (without the .png extension)
## w, h: width and height (in pixels) of the image

saveaspng <- function(name, w = 700, h = 480) {
	filename <- paste0('Plots/', name, '.png')
	png(filename, w, h)
}

##### PLOTS OF DISTRIBUTION OF NUMBER OF TEAMS #####

barplot(t(n_teams_bs_bl), col = col_season, ylab = 'Number of teams',
	main = 'Distribution of the Number of Teams by League and Season', 
	ylim = c(0, max(rowSums(n_teams_bs_bl)) * 1.15),
	legend.text = seasons, args.legend = list(x = 'topleft', 
	title = 'Season', adj = .1, horiz = T, inset = .02))

barplot(n_teams_bl$team ~ n_teams_bl$league, col = col_league,
	main = 'Numbers of Unique Teams Grouped by League', xlab = '',
	ylab = 'Number of unique teams', 
	ylim = c(0, max(n_teams_bl$team) * 1.1))

##### PLOTS OF POINTS PER GAME #####

boxplot(alldat$ppg ~ alldat$league, col = col_league, xlab = NULL,
	main = 'Points Per Game Grouped by League', ylab = 'Points per game', 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
## add mean points
points(aggregate(ppg ~ league, alldat, mean)$ppg, bg = 2, pch = 23, 
	cex = 1.2)
## add text to explain the mean points
mtext('The red diamond shapes represent the mean values.', adj = 0)

## box positions
boxpos <- vector('list', n_seasons)
for (i in 1:n_seasons) {
	boxpos[[i]] <- seq((i - 1) * n_leagues + i, i * n_leagues + i - 1)
}
boxpos

boxplot(ppg ~ league + seasonstart, data = alldat, show.names = F,
	main = 'Points Per Game Grouped by League and Season',
	xlab = 'Year season started', ylab = 'Points per game',
	ylim = c(min(alldat$ppg) / 1.1, max(alldat$ppg) * 1.1),
	col = col_league, boxwex = .6, medlwd = 2, whisklwd = .5, 
	staplewex = .3, outcex = .5, at = unlist(boxpos))
## add labels and legend
mtext(seasons, at = sapply(boxpos, mean), side = 1, line = 1)
legend('topleft', leagues, fill = col_league, ncol = 2, inset = c(.1, 0))
## add mean points
for (i in seq_along(seasons)) {
	points(boxpos[[i]], 
		ppg_league_ss$ppg[ppg_league_ss$seasonstart == seasons[i]], bg = 2, 
		pch = 23, cex = 1.2)
	points(NA)
}
## add text to explain the mean points
mtext('The red diamond shapes represent the mean values.', adj = 0)

plot(alldat$ppg, type = 'n', xlab = 'Season-end position',
	main = 'Points Per Game as a Function of Season-End Position',
	ylab = 'Points per game', xlim = c(1, max(alldat$position)), 
	ylim = range(pos_league_avg$ppg))
for (i in seq_along(leagues)) {
	points(pos_league_avg$ppg[pos_league_avg$league == leagues[i]], 
		type = 'b', col = i, pch = i, lty = i)
}
## add vertical lines at positions 6 and 8
abline(v = c(6, 8), col = i + 1:2, lty = i + 1:2, lwd = 3)
text(c(5, 9), 1, paste0('Position #', c(6, 8)))
## add legend
legend('top', leagues, col = 1:i, pch = 1:i, lty = 1:i, inset = .1)

##### PLOT OF WIN PROPORTIONS #####

plot(alldat$winprop, type = 'n', xlab = 'Season-end position',
	main = 'Win Proportions as a Function of Season-End Position',
	ylab = 'Win proportion', xlim = c(1, max(alldat$position)), 
	ylim = range(pos_league_avg$winprop))
for (i in seq_along(leagues)) {
	points(pos_league_avg$winprop[pos_league_avg$league == leagues[i]], 
		type = 'b', col = i, pch = i, lty = i)
}
## add vertical line at position = 7
abline(v = 7, col = i + 1, lty = i + 1, lwd = 3)
text(6, .3, 'Position #7')
## add legend
legend('top', leagues, col = 1:i, pch = 1:i, lty = 1:i, inset = .1)

##### PLOTS OF DIFFERENCES FROM IMMEDIATELY-BELOW-RANK TEAMS IN POINTS PER GAME #####

boxplot(alldat$diffppg ~ alldat$league, col = col_league, xlab = NULL,
	ylab = 'Difference in points per game', main = paste('Differences From', 
	'Immediately-Below-Rank Teams in Points Per Game, Grouped by League'), 
	boxwex = .6, medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
## add mean points
points(aggregate(diffppg ~ league, alldat, mean)$diffppg, bg = 'red', 
	pch = 23, cex = 1.2)
## add text to explain the mean points
mtext('The red diamond shapes represent the mean values.', adj = 0)

plot(alldat$diffppg, type = 'n', xlim = c(1, max(alldat$position) - 1),
	main = paste('Differences From Immediately-Below-Rank Teams in',
	'Points Per Game, as a Function of Season-End Position'),
	xlab = 'Season-end position', ylab = 'Difference in points per game',
	ylim = range(pos_league_diffavg$diffppg))
for (i in seq_along(leagues)) {
	with(pos_league_diffavg, points(diffppg[league == leagues[i]],
		type = 'b', col = i, pch = i, lty = i))
}
## add horizontal line at difference = .15 and vertical line at position = 2
abline(h = .15, v = 2, col = i + 1:2, lty = i + 1:2, lwd = 3)
## add explanatory text
text(11, .16, 'equivalent to 5-6 points in all games per season')
text(2.3, .27, paste0('difference between\nsecond-place team\n',
	'and third-place team'), adj = 0)
## add legend
legend('top', leagues, col = 1:i, pch = 1:i, lty = 1:i, inset = .1)

##### PLOT OF DIFFERENCES FROM IMMEDIATELY-BELOW-RANK TEAMS IN WIN PROPORTION #####

plot(alldat$diffwinprop, type = 'n', xlim = c(1, max(alldat$position) - 1),
	main = paste('Differences From Immediately-Below-Rank Teams in',
	'Win Proportion, as a Function of Season-End Position'),
	xlab = 'Season-end position', ylab = 'Difference in win proportion',
	ylim = range(pos_league_diffavg$diffwinprop))
for (i in seq_along(leagues)) {
	with(pos_league_diffavg, points(diffwinprop[league == leagues[i]],
		type = 'b', col = col_league[i], pch = i, lty = i))
}
## add horizontal line at difference = .065 and vertical line at position = 2
abline(h = .065, v = 2, col = i + 1:2, lty = i + 1:2, lwd = 3)
## add explanatory text
text(12, .07, 'difference = .065, or more than 2 wins per season')
text(2.3, .125, paste0('difference between\nsecond-place team\n',
	'and third-place team'), adj = 0)
## add legend
legend('top', leagues, col = 1:i, pch = 1:i, lty = 1:i, inset = .1)

##### PLOTS OF POINTS AFTER EACH WEEK #####

## function for plotting points after each week in each league
## league: the league in question

plot_weekpts <- function(league) {
	## obtain data
	
	n_matches <- max(alldat$matches[alldat$league == league])
	leaguedat <- weeks_pos_league[weeks_pos_league$league == league,
		weeks[1:n_matches]]
	
	## plot
	
	plot(t(leaguedat), type = 'n', xlim = c(1, n_matches),
		main = paste('Points After Each Week Grouped by Season-End',
		'Position in the', league), xlab = 'Week in season', 
		ylab = 'Number of points', ylim = range(weeks_pos_league[, weeks], 
		na.rm = T))
	for (i in seq_len(nrow(leaguedat))) {
		points(t(leaguedat[i, ]), type = 'b', col = i, pch = 25-i, lty = i)
	}
	## add explanatory text
	mtext(paste('Different season-end positions are denoted by',
		'different combinations of colors, symbols, and line types.'), 
		adj = 0, line = .1)
}

plot_weekpts(leagues[1])
## add vertical lines at weeks 8 and 16
abline(v = c(8, 16), col = getcol(2, 9), lty = c(4, 2), lwd = 3)
text(c(7, 17.2), c(25, 51), paste('Week', c(8, 16)))

plot_weekpts(leagues[2])
## add vertical lines at weeks 3 and 16
abline(v = c(3, 16), col = rev(getcol(2, 9)), lty = c(2, 4), lwd = 3)
text(c(1.8, 17.2), c(15, 51), paste('Week', c(3, 16)))

plot_weekpts(leagues[3])
## add vertical lines at weeks 18 and 19
abline(v = c(18, 19), col = getcol(2, 9), lty = c(4, 2), lwd = 3)
text(c(16.8, 20.2), c(40, 50), paste('Week', c(18, 19)))

plot_weekpts(leagues[4])
## add vertical lines at weeks 9 and 17
abline(v = c(9, 17), col = rev(getcol(2, 9)), lty = c(2, 4), lwd = 3)
text(c(7.8, 18.2), c(30, 58), paste('Week', c(9, 17)))

##### PLOTS OF DATA FOR TEAMS THAT FINISHED IN THE TOP 4 POSITIONS #####

## number of teams in the top 4

bp <- barplot(n_teams_bl$team, col = col_league2, ylab = 'Number of teams',
	main = paste0('Total Number of Teams and Number of Teams That ',
	'Finished in the Top 4\nAcross All Seasons, Grouped by League'), 
	ylim = c(0, max(n_teams_bl$team) * 1.1))
barplot(n_top4, col = col_league, axes = F, add = T)
## add text denoting percentages
text(bp, 4, paste0(round(n_top4 * 100 / n_teams_bl$team, 2), '%'), font = 2)
## add explanatory text
mtext(paste('The darker area in each bar represents teams that',
	'finished in the top 4.'), line = -1)

## points per game

bp <- barplot(top4means$ppg, col = getcol(4, 9),
	main = paste('Points Per Game of Teams That Finished in the Top 4,',
	'Grouped by Season and League'), ylab = 'Points per game',
	ylim = c(0, max(top4means$ppg) * 1.1), space = c(1, rep(0, 3)),
	legend.text = 1:4, args.legend = list(x = 'top', horiz = T, adj = .75,
	title = 'Season-end position', inset = .02))
## add labels
at <- sapply(split(bp, gl(n_leagues, 4)), mean)
mtext(leagues, at = at, side = 1, line = 1)
## add text denoting mean values
text <- paste0('(M = ', 
	round(aggregate(ppg ~ league, top4means, mean)$ppg, 2), ')')
mtext(text, at = at, side = 1, line = 2.5, font = 3)

##### PLOTS COMPARING THE TOP 4 TEAMS VS. TEAMS OUTSIDE THE TOP 4 #####

## points per game

boxplot(ppg ~ top4 + league, alldat, show.names = F, col = 3:4,
	main = paste('Comparing Points Per Game of Top-4 Vs. Non-Top-4',
	'in Each League'), xlab = NULL, ylab = 'Points per game', boxwex = .6, 
	medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
## add labels
mtext(c('Not top 4', 'Top 4'), at = 1:(n_leagues*2), side = 1, line = 1,
	col = 3:4)
mtext(leagues, at = seq(1, n_leagues*2, 2) + .5, side = 1, line = 3)
## add mean points
points(aggregate(ppg ~ top4 + league, alldat, mean)$ppg, bg = 2, pch = 23, 
	cex = 1.2)
## add text to explain the mean points
mtext('The red diamond shapes represent the mean values.', adj = 0)

## win proportions

boxplot(winprop ~ top4 + league, alldat, show.names = F, col = 3:4,
	main = paste('Comparing Win Proportions for Top-4 Vs. Non-Top-4',
	'in Each League'), xlab = NULL, ylab = 'Win proportion', boxwex = .6, 
	medlwd = 2, whisklwd = .5, staplewex = .3, outcex = .5)
## add labels
mtext(c('Not top 4', 'Top 4'), at = 1 :(n_leagues*2), side = 1, line = 1,
	col = 3:4)
mtext(leagues, at = seq(1, n_leagues*2, 2) + .5, side = 1, line = 3)
## add mean points
points(aggregate(winprop ~ top4 + league, alldat, mean)$winprop, bg = 2, 
	pch = 23, cex = 1.2)
## add text to explain the mean points
mtext('The red diamond shapes represent the mean values.', adj = 0)

########## STATISTICAL ANALYSES ##########

##### K-MEANS CLUSTERING OF TEAMS BASED ON PERFORMANCES #####

## function for performing k-means clustering on each league
## i: index of the league leagues vector
## c: number of clusters

k_means <- function(i, c) {
	## obtain data
	
	df <- subset(pos_league_avg, league == leagues[i], select = c(ppg, 
		winprop))
	df <- data.frame(df, row.names = 1:nrow(df))
	
	## k-means clustering
	
	set.seed(6)   ## for reproducible results
	kmeans(df, c)
}

## function for visualizing clusters obtained from k_means()
## i: index of the league leagues vector
## km: the k-means result returned by k_means()

kmviz <- function(i, km) {
	## obtain data
	
	df <- subset(pos_league_avg, league == leagues[i], select = c(ppg, 
		winprop))
	df <- data.frame(df, row.names = 1:nrow(df))
	
	## visualize clusters
	
	plot(winprop ~ ppg, df, col = km$cluster, pch = km$cluster,
		main = paste0('Clusters of Teams in the ', leagues[i], '\nBased on',
		' Win Proportions and Points Per Game'), xlab = 'Points per game',
		ylab = 'Win proportion')
	## add cluster centers
	points(km$centers, col = 1:max(km$cluster), pch = 8, cex = 3, lwd = 3)
}

## perform k-means clustering on each league and visualize the clusters

par(mfrow = c(2, 2))
km <- vector('list', length(leagues))
for (i in seq_along(leagues)) {
	km[[i]] <- k_means(i, 4)
	kmviz(i, km[[i]])
}
par(mfrow = c(1, 1))