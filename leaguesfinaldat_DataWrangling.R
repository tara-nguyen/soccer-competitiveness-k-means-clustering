## Munging season-end league tables in 5 seasons of 4 soccer leagues
##
## AUTHOR: TARA NGUYEN
## Part of group project for UCLA Extension course: 
## Exploratory Data Analysis and Visualization
## Completed in November 2020

########## DATA IMPORT AND CLEANING ##########

## form tables and season-end league tables for 5 seasons (up to Season 2019/20) of 4 soccer leagues:
## - the English Premier League (EPL),
## - the La Liga,
## - the Bundesliga, and
## - the Major League Soccer (MLS; regular season only)

##### FORM TABLES #####

leagues <- c('Premier League', 'La Liga', 'Bundesliga', 
	'Major League Soccer')
leagues_abbr <- c('epl', 'laliga', 'bundesliga', 'mls')
files <- paste0('form-', leagues_abbr, '.csv')

library(readr)
eplform <- read_csv(files[1])
laligaform <- read_csv(files[2])
bligaform <- read_csv(files[3])
mlsform <- read_csv(files[4])

eplform
laligaform
bligaform
mlsform

##### SEASON-END LEAGUE TABLES #####

library(readxl)
leaguetabs <- read_excel('all-leaguetables.xlsx')
leaguetabs

## function for retrieving team names in each league
## dat: data frame containing team names in all leagues
## i: index of the element of the vector leagues that is the league name

get_teamnames <- function(dat, i) {
	sort(unique(dat$team[dat$league == leagues[i]]))
}

## function for changing team names
## dat: data containing team names
## newnames: string vector of new team names

fix_teamnames <- function(dat, newnames) {
	oldnames <- sort(unique(dat$team))
	for (newname in newnames) {
		oldname <- grep(newname, oldnames, value = T)
		dat$team <- gsub(oldname, newname, dat$team)
	}
	return(dat)
}

## change team names to match those in the form tables

(eplteams <- unique(eplform$team))
sum(get_teamnames(leaguetabs, 1) == sort(eplteams)) == length(eplteams)
## TRUE

(laligateams <- unique(laligaform$team))
get_teamnames(leaguetabs, 2)
leaguetabs[leaguetabs$league == leagues[2], ] <- fix_teamnames(
	leaguetabs[leaguetabs$league == leagues[2], ], laligateams)
sum(get_teamnames(leaguetabs, 2) == sort(laligateams)) ==
	length(laligateams)
## TRUE

(bligateams <- unique(bligaform$team))
get_teamnames(leaguetabs, leagues[3])
leaguetabs[leaguetabs$league == leagues[3], ] <- fix_teamnames(
	leaguetabs[leaguetabs$league == leagues[3], ], bligateams)
sum(get_teamnames(leaguetabs, 3) == sort(bligateams)) == length(bligateams)   
## TRUE

(mlsteams <- unique(mlsform$team))
sum(get_teamnames(leaguetabs, 4) == sort(mlsteams)) == length(mlsteams)
## TRUE

## make position a numeric variable

leaguetabs$position <- as.numeric(leaguetabs$position)

########## DATA TRANSFORMATION ##########

library(tibble)

## function for converting match results (W, D, L) 
## to cumulative number of points after each week
## dat: data frame containing match results

convert_to_pts <- function(dat) {
	dat2 <- dat
	
	## names of columns containing match results
	
	colnames <- grep('match[0-9]', names(dat2))
	
	## convert results to points
	
	dat2[, colnames] <- ifelse(dat[, colnames] == 'W', 3,
		ifelse(dat[, colnames] == 'D', 1, 0))
	dat2[, colnames] <- as_tibble(t(apply(dat2[, colnames], 1, cumsum)))

	## rename columns
	
	names(dat2) <- gsub('match([0-9]+)', 'week\\1', names(dat))
	
	return(dat2)
}

eplform2 <- convert_to_pts(eplform)
laligaform2 <- convert_to_pts(laligaform)
bligaform2 <- convert_to_pts(bligaform)
mlsform2 <- convert_to_pts(mlsform)

eplform2
laligaform2
bligaform2
mlsform2

## add league variable to form tables

eplform2$league <- leagues[1]
laligaform2$league <- leagues[2]
bligaform2$league <- leagues[3]
mlsform2$league <- leagues[4]

## combine all form tables into one dataset

library(data.table)
(all_leagues_form <- as_tibble(rbindlist(
	list(eplform2, laligaform2, bligaform2, mlsform2), fill = T)))

## merge form dataset into league dataset

names(leaguetabs)
names(all_leagues_form)

merged <- as_tibble(merge(leaguetabs, all_leagues_form, 
	by = c('league', 'season', 'team', 'matches')))
merged

## order by league, season, and position

(merged <- as_tibble(merged[with(merged, order(league, season,
	position)), ]))

## rearrange columns

names(merged)
merged <- subset(merged, select = c(league, season, position, team,
	matches, wins:week38))
names(merged)

## save new dataset to csv file

write.csv(merged, row.names = F, file = 'all-form-leaguetables.csv')