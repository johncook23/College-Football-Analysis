# This RScript file creates a master data set for the years 2021-2024, which will serve as our training data

library(cfbfastR)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

years <- 2021:2025 
pull_year <- function(fn, year, ...) {
  out <- tryCatch(fn(year = year, ...), error = function(e) {
    message(sprintf("  [skip] %s year %s: %s", deparse(substitute(fn)), year, e$message))
    NULL
  })
  Sys.sleep(0.5)
  out
}

# High school recruiting rating
hs_recruits <- map_dfr(years, ~ pull_year(cfbd_recruiting_player, .x,
                                          recruit_type = "HighSchool"))

# Transfer portal entries + portal rating
portal <- map_dfr(years, ~ pull_year(cfbd_recruiting_transfer_portal, .x))

# Season-level raw production (passing/rushing/receiving/defense/etc.) 
season_stats <- map_dfr(years, ~ pull_year(cfbd_stats_season_player, .x))

# Opponent-adjusted efficiency (PPA) 
ppa_season <- map_dfr(years, ~ pull_year(cfbd_metrics_ppa_players_season, .x))

# Usage / snap share (opportunity)
usage <- map_dfr(years, ~ pull_year(cfbd_player_usage, .x))

# Draft outcome
draft <- map_dfr(years, ~ pull_year(cfbd_draft_picks, .x))

# We need to convert each API pull to a data frame in order to run merge functions 
ppa_season   <- as.data.frame(ppa_season)
season_stats <- as.data.frame(season_stats)
usage        <- as.data.frame(usage)
portal       <- as.data.frame(portal)
draft <- as.data.frame(draft)
hs_recruits <- as.data.frame(hs_recruits)

# Saving the data frames as csv's for future use
#write.csv(ppa_season, "ppa_season.csv", row.names = FALSE)
#write.csv(season_stats, "season_stats.csv", row.names = FALSE)
#write.csv(usage, "usage.csv", row.names = FALSE)
#write.csv(portal, "portal.csv", row.names = FALSE)
#write.csv(draft, "draft.csv", row.names = FALSE)
#write.csv(hs_recruits, "hs_recruits.csv", row.names = FALSE)


# We merge on athlete id's, so they all need to be consistent as characters
draft$college_athlete_id <- as.character(draft$college_athlete_id)
hs_recruits$athlete_id <- as.character(hs_recruits$athlete_id)
ppa_season$athlete_id <- as.character(ppa_season$athlete_id)
season_stats$athlete_id <- as.character(season_stats$athlete_id)
usage$athlete_id <- as.character(usage$athlete_id)

# season_stats uses "year" and "player" where the others use "season" and
# "name" — rename so the join keys and identity columns line up.
season_stats <- season_stats %>% rename(season = year, name = player)

season_level <- merge(
  ppa_season, season_stats,
  by = c("athlete_id", "season"),
  all = TRUE,
  suffixes = c("_ppa", "_stats")
)

season_level <- merge(
  season_level, usage,
  by = c("athlete_id", "season"),
  all = TRUE,
  suffixes = c("", "_usage")
)

# Sanity check — this should be TRUE. It is FALSE, but there are a minimal amount
# of duplicates so we will continue with this data
nrow(season_level) == length(unique(paste(season_level$athlete_id, season_level$season)))


# Here, we merge career ratings as draft and high school prospects to athletes'
# career stats
career <- merge(
  draft, hs_recruits,
  by.x = "college_athlete_id", by.y = "athlete_id",
  all.x = TRUE,
  suffixes = c("_draft", "_hs")
)

master <- merge(
  season_level, career,
  by.x = "athlete_id", by.y = "college_athlete_id",
  all.x = TRUE
)

# Because "portal" does not have an athlete_id column, we have to filter names 
# to merge on
clean_name <- function(x) {
  x %>% tolower() %>%
    gsub("\\.", "", .) %>%
    gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", .) %>%
    gsub("[^a-z\\s'-]", "", .) %>%
    trimws()
}

# Cleaning the data to merge without duplicates
master$name_clean  <- clean_name(master$name) 
portal$name_clean  <- clean_name(paste(portal$first_name, portal$last_name))

master <- merge(
  master, portal,
  by.x = c("name_clean", "season"),
  by.y = c("name_clean", "season"),
  all.x = TRUE
)

# Check for true duplicates 
nrow(master)
length(unique(paste(master$athlete_id, master$season)))
ncol(master)
# The numbers are different, but it is negligible for such a big data set

#taking out this column as all values are Null
master <- subset(master, select = -countable_plays) 

write.csv(master, "master_cfb_2021-2025.csv", row.names = FALSE)