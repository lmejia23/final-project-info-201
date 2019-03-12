library(httr)
library("jsonlite")
library(dplyr)
library(tidyr)

# Get the key for accessing Spotify API

clientID = "68484b6727504e0ea7b98d1c98122c8f"
secret = "33ea1e3429714dacb1604034d5a59670"

response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

key = content(response)$access_token

auth_header = paste0('Bearer ', key)

# Get single track's name, id, duration, popularity information from Spotify API. Taking in a track's info (track's name and the artist), return the information of the track in a data frame.
track_ids <- function(name) {
  search_tracks <- GET("https://api.spotify.com/v1/search", query = list(q = name, type = "track"), add_headers(Authorization = auth_header))
  search_tracks_data <- fromJSON(content(search_tracks, "text"))
  track_data <- search_tracks_data$tracks$items %>% 
    head(1)
  if (length(track_data) == 0) { 
    stop("The track you are looking for does not exist!")
  } else {
    track_data <- select(track_data, name, id, duration_ms, popularity)
    track_data
  }
}

# Get specific year's billboard top songs data. Taking in the specific year as the parameter.
get_year <- function(year) {
  billboard <- fromJSON(paste0("years/", year, ".json")) %>% 
    select(title, artist, year, num_words)
  billboard
}

# Get the spotify data for all songs in specific billboard data file, taking in a specific billboard data file as a parameter.
spotify_data <- function(billboard_data) {
  tracks_info <- as.data.frame(sapply(paste(billboard_data$title, billboard_data$artist), track_ids)) 
  tracks_info <- as.data.frame(t(tracks_info)) %>% 
    mutate(name = as.character(name)) %>% 
    mutate(name = gsub("\\s*\\([^\\)]+\\)", "", name)) %>% 
    mutate(name = gsub("-.*", "", name)) %>% 
    mutate(name = gsub("!", "", name)) %>% 
    mutate(name = gsub("?", "", name)) %>% 
    mutate(name = gsub("  ", " ", name)) %>% 
    mutate(name = trimws(name)) %>% 
    mutate(name = toupper(name)) %>% 
    mutate(popularity = unlist(popularity), duration_ms = unlist(duration_ms))
  rownames(tracks_info) <- 1:nrow(tracks_info)
  tracks_info
}

single_track_analysis <- function(title, artist) {
  name <- paste(title, artist)
  track_id <- track_ids(name)
  audio_analysis <- get_audio_features(track_id$id)
  audio_analysis %>% 
    select(-id)
}

# single_track_analysis("Where are ü now", "Jack ü")

# Combine the billboard data from specific billboard data file with its corresponding Spotify data. Taking in a specific billboard data file as a parameter.
combined_data_frames <- function(billboard_data) {
  tracks_info <- spotify_data(billboard_data)
  billboard_data <- mutate(billboard_data, title = toupper(title)) %>% 
    mutate(title = gsub("!", "", title)) %>% 
    mutate(title = gsub("?", "", title))
  combined_tracks_info <- inner_join(billboard_data, tracks_info, by = c("title" = "name")) 
  combined_tracks_info
}

# Get the audio features for single track from Spotify API, taking a track id as the parameter.
get_audio_features <- function(track_ids) {
  track_analysis <- GET("https://api.spotify.com/v1/audio-features/", query = list(ids = track_ids), add_headers(Authorization = auth_header))
  features_result <- fromJSON(content(track_analysis, "text"))
  track_features <- features_result$audio_features %>% 
    select(id, danceability, energy, loudness, valence, tempo)
}

# Get the audio features for all tracks from Spotify API. Taking in a specific billboard data file as a parameter.
tracks_audio_features_data <- function(billboard_data) {
  get_ids <- spotify_data(billboard_data) %>% 
    mutate(id = as.list(id))
  ids <- do.call(rbind.data.frame, get_ids$id) %>% 
    unlist()
  track_features_data <- as.data.frame(sapply(ids, get_audio_features))
  track_features_data <- as.data.frame(t(track_features_data)) %>% 
    mutate(id = as.list(id))
  rownames(track_features_data) <- 1:nrow(track_features_data)
  combined_track_info <- cbind(get_ids, track_features_data, stringsAsFactors = FALSE) 
  combined_track_info <- combined_track_info[, c(-5, -2)]
  combined_track_info
}

combined_year_data <- function(billboard_songs) {
  spotify_year_data <- tracks_audio_features_data(billboard_songs)
  combined_data <- combined_data_frames(billboard_songs) %>% 
    left_join(spotify_year_data, by = c("title" = "name", "duration_ms", "popularity"))
}

