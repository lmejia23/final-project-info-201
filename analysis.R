# Main program script

# Section 3

library(httr)
library("jsonlite")
library(dplyr)
library(tidyr)
source("billboard_data.R")

clientID = "68484b6727504e0ea7b98d1c98122c8f"
secret = "e0954334710c40a88233e4538ef16954"

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

track_ids <- function(name) {
  search_tracks <- GET("https://api.spotify.com/v1/search", query = list(q = name, type = "track"), add_headers(Authorization = auth_header)) 
  search_tracks_data <- fromJSON(content(search_tracks, "text"))
  track_data <- search_tracks_data$tracks$items %>% 
    head(1) %>% 
    select(name, id, duration_ms, popularity)
  track_data
}

get_year <- function(year) {
  billboard <- fromJSON(paste0("years/", year, ".json")) %>% 
    select(title, artist, year, num_words, tags)
  billboard
}

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

combined_data_frames <- function(billboard_data) {
  tracks_info <- tracks_audio_features_data(billboard_data)
  billboard_data <- mutate(billboard_data, title = toupper(title)) %>% 
    mutate(title = gsub("!", "", title)) %>% 
    mutate(title = gsub("?", "", title))
  combined_tracks_info <- inner_join(billboard_data, tracks_info, by = c("title" = "name")) 
  combined_tracks_info
}

get_audio_features <- function(track_ids) {
  track_analysis <- GET("https://api.spotify.com/v1/audio-features/", query = list(ids = track_ids), add_headers(Authorization = auth_header))
  features_result <- fromJSON(content(track_analysis, "text"))
  track_features <- features_result$audio_features %>% 
    select(id, danceability, energy, loudness, valence, tempo)
}

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

