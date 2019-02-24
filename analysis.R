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
    select(name, duration_ms, popularity)
  track_data
}

combine_data_frames <- function(billboard_data) {
  tracks_info <- as.data.frame(sapply(paste(billboard_data$title, billboard_data$artist), track_ids)) 
  tracks_info <- as.data.frame(t(tracks_info)) %>% 
    mutate(name = as.character(name)) %>% 
    mutate(name = gsub("\\s*\\([^\\)]+\\)", "", name)) %>% 
    mutate(name = gsub("-.*", "", name)) %>% 
    mutate(name = gsub("!", "", name)) %>% 
    mutate(name = gsub("  ", " ", name)) %>% 
    mutate(name = trimws(name)) %>% 
    mutate(name = toupper(name))
  rownames(tracks_info) <- 1:nrow(tracks_info)
  billboard_data <- mutate(billboard_data, title = toupper(title)) %>% 
    mutate(title = gsub("!", "", title))
  combined_tracks_info <- inner_join(billboard_data, tracks_info, by = c("title" = "name")) %>% 
    mutate(popularity = unlist(popularity))
  combined_tracks_info
}

