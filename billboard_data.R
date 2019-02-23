library(jsonlite)
library(dplyr)

# Examples of how to get data-frames from the individual .json files. 

billboard_60 <- fromJSON("years/1960.json")
billboard_70 <- fromJSON("years/1970.json")
billboard_80 <- fromJSON("years/1980.json")
billboard_90 <- fromJSON("years/1990.json")
billboard_00 <- fromJSON("years/2000.json")
billboard_10 <- fromJSON("years/2010.json")

# Function which gets a specific data frame for a given year

get_year <- function(year) {
  billboard <- fromJSON(paste0("years/", year, ".json"))
  billboard <- billboard[1:50, ]
  billboard <- select(billboard, title, artist, pos)
  billboard
}
test <- 1980
View(get_year(test))