library(jsonlite)
library(dplyr)

# Examples of how to get data-frames from the individual .json files. 

billboard_60 <- fromJSON("years/1960.json")
billboard_70 <- fromJSON("years/1970.json")
billboard_80 <- fromJSON("years/1980.json")
billboard_90 <- fromJSON("years/1990.json")
billboard_00 <- fromJSON("years/2000.json")
billboard_10 <- fromJSON("years/2010.json")
billboard_11 <- fromJSON("years/2011.json")
billboard_12 <- fromJSON("years/2012.json")
billboard_13 <- fromJSON("years/2013.json")
billboard_14 <- fromJSON("years/2014.json") 
billboard_15 <- fromJSON("years/2015.json")
