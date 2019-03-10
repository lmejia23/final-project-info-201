library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)

my_ui <- navbarPage(
  "Spotify Data",
  tabPanel("About", includeMarkdown("about_tab.Rmd")),
  tabPanel("Analysis",
    tabsetPanel(type = "tabs",
      tabPanel("2 Years",
        sidebarLayout(
          sidebarPanel(
            selectInput("year1", label = h3("Select Year 1"),
                        choices = list("1980" = "1980", "1981" = "1981", "1982" = "1982", "1983" = "1983",
                                       "1984" = "1984", "1985" = "1985", "1986" = "1986", "1987" = "1987",
                                       "1988" = "1988", "1989" = "1989", "1990" = "1990", "1991" = "1991",
                                       "1992" = "1992", "1993" = "1993", "1994" = "1994", "1995" = "1995",
                                       "1996" = "1996", "1997" = "1997", "1998" = "1998", "1999" = "1999",
                                       "2000" = "2000", "2001" = "2001", "2002" = "2002", "2003" = "2003",
                                       "2004" = "2004", "2005" = "2005", "2006" = "2006", "2007" = "2007",
                                       "2008" = "2008", "2009" = "2009", "2010" = "2010", "2011" = "2011",
                                       "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015"),
                        selected = "1980"),
              selectInput("year1", label = h3("Select Year 2"),
                        choices = list("1980" = "1980", "1981" = "1981", "1982" = "1982", "1983" = "1983",
                                       "1984" = "1984", "1985" = "1985", "1986" = "1986", "1987" = "1987",
                                       "1988" = "1988", "1989" = "1989", "1990" = "1990", "1991" = "1991",
                                       "1992" = "1992", "1993" = "1993", "1994" = "1994", "1995" = "1995",
                                       "1996" = "1996", "1997" = "1997", "1998" = "1998", "1999" = "1999",
                                       "2000" = "2000", "2001" = "2001", "2002" = "2002", "2003" = "2003",
                                       "2004" = "2004", "2005" = "2005", "2006" = "2006", "2007" = "2007",
                                       "2008" = "2008", "2009" = "2009", "2010" = "2010", "2011" = "2011",
                                       "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015"),
                         selected = "2015"),
            radioButtons("feature", label = h3("Select Feature of Interest"),
                         choices = list("Popularity" = "Popularity", "Danceability" = "Danceability", "Energy" = "Energy",
                                        "Loudness" = "Loudness", "Valence" = "Valence", "Tempo" = "Tempo"),
                         selected = "Popularity"),
            includeMarkdown("descriptions.md")
          ),
          mainPanel(
            tabsetPanel(type = "tabs",
              tabPanel("Plot"),
              tabPanel("Summary"),
              tabPanel("Total Data Table"))
          )
        )),
      tabPanel("Multiple Years"),
      tabPanel("All Years"))),
  tabPanel("Song Search")
)

my_server <- function(input, output) {
  
}

shinyApp(ui = my_ui, server = my_server)
