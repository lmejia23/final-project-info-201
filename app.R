library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)
library("jsonlite")

source("analysis.R")

year_data <- read.csv("final_project_data.csv")

my_ui <- fluidPage(
  theme = "bootstrap.css",
  titlePanel(
    title = div(img(src = "logo_final.jpg", height = 200, width = 900, style = "display: block; margin-left: auto; margin-right: auto;")), windowTitle = "Music Analyzer"
  ),
  navbarPage(
    title = "",
    tabPanel("About", includeMarkdown("about_tab.Rmd")),
    tabPanel(
      "Analysis",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "2 Years",
          sidebarLayout(
            sidebarPanel(
              selectInput("year1",
                label = h3("Select Year 1"),
                choices = list(
                  "1980" = "1980", "1981" = "1981", "1982" = "1982", "1983" = "1983",
                  "1984" = "1984", "1985" = "1985", "1986" = "1986", "1987" = "1987",
                  "1988" = "1988", "1989" = "1989", "1990" = "1990", "1991" = "1991",
                  "1992" = "1992", "1993" = "1993", "1994" = "1994", "1995" = "1995",
                  "1996" = "1996", "1997" = "1997", "1998" = "1998", "1999" = "1999",
                  "2000" = "2000", "2001" = "2001", "2002" = "2002", "2003" = "2003",
                  "2004" = "2004", "2005" = "2005", "2006" = "2006", "2007" = "2007",
                  "2008" = "2008", "2009" = "2009", "2010" = "2010", "2011" = "2011",
                  "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015"
                ),
                selected = "1980"
              ),
              selectInput("year2",
                label = h3("Select Year 2"),
                choices = list(
                  "1980" = "1980", "1981" = "1981", "1982" = "1982", "1983" = "1983",
                  "1984" = "1984", "1985" = "1985", "1986" = "1986", "1987" = "1987",
                  "1988" = "1988", "1989" = "1989", "1990" = "1990", "1991" = "1991",
                  "1992" = "1992", "1993" = "1993", "1994" = "1994", "1995" = "1995",
                  "1996" = "1996", "1997" = "1997", "1998" = "1998", "1999" = "1999",
                  "2000" = "2000", "2001" = "2001", "2002" = "2002", "2003" = "2003",
                  "2004" = "2004", "2005" = "2005", "2006" = "2006", "2007" = "2007",
                  "2008" = "2008", "2009" = "2009", "2010" = "2010", "2011" = "2011",
                  "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015"
                ),
                selected = "2015"
              ),
              radioButtons("feature",
                label = h3("Select Feature of Interest"),
                choices = list(
                  "Popularity" = "Popularity", "Danceability" = "Danceability", "Energy" = "Energy",
                  "Loudness" = "Loudness", "Valence" = "Valence", "Tempo" = "Tempo", "Words_Per_Second" = "Words_Per_Second"
                ),
                selected = "Popularity"
              ),
              includeMarkdown("descriptions.md")
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs",
                tabPanel("Plot", plotOutput("my_plot")),
                tabPanel("Summary", tableOutput("summary")),
                tabPanel("Total Data Table", tableOutput("total_data")),
                tabPanel("Statistical Summary", verbatimTextOutput("adv_summary"))
              )
            )
          )
        ),
        tabPanel(
          "Multiple Years",
          sidebarLayout(
            sidebarPanel(
              sliderInput("range", label = h3("Select Range of Years"), min = 1980, max = 2015, value = c(1980, 2015), sep = ""),
              radioButtons("feature2",
                label = h3("Select Feature of Interest"),
                choices = list(
                  "Popularity" = "Popularity", "Danceability" = "Danceability", "Energy" = "Energy",
                  "Loudness" = "Loudness", "Valence" = "Valence", "Tempo" = "Tempo", "Words_Per_Second" = "Words_Per_Second"
                ),
                selected = "Popularity"
              ),
              includeMarkdown("descriptions.md")
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs",
                tabPanel("Plot", plotOutput("plot2"), br(), br(), br(), br(), br(), br(), br(), br(), div(textOutput("test"), style = "font-size:150%")),
                tabPanel("Summary", tableOutput("summary2")),
                tabPanel("Total Data Table", tableOutput("total_data2")),
                tabPanel("Statistical Summary", verbatimTextOutput("adv_summary2"))
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Song Search",
      sidebarLayout(
        sidebarPanel(
          titlePanel("Audio Analysis"),
          textInput(
            inputId =
              "songTitle",
            label = "Song Title"
          ),
          textInput(
            inputId =
              "artistName",
            label = "Artist Name"
          ),
          includeMarkdown("searchFuncDescriptions.md")
        ),
        mainPanel(
          div(tableOutput("table"), style = "font-size:275%"), br(), uiOutput("spotifyframe")
        )
      )
    )
  )
)

my_server <- function(input, output) {
  output$test <- renderText({
    if (input$feature2 == "Popularity") {
      paste("The popularity of a track is a value between 0 and 100, with 100 being the most popular. Over the course of 35 years there has been an overall growth. 
            This is due to idea that popular music lyrics now include more words related to a focus on the self, fewer words describing
            companionship and social contact and more anti-social words correlating with the overall increase in tendencies towards loneliness and social isolation. Judging from the
            growth, we can say that songs which have charted more recently are more popular among today's Spotify listeners.")
    } else if (input$feature2 == "Danceability") {
      "Danceability describes how suitable a track is for dancing.  A value of 0.0 is least danceable and 1.0 is most danceable. Over the course of 35 years there 
      hasn't really been any notable change in danceablility. This is probably related to the fact that most popular music is easy listening and easy to dance to. There isn't a very
      strong deviance in the data with most years hovering around an average danceability of .64"
    } else if (input$feature2 == "Energy") {
      "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Over the course of 35 years there have been 
      fluctuations in energy, however overall it has shown a general increase. From 1980 to the mid 1990's, there has been growth with occasional negative inflections. 
      From then on, another point of inflection occurs, indicating an increase in energy. Again, another inflection occurs in mid 2005 era indicating
      another decrease in energy. However, the energy of music in later years (2000's onward) is greater than that of music from the 1980's-1990's.
      This graph shows similarity to danceability trends of the same time frame. These trends could be related to the increase of electronic music and 
      other genres such as hip-hop and its sub-genres."
    } else if (input$feature2 == "Loudness") {
      "The overall loudness of a track in decibels (dB). Values typically range between -60 and 0 dB. Over the course of 35 years there has a been a steady increase in
      loudness. This is due to digital recordings now allowing sound to be louder overall without introducing audible background static and tape hiss, so audio engineers often elevate
      the volume of a song to the recordable limit without having to sacrifice quality and fidelity for loudness."
    } else if (input$feature2 == "Valence") {
      "A measure from 0.0 to 1.0 describing the musical positivity conveyed by a track. Over the course of 35 years there has been a steady decrease in valence. This could be due to the 
      idea that music nowadays has fewer words describing companionship and social contact, and more anti-social words correlating with the overall increase in tendencies towards
      loneliness, social isolation, and pschopathology."
    } else if (input$feature2 == "Tempo") {
      "The overall estimated tempo of a track in beats per minute (BPM). Over the course of 35 years there has been parabolic shape in the values of Tempo. In other words, 
      from the years 1980 to 2000 there was a steady decline in tempo, however from 2000 to 2015 there was a steady increase. Another factor that may lead
      to the recent (2000's and onward) increase in tempo could be attributed to the introductions of newer genres such as EDM (Electronic Dance Music)."
    } else if (input$feature2 == "Words_Per_Second") {
      "The average words per second of a song. From 1980 to the early 2000's we see a general increase with a positive point of inflection of the
      words per minute. This large static increase in lyrical density is most likely due to rap/hip hop becoming more prevelent in the mainstream.
      After 2000, there is a negative inflection point and we begin to see the words per second slow down and actually begin to 
      decrease after 2008. This trend suggests that a decrease will continue, which also can be attributed to the increase of music that is less lyrical 
      based and more focused on rhythm (EDM, Mumble Rap, etc)"
    }
  })

  output$summary2 <- renderTable({
    final_data <- year_data[FALSE, ]
    for (i in 0:(input$range[2] - input$range[1])) {
      yeari <- year_data %>% filter(year == (i + input$range[1]))
      yeari <- summarize(
        yeari,
        year = (i + input$range[1]),
        avg_pop = mean(as.numeric(popularity)),
        avg_dance = mean(as.numeric(danceability)),
        avg_energy = mean(as.numeric(energy)),
        avg_loudness = mean(as.numeric(loudness)),
        avg_valence = mean(as.numeric(valence)),
        avg_tempo = mean(as.numeric(tempo)),
        avg_words_sec = mean(num_words / (duration_ms / 1000))
      )
      final_data <- rbind(final_data, yeari)
    }
    final_data
  })

  output$plot2 <- renderPlot({
    final_data <- year_data[FALSE, ]
    for (i in 0:(input$range[2] - input$range[1])) {
      yeari <- year_data %>% filter(year == (i + input$range[1]))
      yeari <- summarize(
        yeari,
        year = (i + input$range[1]),
        avg_pop = mean(as.numeric(popularity)),
        avg_dance = mean(as.numeric(danceability)),
        avg_energy = mean(as.numeric(energy)),
        avg_loudness = mean(as.numeric(loudness)),
        avg_valence = mean(as.numeric(valence)),
        avg_tempo = mean(as.numeric(tempo)),
        avg_words_sec = mean(num_words / (duration_ms / 1000))
      )
      final_data <- rbind(final_data, yeari)
    }
    names(final_data) <- c("Year", "Popularity", "Danceability", "Energy", "Loudness", "Valence", "Tempo", "Words_Per_Second")
    p <- ggplot(final_data) +
      geom_point(
        mapping = aes_string(x = "Year", y = input$feature2),
        color = "black"
      ) +
      geom_smooth(
        mapping = aes_string(x = "Year", y = input$feature2), se = F,
        color = "#1ED760"
      ) +
      theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20)
      )
    p
  }, height = 550, width = 900)

  output$total_data2 <- renderTable({
    total_data2 <- year_data %>% filter(year >= input$range[1] & year <= input$range[2])
    total_data2
  })

  output$total_data <- renderTable({
    year_data1 <- year_data %>% filter(year == input$year1)
    year_data2 <- year_data %>% filter(year == input$year2)
    total_data1 <- rbind(year_data1, year_data2, stringsAsFactors = FALSE)
    total_data1
  })

  output$adv_summary2 <- renderPrint({
    final_data <- year_data %>% filter(year >= input$range[1] & year <= input$range[2])
    final_data <- summary(final_data)
    final_data <- final_data[, -1]
    final_data <- final_data[, -2]
    final_data <- final_data[, -3]
    final_data <- final_data[, -1]
    final_data
  })

  output$summary <- renderTable({
    year1 <- year_data %>% filter(year == input$year1)
    year1 <- summarize(
      year1,
      year = input$year1,
      avg_pop = mean(as.numeric(popularity)),
      avg_dance = mean(as.numeric(danceability)),
      avg_energy = mean(as.numeric(energy)),
      avg_loudness = mean(as.numeric(loudness)),
      avg_valence = mean(as.numeric(valence)),
      avg_tempo = mean(as.numeric(tempo)),
      avg_words_sec = mean(num_words / (duration_ms / 1000))
    )
    year2 <- year_data %>% filter(year == input$year2)
    year2 <- summarize(
      year2,
      year = input$year2,
      avg_pop = mean(as.numeric(popularity)),
      avg_dance = mean(as.numeric(danceability)),
      avg_energy = mean(as.numeric(energy)),
      avg_loudness = mean(as.numeric(loudness)),
      avg_valence = mean(as.numeric(valence)),
      avg_tempo = mean(as.numeric(tempo)),
      avg_words_sec = mean(num_words / (duration_ms / 1000))
    )
    total_sum <- rbind(year1, year2, stringsAsFactors = FALSE)
    total_sum
  })

  output$adv_summary <- renderPrint({
    year_data1 <- year_data %>% filter(year == input$year1)
    year_data2 <- year_data %>% filter(year == input$year2)
    total_data1 <- rbind(year_data1, year_data2, stringsAsFactors = FALSE)
    total_data1 <- summary(total_data1)
    total_data1 <- total_data1[, -1]
    total_data1 <- total_data1[, -2]
    total_data1 <- total_data1[, -3]
    total_data1 <- total_data1[, -1]
    total_data1
  })

  output$my_plot <- renderPlot({
    year1 <- year_data %>% filter(year == input$year1)
    year1 <- summarize(
      year1,
      year = input$year1,
      avg_pop = mean(as.numeric(popularity)),
      avg_dance = mean(as.numeric(danceability)),
      avg_energy = mean(as.numeric(energy)),
      avg_loudness = mean(as.numeric(loudness)),
      avg_valence = mean(as.numeric(valence)),
      avg_tempo = mean(as.numeric(tempo)),
      avg_words_sec = mean(num_words / (duration_ms / 1000))
    )
    year2 <- year_data %>% filter(year == input$year2)
    year2 <- summarize(
      year2,
      year = input$year2,
      avg_pop = mean(as.numeric(popularity)),
      avg_dance = mean(as.numeric(danceability)),
      avg_energy = mean(as.numeric(energy)),
      avg_loudness = mean(as.numeric(loudness)),
      avg_valence = mean(as.numeric(valence)),
      avg_tempo = mean(as.numeric(tempo)),
      avg_words_sec = mean(num_words / (duration_ms / 1000))
    )
    total_sum <- rbind(year1, year2, stringsAsFactors = FALSE)
    names(total_sum) <- c("Year", "Popularity", "Danceability", "Energy", "Loudness", "Valence", "Tempo", "Words_Per_Second")
    p <- ggplot(total_sum) +
      geom_col(
        mapping = aes_string(x = "Year", y = input$feature),
        color = "black",
        fill = "#1ED760"
      ) +
      theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20)
      )
    p
  }, height = 550, width = 900)

  output$table <- renderTable({
    shiny::validate(
      need(input$songTitle, "Please choose a song title"),
      need(input$artistName, "Please choose an artist")
    )
    single_track_analysis(input$songTitle, input$artistName)
  })

  output$spotifyframe <- renderUI({
    shiny::validate(
      need(input$songTitle, ""),
      need(input$artistName, "")
    )
    track_info <- track_ids(paste(input$songTitle, input$artistName))
    track_id <- track_info$id
    tags$div(HTML(paste0('<iframe src="https://open.spotify.com/embed/track/', track_id, '" width="300" height="380" frameborder="0" allowtransparency="true" allow="encrypted-media"></iframe>')))
  })
}

shinyApp(ui = my_ui, server = my_server)
