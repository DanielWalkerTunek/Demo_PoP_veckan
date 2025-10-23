#########
# paket
#########
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)

#########
# ladda dataset
#########
songs <- read.csv("dataset.csv")

# behåll bara nödvändiga kolumner och rensa dubbletter
songs <- songs %>%
  select(track_name, artists, track_genre, danceability, energy, valence, tempo, popularity, track_id) %>%
  rename(
    låt = track_name,
    artist = artists,
    genre = track_genre
  ) %>%
  distinct(låt, artist, .keep_all = TRUE) %>%
  group_by(låt, artist) %>%
  slice(1) %>%
  ungroup()

#########
# ui
#########
ui <- fluidPage(
  theme = shinytheme("flatly"),  # använd modernt blågrönt tema
  titlePanel("🎵 Låtmatcharen — hitta din perfekta låt!"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🎚️ Justera din känsla:"),
      br(),
      
      sliderInput("dance", "💃 Dansbarhet", 0, 1, 0.5, step = 0.05),
      sliderInput("energy", "⚡ Energi", 0, 1, 0.5, step = 0.05),
      sliderInput("valence", "😊 Glädje (Valence)", 0, 1, 0.5, step = 0.05),
      
      sliderInput(
        "tempo", "🎵 Tempo (BPM)",
        min = floor(min(songs$tempo, na.rm = TRUE)),
        max = ceiling(max(songs$tempo, na.rm = TRUE)),
        value = round(median(songs$tempo, na.rm = TRUE)),
        step = 1,
        width = '100%',
        ticks = FALSE
      ),
      
      selectInput("genre", "🎧 Välj genre",
                  choices = c("Alla", sort(unique(songs$genre)))),
      br(),
      
      actionButton("find", "🔍 Hitta låtar!", 
                   class = "btn btn-primary btn-lg",
                   style = "width:100%; background-color:#1DB954; border:none; color:white;"),
      br(), br(),
      
      tags$div(
        style = "font-size: 0.9em; color: gray;",
        "Justera reglagen och klicka på knappen för att hitta låtar som matchar din känsla!"
      )
    ),
    
    mainPanel(
      h3("🎶 Dina 5 bästa låtmatchningar", style = "margin-top: 10px;"),
      hr(),
      DTOutput("results")
    )
  )
)

#########
# server
#########
server <- function(input, output) {
  observeEvent(input$find, {
    filtered <- songs
    
    # filtrera på vald genre om inte "Alla"
    if (input$genre != "Alla") {
      filtered <- filtered %>% filter(genre == input$genre)
    }
    
    # beräkna hur nära låten matchar användarens inställningar
    filtered <- filtered %>%
      mutate(
        skillnad = abs(danceability - input$dance) +
          abs(energy - input$energy) +
          abs(valence - input$valence) +
          abs((tempo - input$tempo) / 200)
      ) %>%
      arrange(skillnad, desc(popularity)) %>%
      slice_head(n = 5) %>%
      mutate(
        Spotify = paste0(
          '<a href="https://open.spotify.com/track/', track_id, 
          '" target="_blank" style="color:#1DB954; font-weight:bold;">🎧 Spela på Spotify</a>'
        )
      ) %>%
      select(
        Låt = låt,
        Artist = artist,
        Genre = genre,
        Skillnad = skillnad,
        Spotify
      ) %>%
      mutate(Skillnad = round(Skillnad, 3))
    
    # visa interaktiv tabell med klickbara spotify-länkar
    output$results <- renderDT({
      filtered
    },
    escape = FALSE,
    options = list(
      dom = 't',
      pageLength = 5,
      ordering = FALSE,
      autoWidth = TRUE
    ))
  })
}

#########
# kör appen
#########
shinyApp(ui, server)
