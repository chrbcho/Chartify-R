library(shiny)
library(bslib)
library(dplyr)

# ── DATA ─────────────────────────────────────────────────────────────

spotify_data <- read.csv("data/spotify_clean.csv")


# ── UI ───────────────────────────────────────────────────────────────

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  title = "Chartify (R)",
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      radioButtons(
        inputId = "most_playedon",
        label = "Select Platform",
        choices = c("Spotify", "Youtube", "Both"),
        selected = "Both"
      ),
    ),
    
    # value box row
    layout_columns(
      value_box(
        title = "Avg Streams",
        value = textOutput("avg_stream"),
        theme = value_box_theme(bg = "#1DB954", fg = "white")
      ),
      value_box(
        title = "Avg Likes",
        value = textOutput("avg_likes"),
        theme = value_box_theme(bg = "#1DB954", fg = "white")
      ),
      value_box(
        title = "Avg Views",
        value = textOutput("avg_views"),
        theme = value_box_theme(bg = "#1DB954", fg = "white")
      ),
      fill = FALSE
    ),

    card(
      card_header(h4("Top 5 Songs")),
      tableOutput("top_5_table")
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── REACTIVE CALC ──────────────────────────────────────────────────
    filtered_data <- reactive({
    if (input$most_playedon == "Both"){
      return(spotify_data)
    } else {
      return(spotify_data |> 
               filter(most_playedon == input$most_playedon))
    }
    })
  
  # ── OUTPUT COMPONENTS ──────────────────────────────────────────────
  
  output$avg_stream <-  renderText({
    value <- mean(filtered_data()$Stream, na.rm = TRUE)
    format(round(value, 0), big.mark = ",")
  })
  
  output$avg_likes <-  renderText({
    value <- mean(filtered_data()$Likes, na.rm = TRUE)
    format(round(value, 0), big.mark = ",")
  })
  
  output$avg_views <- renderText({
    value <- mean(filtered_data()$Views, na.rm = TRUE)
    format(round(value, 0), big.mark = ",")
  })
  
  output$top_5_table <- renderTable({
    filtered_data() |>
      arrange(desc(Stream)) |>
      slice_head(n = 5) |>
      select(Track, Artist, Album, Stream)
  })
}

# ── APP ───────────────────────────────────────────────────────────────
  
shinyApp(ui, server)