library(shiny)
library(bslib)
library(ggplot2)

theme_set(theme_bw())
mpg <- read.csv("http://goo.gl/uEeRGu")

ui <- shinyUI(
  fluidPage(
    theme = bs_theme(version = 5,
                     bg = "#008080",
                     fg = "#ffffff"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    ),
    
    # Title and subtitle
    fluidRow(column(
      fluidRow(column(
        h1(
          "Tracking Australia's vulnerable youth in the COVID-19 recovery",
          id = "main-title"
        ),
        width = 8,
        offset = 2
      )),
      fluidRow(column(
        h6("The e61 Institute and The Paul Ramsay Foundation", id = "author-title"),
        width = 8,
        offset = 2
      )),
      width = 12
    ),
    class = "mb-5 radial"),
    
    # Section 1
    fluidRow(column(
      h6("The pandemic recovery has left behind vulnerable groups", class = "display-6"),
      fluidRow(
        column(
          div(
            img(src = "unemployment and E-to-P aggregates.png"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        ),
        
        column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )
        
      ),
      width = 12,
    ),
    class = "m-3 justify-content-center"),
    
    # Section 2
    fluidRow(
      column(
        h6("Job mismatch is exacerbated by recessions", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "job mobility rate aggregates.png"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body-2"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        ),
        column(
          div(
            img(src = "job mobility rate aggregates.png"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body-2"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
    
    # Section 3
    fluidRow(
      column(
        h6("Outcomes have worsened for the long - term unemployed", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "duration unemployed shares.png"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        ),
        column(
          div(
            img(src = "youth unemployment sa4 map.png"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
    
    # Section 4
    fluidRow(
      column(
        h6("... and those in disadvantaged areas", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body-2"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
    
    # Section 5
    fluidRow(
      column(
        h6("Youth NEET in disadvantaged areas is of concern", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
    
    # Section 6
    fluidRow(
      column(
        h6("What opportuntities are there to exit disadvantage", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body"),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
  )
)

server <- function(input, output, session) {
  output$mpg <- renderPlot({
    g <- ggplot(mpg, aes(cty, hwy)) +
      geom_count(col = "tomato3", show.legend = F) +
      labs(
        subtitle = "mpg: city vs highway mileage",
        y = "hwy",
        x = "cty",
        title = "Counts Plot"
      )
    print(g)
  })
}

shinyApp(ui, server)
