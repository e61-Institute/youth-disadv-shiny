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
    fluidRow(column(
      fluidRow(column(
        h1("Tracking the COVID-19 Recovery:", class = "display-1"),
        width = 8,
        offset = 2
      )),
      fluidRow(column(
        h1("Vulnerable Youth", class = "display-1"),
        width = 8,
        offset = 2
      )),
      fluidRow(column(
        h6("The e61 Institute and The Paul Ramsay Foundation", class = "h6"),
        width = 8,
        offset = 2
      )),
      width = 12
    ),
    class = "mb-5 radial"),
    
    fluidRow(column(
      h6("The pandemic recovery has left behind vulnerable groups", class = "display-6"),
      fluidRow(
        column(
          div(
            plotOutput(outputId = "mpg"),
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
    
    fluidRow(
      column(
        h6("Job mismatch is exacerbated by recessions", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body -2"),
            class = "card",
            style = ""
          ),
          width = 6,
          class = "m-2"
        )),
        width = 12,
        class = " m-3 justify-content-center "
      )
    ),
    
    fluidRow(
      column(
        h6("Outcomes have worsened for the long - term unemployed", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body "),
            class = "card",
            style = ""
          ),
          width = 6,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center "
      )
    ),
    
    fluidRow(
      column(
        h6("... and those in disadvantaged areas", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body -2"),
            class = "card",
            style = ""
          ),
          width = 6,
          class = "m-2"
        )),
        width = 12,
        class = " m-3 justify-content-center  "
      )
    ),
    
    fluidRow(
      column(
        h6("Youth NEET in disadvantaged areas is of concern", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
            div(h5("Graph Here"),
                p("Graph blurb wow"),
                class = "card-body "),
            class = "card",
            style = ""
          ),
          width = 6,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center"
      )
    ),
    
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
          width = 6,
          class = "m-2"
        )),
        width = 12,
        class = "m-3 justify-content-center  "
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
