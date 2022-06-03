library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(data.table)
library(magrittr)
library(tidyverse)

theme_set(theme_bw())
df <- st_read("./data/jobcreation.shp")

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
        
      ),
      width = 12,
    ),
    class = "m-3 justify-content-center"),
    
    # Section 2
    fluidRow(
      column(
        h6("Job mismatch is exacerbated by recessions", class = "display-6"),
        fluidRow(
          column(
            div(
              img(
                src = "scatter-plot.jpg",
                width = "75%",
                height = "75%"
              ),
              div(
                h5("Graduate Mismatch in 2015 vs 2020"),
                p("Source: QILT Survey"),
                class = "card-body -2"
              ),
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
          ),
        ),
        width = 12,
        class = " m-3 justify-content-center "
      )
    ),

    # Section 3
    fluidRow(
      column(
        h6("Outcomes have worsened for the long-term unemployed", class = "display-6"),
        fluidRow(
          column(
            div(
              img(src = "duration unemployed shares.png"),
              div(h5("Graph Here"),
                  p("Graph blurb wow"),
                  class = "card-body "),
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
          )
        ),
        width = 12,
        class = "m-3 justify-content-center "
      )
    ),

    # Section 4
    fluidRow(
      column(
        h6("... and those in disadvantaged areas", class = "display-6"),
        fluidRow(column(
          div(
            img(
              src = "dot plot.png",
              width = "75%",
              height = "75%"
            ),
            div(
              h5("Graduate outcomes for disadvantaged students"),
              p("Source: QILT Survey"),
              class = "card-body -2"
            ),
            class = "card",
            style = ""
          ),
          width = 5,
          class = "m-2"
        )),
        width = 12,
        class = " m-3 justify-content-center  "
      )
    ),

    # Section 5
    fluidRow(
      column(
        h6("Youth NEET in disadvantaged areas is of concern", class = "display-6"),
        fluidRow(column(
          div(
            img(src = "animated_gradient_males_shadow.gif"),
            div(
              h5("Growing disparity across regions"),
              p("Source: HILDA"),
              class = "card-body "
            ),
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
    fluidRow(column(
      h6("Exiting disadvantage and vulnerability", class = "display-6"),
      fluidRow(
        column(
          div(
            leafletOutput("map"),
            div(
              h5("Net Jobs Created by Area"),
              p("Per 1000 Workers between 2002 - 2021"),
              p("Source: BLADE Data Industries with less than 10 firms excluded"),
              selectInput("name",
                          "Select Industry",
                          unique(df$indstry)),
              class = "card-body"
            ),
            class = "card",
            style = ""
          ),
          width = 4,
          class = "m-2"
        ),
        column(
          div(
            div(
              h5("Net Jobs Created in Area"),
              p("Per 1000 Workers between 2002 - 2021"),
              p("Source: BLADE Data Industries with less than 10 firms excluded"),
              selectInput("name_area",
                          "Select Area",
                          unique(df$s3_n_16)),
              class = "card-body",
              dataTableOutput("table"),

            ),
            class = "card",
            style = ""
          ),
          width = 7,
          class = "m-2"
        )
      ), width = 12
    ))
  )
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    data_map <- subset(df, indstry == input$name)
    
    pal <- colorBin("RdYlBu", domain = data_map$net)
    
    leaflet(data_map) %>% 
      addProviderTiles("Stamen.TonerLite") %>% 
      addPolygons(
        opacity = 1, fillOpacity = 0.7,
        weight = 2, color = "#999999",
        fillColor = ~pal(net),
        highlightOptions = highlightOptions(color = "#444444", weight = 3, bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = ~net, opacity = 0.7, title = NULL, position = "bottomright")
  })
  
  
  output$table <- renderDataTable({
    df %>%
      as.data.frame() %>%
      filter(s3_n_16 == input$name_area) %>%
      select(indstry, net, gross) %>%
      rename(Net = net,
             Gross = gross,
             Industry = indstry)
  },
  options = list(
    pageLength = 5,
    searching = FALSE,
    paginationPosition = "bottom"
  ))
}

shinyApp(ui, server)
