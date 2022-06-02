library(shiny)
library(bslib)
library(sf)
library(tmap)
library(tidyverse)

theme_set(theme_bw())
df=st_read("data/jobcreation.shp")

ui <- shinyUI( 
  fluidPage(
    theme = bs_theme(version = 5,
                     bg = "#003039",
                     fg = "#ffffff"
                     ),
    tags$head(
      tags$style(
        type="text/css", "
                          .radial { background: rgb(255,255,255);
                                    background: linear-gradient(200deg, 
                                    rgba(255,255,255,0.8099614845938375) 0%, 
                                    rgba(0,48,57,1) 25%);
                                  }"
      ),
    ),
    fluidRow(
      column(
        fluidRow(column(h1("Tracking the COVID-19 Recovery:",class = "display-1"),width = 8, offset = 2)),
        fluidRow(column(h1("Vulnerable Youth",class = "display-1"),width = 8, offset = 2)),
        fluidRow(column(h6("The e61 Institute and The Paul Ramsay Foundation",class = "h6"),width = 8, offset = 2)),
        width = 12),
      class = "mb-5 radial"
    ),
    
    fluidRow(
      column(
        h6("The pandemic recovery has left behind vulnerable groups",class = "display-6"),
        fluidRow(
          column(
            div(img(src="artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
                div(h5("Graph Here"),
                    p("Graph blurb wow"),
                    class="card-body"
                ),
                class="card",
                style=""
            ),
            width = 5,
            class = "m-2"),
          
          column(
            div(img(src="artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
                div(h5("Graph Here"),
                    p("Graph blurb wow"),
                    class="card-body"
                ),
                class="card",
                style=""
            ),
            width = 5,
            class = "m-2")
          
        ),
        width = 12, 
        ),
      class = "m-3 justify-content-center"
    ),
    
    fluidRow(
      column(
        h6("Job mismatch is exacerbated by recessions",class = "display-6"),
        fluidRow(
          column(
            div(img(src="scatter-plot.jpg",width="75%",height="75%"),
                div(h5("Graduate Mismatch in 2015 vs 2020"),
                    p("Source: QILT Survey"),
                    class="card-body -2"
                ),
                class="card",
                style=""
            ),
            width = 6,
            class = "m-2")
        ),
        width = 12, 
        class = " m-3 justify-content-center "
      )
    ),
    
    fluidRow(
      column(
        h6("Outcomes have worsened for the long - term unemployed",class = "display-6"),
        fluidRow(
          column(
            div(img(src="artworks-XJdVplPCbvDvJlH7-jF9c4A-t500x500.jpg"),
                div(h5("Graph Here"),
                    p("Graph blurb wow"),
                    class="card-body "
                ),
                class="card",
                style=""
            ),
            width = 6,
            class = "m-2")
        ),
        width = 12, 
        class = "m-3 justify-content-center "
      )
    ),
    
    fluidRow(
      column(
        h6("... and those in disadvantaged areas",class = "display-6"),
        fluidRow(
          column(
            div(img(src="dot plot.png",width="75%",height="75%"),
                div(h5("Graduate outcomes for disadvantaged students"),
                    p("Source: QILT Survey"),
                    class="card-body -2"
                ),
                class="card",
                style=""
            ),
            width = 6,
            class = "m-2")
        ),
        width = 12, 
        class = " m-3 justify-content-center  "
      )
    ),
    
    fluidRow(
      column(
        h6("Youth NEET in disadvantaged areas is of concern",class = "display-6"),
        fluidRow(
          column(
            div(img(src="animated_gradient_males_shadow.gif"),
                div(h5("Growing disparity across regions"),
                    p("Source: HILDA"),
                    class="card-body "
                    ),
                class="card",
                style=""
              ),
            width = 6,
            class = "m-2"
            )
        ),
        width = 12, 
        class = "m-3 justify-content-center"
      )
    ),
    
    fluidRow(
      column(
        h6("Exiting disadvantage and vulnerability",class = "display-6"),
        fluidRow(
          column(
            div(tmapOutput("map"),
                div(h5("Net Jobs Created by Area"),
                    p("Per 1000 Workers between 2002 - 2021"),
                    p("Source: BLADE Data Industries with less than 10 firms excluded"),
                    selectInput("name",
                                "Select Industry",
                                unique(df$indstry)),
                    class="card-body"
                ),
                class="card",
                style=""
            ),
            width = 4,
            class = "m-2"
          ),
          column(
            div(div(h5("Net Jobs Created in Area"),
                    p("Per 1000 Workers between 2002 - 2021"),
                    p("Source: BLADE Data Industries with less than 10 firms excluded"),
                    selectInput("name_area",
                                "Select Area",
                                unique(df$s3_n_16)),
                    class="card-body",
                    dataTableOutput("table"),
                
                ),
                class="card",
                style=""
            ),
            width = 7,
            class = "m-2"
          ),
        ),
        width = 12, 
        class = "m-3 justify-content-center  "
      )
    ),
  )
)

server <- function(input, output, session) {
  
  output$map <- renderTmap({
    data_map <- subset(df,indstry==input$name)
    tmap_mode("view")
    tm_shape(data_map)+
      tm_polygons("net",
                  style = "cont",
                  palette = "RdYlBu")
  })
  
 
           
  output$table <- renderDataTable({
    df %>%
      as.data.frame()%>%
      filter(s3_n_16==input$name_area)%>%
      select(indstry,net,gross)%>%
      rename(Net=net,Gross=gross,Industry = indstry)
  },
  options = list(pageLength = 5, searching = FALSE,paginationPosition="bottom"))
}

shinyApp(ui, server)
