
rm(list = ls())
library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(shinyWidgets)
theme_set(theme_bw())

# read in data
df_map <- st_read("data/jobcreation.shp")
df_unemp <- read_csv("data/unemployment and E-to-P aggregates.csv")
# df_neet <- read_csv(paste0(local_data_folder,"Matt/neet_entry_and_exit_rate.csv"))
df_job_mobility <- read_csv("data/job mobility rate aggregates.csv")
df_duration <- read_csv("data/duration unemployed shares.csv")
df_map2 <- st_read("data/youth unemployment sa4 map.shp")
df_map2 <- df_map2[!is.na(df_map2$date),]

ui <- shinyUI(
  fluidPage(
    theme = bs_theme(version = 5,
                     bg = "#303233",
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

    fluidRow(class = "m-3 justify-content-center",
             column(width = 12, class = "card m-2",
              h3("The pandemic recovery has left behind vulnerable groups"),
              br(),
              fluidRow(width = 12,
                column(width = 7, class = "m-2", style = "",
                  div(
                    # img(src = "unemployment and E-to-P aggregates.png"),
                    plotlyOutput("unemployment"),
                    fluidRow(
                      class = "card-body",
                      column(6,
                      selectInput('measure', 'Select measure:', 
                                  choices = unique(df_unemp$measure))),
                      column(6,
                      checkboxGroupInput('ages', 'Select age groups:',
                                         choices = unique(df_unemp$age_group),
                                         selected = c("Total", "15-24 years"))),
                    ),
                  ),
                ),
                column(width = 4, class = "m-2",
                  h6("First takeaway"),
                  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. "),
                  
                  h6("Additional takeaway"),
                  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."),
        
                  ),
                ),
              ), 
            ),
          
    
    # Section 2
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
        h3("Job mismatch is exacerbated by recessions"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
            div(
              img(
                src = "scatter-plot.jpg",
                width = "20%",
                height = "20%"
              ),
              div(
                h5("Graduate Mismatch in 2015 vs 2020"),
                p("Source: QILT Survey"),
                class = "card-body -2"
              ),
              
            ),
         ),
          column(width = 4, class = "m-2",
                 h6("First takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. "),
                 
                 h6("Additional takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."),
        ),
        ),
        fluidRow(
          column(
            width = 7, class = "m-2",
            div(
              #img(src = "job mobility rate aggregates.png"),
              plotlyOutput("job_mobility"),
                  div(
                  checkboxGroupInput('ages_jm', 'Select age groups:',
                                   choices = unique(df_job_mobility$age_group),
                                   selected = c("Total", "15-24 years")),
                  class = "card-body"),
              class = "m-2",
              style = ""
            ),
          ),
          column(width = 4, class = "m-2",
                 h6("First takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. "),
                 
                 h6("Additional takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."),
                 ),
        ),
        
      ),
    ),
 

    # Section 3
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "m-2 card",
        h3("Outcomes have worsened for the long-term unemployed"),
        br(),
        fluidRow(
          column(width = 12, class = "m-2",
            div(
              #img(src = "duration unemployed shares.png"),
              plotlyOutput("duration_unemployed"),
              fluidRow(
                  column(width = 6,
                  selectInput("age_dur_1", "Select first age group:",
                              choices = unique(df_duration$age_group),
                              selected = "15-24 years")),
                  column(width = 6,
                  selectInput("age_dur_2", "Select second age group:",
                              choices = unique(df_duration$age_group),
                              selected = "Total")),
                  class = "card-body "),
              
            ),
            
          ),
        ),
        fluidRow(
          column(width = 6, class = "m-2",
                 h6("First takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. ")),
          column(width = 5, class = "m-2",
                 h6("Additional takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem.")),
        ),
        div(class = "m-2",
        br(),
        h5("Unemployed share by age group over time"),
        ),
        fluidRow(
          column(width = 7, class = "m-2",
            
              #  img(src = "youth unemployment sa4 map.png",
              #     width = "60%",
              #     height = "60%"),
              leafletOutput("map2"),
               
              
            ),
          column(width = 4, class = "m-2",
          selectInput("age_map", "Select age group: ", 
                      choices = unique(df_map2$age),
                      selected = "15-24 years"),
          sliderTextInput("timeline", "Select date: ",
                          choices = seq(min(df_map2$date), max(df_map2$date), by = "months"),
                          selected = min(df_map2$date)
                          # animate = animationOptions(interval = 50, loop = TRUE)
                          # Note that animation needs to be fixed - it currently causes the map to reload, which takes too much time
        ))),
        div(class = "m-2",
        p("Source: [INSERT SOURCE]", style = "font-size:10pt; color:grey" ),
        ),
        fluidRow(
          column(width = 6, class = "m-2",
                 h6("First takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi.")),
          column(width = 5, class = "m-2",
                 h6("Additional takeaway"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."))),
        ),
        
      ),
    

    # Section 4
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
        h3("... and those in disadvantaged areas"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
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
            
          ),
          
        ),
        
      
      column(width = 4, class = "m-2",
             h6("First takeaway"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi."),
             
             h6("Additional takeaway"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."))
    ))),

    # Section 5
    fluidRow(class = "m-3 justify-content-center", 
      column(width = 12, class = "card m-2",
        h3("Youth NEET in disadvantaged areas is of concern"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
          div(
            img(src = "animated_gradient_males_shadow.gif", height = "auto", 
                width = "20%"),
            div(
              h5("Growing disparity across regions"),
              p("Source: HILDA"),
              class = "card-body "
            ),
            
          ),
         
        ),
        
        column(width = 4, class = "m-2",
               h6("First takeaway"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. "),
               
               h6("Additional takeaway"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."))),
      )
    ),

    # Section 6
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
      h3("Exiting disadvantage and vulnerability"),
      fluidRow(
        column(width = 7, class = "m-2",
        div(
          h5("Map: net change in jobs by industry and location"),
          p(em("Change in jobs per 1000 workers between 2002 - 2021")),
          selectInput("name",
                      "Select industry",
                      unique(df_map$indstry)),
          leafletOutput("map"),
          ),
        br(),
        p("Source: BLADE Data Industries with less than 10 firms excluded", 
          style = "font-size:10pt; color:grey")),
        
        column(width = 4, class = "m-2",
               br(),
               h6("First takeaway"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem 
                    fringilla dolor, sit amet porttitor elit nulla vel arcu. 
                    Mauris enim diam, euismod non arcu et, consequat ultricies 
                    mi. "),
               
               h6("Additional takeaway"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem.")),
          ),
      fluidRow(
        column(width = 8, class = "m-2",
          div(
            h5("Table: change in jobs by industry and location"),
            p(em("Change in jobs per 1000 workers between 2002 - 2021")),
            selectInput("name_area",
                        "Select location",
                        unique(df_map$s3_n_16)),
            dataTableOutput("table")
          ),
          br(),
          p("Source: BLADE Data Industries with less than 10 firms excluded", 
            style = "font-size:10pt; color:grey"),
          ),
      ),
        ),
      ),
   
   
   
      )
    )
 
            
           
        
     
      
 

server <- function(input, output, session) {

  chart_bg_color <-'black'
  chart_text_color <- 'white'
  
  # Section 1
 
  output$unemployment <- renderPlotly({
  
    req(input$ages)
  
    ue_graph <- df_unemp %>% filter(measure == input$measure, age_group == input$ages) %>%
      plot_ly(x = ~date, y = ~value, split = ~age_group, type = "scatter", mode = "lines")
    
    ue_graph <- ue_graph %>% layout(
      showlegend = TRUE,
      title = "Measures of employment by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = input$measure, zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      annotations = list(text = "Source: ABS (2022), Labour Force, Detailed",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )
  

  })
  
  # Section 2
  
  output$job_mobility <- renderPlotly({
    
    req(input$ages_jm)
    
    jm_graph <- df_job_mobility %>% filter(age_group == input$ages_jm) %>% 
      plot_ly(x = ~date, y = ~value, split = ~age_group, type = "scatter", mode = "lines")
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Job mobility by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Job mobility", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      annotations = list(text = "Source: [INSERT SOURCE]",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)
    )
  })
  
  # Section 3
  
  output$duration_unemployed <- renderPlotly({
    
    req(input$age_dur_1)
    req(input$age_dur_2)
    
    # note each trace needs to be added individually to make the legend and 
    # colours work
    dur_graph_1 <- df_duration %>% filter(age_group == input$age_dur_1) %>% 
      plot_ly(x = ~date) %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "10 years or more"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "10 years or more",
                name = ~measure, showlegend = F, fillcolor = "#db410d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "5-9 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "5-9 years ago",
                name = ~measure, showlegend = F, fillcolor = "#db520d") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "3-4 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-4 years ago",
                name = ~measure, showlegend = F, fillcolor = "#db6d0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "1-2 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "1-2 years ago", 
                name = ~measure, showlegend = F, fillcolor = "#db8c0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "6-12 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "6-12 months ago",
                name = ~measure, showlegend = F, fillcolor = "#d9a53f") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "3-6 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-6 months ago",
                name = ~measure, showlegend = F, fillcolor = "#e3ca84") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Less than 3 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Less than 3 months ago",
                name = ~measure, showlegend = F, fillcolor = "#e8dc9e") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Never worked before"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Never worked before",
                name = ~measure, showlegend = F, fillcolor = "#9a9c9b")
    
    
    dur_graph_1 <- dur_graph_1 %>% layout(
      
      title = "Duration of unemployment by age group",
      xaxis = list(title = input$age_dur_1, zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Share of unemployed", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      annotations = list(text = "Source: [INSERT SOURCE]",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      paper_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )

    
    dur_graph_2 <- df_duration %>% filter(age_group == input$age_dur_2) %>% 
      plot_ly(x = ~date) %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "10 years or more"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "10 years or more",
                name = ~measure, showlegend = T, fillcolor = "#db410d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "5-9 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "5-9 years ago",
                name = ~measure, showlegend = T, fillcolor = "#db520d") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "3-4 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-4 years ago",
                name = ~measure, showlegend = T, fillcolor = "#db6d0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "1-2 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "1-2 years ago", 
                name = ~measure, showlegend = T, fillcolor = "#db8c0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "6-12 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "6-12 months ago",
                name = ~measure, showlegend = T, fillcolor = "#d9a53f") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "3-6 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-6 months ago",
                name = ~measure, showlegend = T, fillcolor = "#e3ca84") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Less than 3 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Less than 3 months ago",
                name = ~measure, showlegend = T, fillcolor = "#e8dc9e") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Never worked before"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Never worked before",
                name = ~measure, showlegend = T, fillcolor = "#9a9c9b")
      

    dur_graph_2 <- dur_graph_2 %>% layout(
      title = "Duration of unemployment by age group",
      xaxis = list(title = input$age_dur_2, zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Share of unemployed persons", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color),
      legend = list(title = list(text = "Time since last employed"), traceorder = "reversed")
    )
    
    dur_graph <- subplot(dur_graph_1, dur_graph_2, titleX = T, shareY = T, 
                         margin = 0.02)
    
  })
  
  output$map2 <- renderLeaflet({
    Unemployment <- df_map2 %>% filter(age == input$age_map, 
                                       date == input$timeline,
                                       sex == "Total") 
    domain <- c(0,50)
    
    pal <- colorNumeric("OrRd",domain = domain)
    leaflet(Unemployment) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.2,color = ~pal(Unemployment$value),
                  fillOpacity = 0.7) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend("bottomright",opacity = 1, pal = pal,values=~domain, title = "Unemployed share (%)")

      
  })
  
  
  
  # Section 6
  
  
  output$map <- renderLeaflet({
     
    data_map <- subset(df_map, indstry == input$name) %>%
      rename(Net = net)
    
    max <- max(c(abs(min(data_map$Net)),max(data_map$Net)))
    domain <- c(-max,max)
    
    pal2 <- colorNumeric("RdYlGn",domain = domain)
    leaflet(data_map) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.2,color = ~pal2(data_map$Net),
                  fillOpacity = 0.7) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend("bottomright",opacity = 1, pal = pal2,values=~domain, title = "Net Change in Jobs")
    
  })
  


  output$table <- renderDataTable({
    df_map%>%
      as.data.frame() %>%
      filter(s3_n_16 == input$name_area) %>%
      select(indstry, net, gross) %>%
      rename(Net = net,
             Gross = gross,
             Industry = indstry)
  },
  options = list(
    pageLength = 5,
    searching = F,
    paging = F,
    scrollY = 200,
    scrollCollapse = T,
    fixedHeader = T
    
    
    
    
  ))
 }

shinyApp(ui, server)
