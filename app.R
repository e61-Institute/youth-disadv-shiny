pacman::p_load(
    "shiny",
    "bslib",
    "sf",
    "leaflet",
    "data.table",
    "magrittr",
    "tidyverse",
    "plotly",
    "shinyWidgets"
)

theme_set(theme_bw())

# Read in data ------------------------------------------------------------

df_map <- st_read("data/jobcreation.shp")
df_unemp <- read_csv("data/unemployment and E-to-P aggregates.csv")
# df_neet <- read_csv(paste0(local_data_folder,"Matt/neet_entry_and_exit_rate.csv"))
df_job_mobility <- read_csv("data/job mobility rate aggregates.csv")
df_duration <- read_csv("data/duration unemployed shares.csv")
df_map2 <- st_read("data/youth unemployment sa4 map.shp")
df_map2 <- df_map2[!is.na(df_map2$date),]
df_occupation <- read_csv("data/two_digit_occupation_by_age.csv")
df_occupation_area <- st_read("data/occupation_area.shp") %>%
  rename(Area = SA3_nam, Percent = prcnt_t, Occupation = two_nam) %>%
  group_by(Area,age)%>%
  summarize(Percent  = sum(Percent),
            Occupation = paste0(Occupation, collapse = " , <br/>"))

# UI ----------------------------------------------------------------------



ui <- shinyUI(
  fluidPage(
    theme = bs_theme(version = 5,
                     bg = "#303233",
                     fg = "#ffffff"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    ),
    
    ## Title and subtitle ####
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
        h6("e61 Institute and The Paul Ramsay Foundation", id = "author-title"),
        width = 8,
        offset = 2
      )),
      width = 12
    ),
    class = "mb-5 radial"),
    
   
   
    ## Section 1 ####

    fluidRow(class = "m-3 justify-content-center",
             column(width = 12, class = "card m-2",
              h3("The recovery from the pandemic has been uneven for vulnerable groups"),
              br(),
              fluidRow(width = 12,
                column(width = 7, class = "m-2", style = "",
                  div(
                    # img(src = "unemployment and E-to-P aggregates.png"),
                    plotlyOutput("unemployment"),
                    fluidRow(
                      class = "card-body",
                      column(6,
                      selectInput("measure", "Select measure:", 
                                  choices = unique(df_unemp$measure))),
                      column(6,
                      checkboxGroupInput("ages", "Select age groups:",
                                         choices = unique(df_unemp$age_group),
                                         selected = c("Total", "15-24 years"))),
                    ),
                  ),
                ),
                column(width = 4, class = "m-2",
                  h6("Introduction"),
                  p("In aggregate terms the recovery has been strong, unemployment is at historic lows, while the employment-to-population ratio is well above pre-pandemic levels. Although aggregate labour market indicators show that as a whole, the labour market is strong, including for young Australians, the recovery has been uneven for some groups of vulnerable young people, which this data visualisation will explore.
"),
                  
                  h6("Employment-to-population and unemployment"),
                  p("The employment-to-population ratio for young Australians aged between 15-24 years is now higher than that of the total population after being below the total population rate throughout the 2010s.

However, the unemployment rate for 15-24 year olds continues to be significantly higher than that for the total population, although the absolute rate has declined sharply from pandemic highs to be at the lowest level since 2008. This reflected the overrepresentation of young people in industries -- hospitality and arts and recreation services -- that were most affected by the pandemic.
"),
        
                  ),
                ),
              ), 
            ),
          
    
    ## Section 2 ####
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
        h3("Recessions make it harder to find the best match between workers and jobs"),
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
                class = "card-body-2"
              ),
              
            ),
         ),
          column(width = 4, class = "m-2",
                 h6("First takeaway"),
                 p("Recessions cause a decrease in the quality of job match for two reasons. The relative shortage of high-quality jobs in a downturn forces workers to shift down the job quality ladder and potentially take jobs to which they are less well matched. In addition, recessions often damage labour mobility prospects, which can lead recent entrants to be trapped in poorly matched jobs.  Thus, the incidence of mismatch is likely to be greater following a sustained period of weakness in the labour market.

Job mobility (the share of workers changing jobs in the past year) has increased in 2022, following declines in 2020 and 2021 relative to pre-pandemic levels. The pandemic constrained the ability of workers to move location and switch to better matched jobs, hampering their ability to climb the job ladder. This effect appears to have eased, although part of the increase in mobility may represent a partial catch-up on previous years.
"),
                 
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
                  checkboxGroupInput("ages_jm", "Select age groups:",
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
        fluidRow(
          column(
            width = 7, class = "m-2",
            div(
              #img(src = "job mobility rate aggregates.png"),
              plotlyOutput("occupation_intensity"),
              div(
                selectInput('age_gp', 'Select Age', 
                            choices = unique(df_occupation$age))),
                class = "card-body"),
              class = "m-2",
              style = ""
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


    ## Section 3 ####
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "m-2 card",
        h3("Labour market outcomes are worse for the long-term unemployed..."),
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
                  class = "card-body"),
              
            ),
            
          ),
        ),
        fluidRow(
          column(width = 6, class = "m-2",
                 h6("First takeaway"),
                 p("Long periods of time out of employment make it more difficult to transition back into employment. An elevated share of 15-24 year olds have been unemployed for 1 year or more relative to the total population. Although the COVID-19 recession exacerbated this problem, this was an ongoing concern well before the pandemic.

[Maybe move the map to the next section]
")),
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
    

    ## Section 4 ####
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
        h3("... and for those in disadvantaged areas"),
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
              class = "card-body-2"
              ),
            
          ),
          div(
            img(
              src = "youth-ihad-unemployment.png",
              width = "75%",
              height = "75%"
            ),
            div(
              h5("Youth unemployment and household disadvantage"),
              p("Source: ABS"),
              class = "card-body-2"
            ),
            
          ),
          
        ),
        
      
      column(width = 4, class = "m-2",
             h6("First takeaway"),
             p("[Something about labour market outcomes for graduates from disadvantaged backgrounds being worse than those from privileged backgrounds]"),
             
             h6("Additional takeaway"),
             p("Young Australians living in areas with greater household disadvantage (as measured by the ABS Index of Household Advantage and Disadvantage) tend to have more difficulty finding employment, with unemployment rates in these areas higher than more advantaged areas. These disadvantaged areas tend to be clustered in regional Australia or the outer rings of the capital cities."))
    ))),

    ## Section 5 ####
    fluidRow(class = "m-3 justify-content-center", 
      column(width = 12, class = "card m-2",
        h3("Youth not in employment, education or training living in disadvantaged areas are a concern"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
          div(
            # Graph title: Youth NEET rate as percentage of young population, by age and demographic group
            img(src = "aggregate_neet_example_graph.png", height = "auto", 
                width = "50%"),
            div(
              h5("Youth NEET rate"),
              p("Source: ABS Labour Force Survey"),
              class = "card-body"
            ),
            
          ),
          div(
            img(src = "animated_gradient_males_shadow.gif", height = "auto", 
                width = "50%"),
            div(
              h5("Growing disparity across regions"),
              p("Source: HILDA"),
              class = "card-body"
            ),
            
          ),
         
        ),
        
        column(width = 4, class = "m-2",
               h6("First takeaway"),
               p("The likelihood of young Australians not being in employment, education or training increases with distance from Australia's capital cities. This suggests that economic opportunities are concentrated in the capital cities, and disadvantaged areas are being further left behind."),
               
               h6("Additional takeaway"),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."))),
      )
    ),

    ## Section 6 ####
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
      h3("Where are the opportunities to exit disadvantage and vulnerability?"),
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
               p("Employment opportunities vary by industry and across Australia. Young people living in more disadvantaged regions may have to relocate to find opportunities that best match their interests and skills."),
               
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
      fluidRow(
        column(width = 8, class = "m-2",
               div(
                 h5("Top 3 occupations worked by Youth (19-29) in region"),
                 p(em("SA3 level")),
                 selectInput("age_area",
                             "Select Age",
                             unique(df_occupation_area$age)),
                 leafletOutput("area_occupation")
               ),
               br(),
               p("Source: MADIP ATO extracts FY20", 
                 style = "font-size:10pt; color:grey"),
        ),
      ),
        ),
      ),
   
   
   
      )    )
 
            
           

# Server  -----------------------------------------------------------------

server <- function(input, output, session) {

  chart_bg_color <- "black"
  chart_text_color <- "white"
  
  ## Section 1 ####
 
  output$unemployment <- renderPlotly({
  
    req(input$ages)
  
    ue_graph <- df_unemp %>% filter(measure == input$measure, age_group == input$ages, date > "2000-01-01") %>%
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
                         xref = "paper", x = 0,
                         yref = "paper", y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )
  

  })
  
  ## Section 2 ####
  
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
                         xref = "paper", x = 0,
                         yref = "paper", y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)
    )
  })
  
  output$occupation_intensity <- renderPlotly({
    
    req(input$age_gp)
    
    jm_graph <- df_occupation %>% filter(age == input$age_gp) %>% 
      group_by(year)%>%
      slice_max(order_by=percent_total, n= 8)%>%
      ungroup()%>%
      mutate(percent_total = percent_total*100)%>%
      plot_ly(x = ~year, y = ~percent_total, color = ~two_name,type="scatter",mode = "lines") 
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Top 8 occupations by Age Category",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Percent Total", zeroline = FALSE, showgrid = F,ticksuffix = "%"),
      legend = list(orientation = 'h'),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)
    )
  })
  
  ## Section 3 ####
  
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
                         xref = "paper", x = 0,
                         yref = "paper", y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      paper_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )

    
    dur_graph_2 <- df_duration %>% filter(age_group == input$age_dur_2) %>% 
      plot_ly(x = ~date) %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "10 years or more"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "10 years or more",
                name = ~measure, showlegend = T, fillcolor = "#db410d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "5-9 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "5-9 years ago",
                name = ~measure, showlegend = T, fillcolor = "#db520d") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "3-4 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-4 years ago",
                name = ~measure, showlegend = T, fillcolor = "#db6d0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "1-2 years ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "1-2 years ago", 
                name = ~measure, showlegend = T, fillcolor = "#db8c0d") %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "6-12 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "6-12 months ago",
                name = ~measure, showlegend = T, fillcolor = "#d9a53f") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "3-6 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3-6 months ago",
                name = ~measure, showlegend = T, fillcolor = "#e3ca84") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "Less than 3 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Less than 3 months ago",
                name = ~measure, showlegend = T, fillcolor = "#e8dc9e") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
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
  
  
  
  ## Section 6 ####
  
  
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
  
  output$area_occupation <- renderLeaflet({
    
    data_map <- subset(df_occupation_area, age = input$age_area) 
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = data_map$Percent)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      data_map$Area, data_map$Occupation
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data_map) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.2,color = ~pal(Percent),
                  fillOpacity = 0.7,
                  label = labels,
                  labelOptions =labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                             textsize = "15px",
                                             direction = "auto")) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(pal = pal, values = ~Percent, opacity = 0.5, title = " % Total Employment", position = "bottomright")
      
    
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
