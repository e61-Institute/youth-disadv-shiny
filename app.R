library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(leaftime)
library(htmltools)
library(geojsonio)

theme_set(theme_bw())

# Leaflet code (not working currently) --------------------------------------

# df_map2 <- st_read("data/youth unemployment sa4 map.shp")
# df_map2 <- df_map2[!is.na(df_map2$date),]

# Unemployment <- df_map2 %>% filter(age == "15-24 years",
#                                    sex == "Total") 
# 
# Unemployment$start <- as.Date(Unemployment$date, "%Y-%m-%d")
# Unemployment$end <- as.Date(Unemployment$date, "%Y-%m-%d")
# 
# unemployment_geo <- geojsonio::geojson_json(Unemployment, lat = "cent_lt", lon = "cent_lng")
# 
# domain <- c(0,50)
# 
# pal <- colorNumeric("OrRd",domain = domain)
# 
# leaflet(Unemployment) %>% 
#   addPolygons(stroke = FALSE, smoothFactor = 0.2,color = ~pal(Unemployment$value),
#               fillOpacity = 0.7) %>%
#   addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#   addLegend("bottomright",opacity = 1, pal = pal,values=~domain, title = "Unemployed share (%)") %>% 
#   addTimeline(data = unemployment_geo)

# Read in data ------------------------------------------------------------

df_map <- st_read("data/jobcreation.shp")
df_unemp <- read_csv("data/unemployment and E-to-P aggregates.csv")
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

df_youth_unem <- read_csv("data/youth-ihad-unemployment.csv")
df_neet <- read_csv("data/aggregate_neet_rate.csv")
df_js <- st_read("data/js-recipient-share-map.shp")
df_js <- df_js[!is.na(df_js$date),]
df_js$date <- as.Date(df_js$date, "%Y-%m-%d")
df_neet_2 <- read_csv("data/neet-entry-exit-rates.csv")
df_duration_v_ue <- read_csv("data/duration_v_rates_unemployment.csv")
df_pc_mismatched <- read_csv("data/percent_mismatched.csv")
df_helpful <- read_csv("data/percent_helpful_transitions.csv")
df_ue_gained <- read_csv("data/percent_unemployed_gained_emp.csv")
df_neet_distance <- read_csv("data/neet_distance_fitted_values.csv")



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
    
    
    
    ## Navigation bar ####
    
    
    tags$ul(tags$li(a(href = "#section-1", "Recovery progress"), class = "nav-bar-element"),
            tags$li(a(href = "#section-2", "Job mobility"), class = "nav-bar-element"),
            tags$li(a(href = "#section-3", "Long-term unemployed"), class = "nav-bar-element"),
            tags$li(a(href = "#section-4", "Disadvantaged areas"), class = "nav-bar-element"),
            tags$li(a(href = "#section-5", "Youth NEET"), class = "nav-bar-element"),
            tags$li(a(href = "#section-6", "Where are the opportunities?"), class = "nav-bar-element"),
            class = "nav-bar-container"
    ),
    
    
    
   
   
    ## Section 1 ####

    a(id = "section-1"),
    tags$section(
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
            )),
          
    
    ## Section 2 ####
    a(id = "section-2"),
    tags$section(
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
            
              
              plotlyOutput("pc_mismatched"),
              
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
            
            
            plotlyOutput("helpful_jt"),
            selectInput("helpful_age", "Select age group: ",
                        choices = unique(df_helpful$Age),
                        selected = "15-24")
            
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
        
        fluidRow(height = 700,
          column(
            width = 7, class = "m-2",
            div(
              #img(src = "job mobility rate aggregates.png"),
              plotlyOutput("occupation_intensity"),
              div(
                selectInput('age_gp', 'Select age groups:', 
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
        
      )),


    ## Section 3 ####
    a(id = "section-3"),
    tags$section(
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
                          selected = min(df_map2$date),
                          animate = animationOptions(interval = 1000, loop = F)
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
        
        
        fluidRow(
          column(width = 7, class = "m-2",
                 
                 plotlyOutput("duration_v_ue"),
                 selectInput("dur_v_ue_date", "Select date: ", 
                             choices = unique(df_duration_v_ue$Date),
                             selected = "2022-06-01"),
                 p("Note: have included a date dropdown as requested, but would this be better as 
                   a timeline?", style = "color: red")
                 
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
                    mi."))
                 
          ),
        
        fluidRow(
          column(width = 7, class = "m-2",
                 
                 plotlyOutput("pc_ue_gained"),
                 selectInput("ue_dur", "Select unemployed duration: ", 
                             choices = unique(df_ue_gained$Duration),
                             selected = "1 year +")
                 
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
                    mi."))
          
        ),
        
        div(class = "m-2",
            br(),
            h5("Relative intensity of JS recipients share for 18-24 year olds"),
            p("Note: double check 'rel_prp' is the correct value to display", style =  "color:red" )
        ),
        
        
          
        
        fluidRow(
          column(width = 7, class = "m-2",
                
                 leafletOutput("js_map"),
                 
                 
          ),
          column(width = 4, class = "m-2",
                 selectInput("age_js", "Select age group: ", 
                             choices = unique(df_js$ag_bckt),
                             selected = "18-24"),
                 sliderTextInput("timeline_js", "Select date: ",
                                 choices = seq(min(df_js$date), max(df_js$date), by = "months"),
                                 selected = min(df_js$date),
                                 animate = animationOptions(interval = 1000, loop = F)
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
        
      )),
    

    ## Section 4 ####
    a(id = "section-4"),
    tags$section(
    fluidRow(class = "m-3 justify-content-center",
      column(width = 12, class = "card m-2",
        h3("... and for those in disadvantaged areas"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
          div(
            img(
              src = "dot plot.png",
              width = "30%",
              height = "30%"
            ),
            div(
              h5("Graduate outcomes for disadvantaged students"),
              p("Source: QILT Survey"),
              class = "card-body-2"
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
                    Suspendisse placerat, purus nec varius gravida, eros lorem.")
                 )
        ),
        fluidRow(
            column(width = 7, class = "m-2",
            div(
              plotlyOutput("youth_unem"),
            ),
            fluidRow(column(width = 5,
                      radioButtons("sex_youth_unem", "Select sex:", 
                           choices = unique(df_youth_unem$sex),
                           selected = "Total")),
                    column(width = 5,
                      checkboxGroupInput("age_youth_unem", "Select age groups:",
                                         choices = unique(df_youth_unem$age),
                                         selected = c("15-24 years", "Total")))
            ),
            ),
            column(width = 4, class = "m-2",
                   h6("First takeaway"),
                   p("[Something about labour market outcomes for graduates from disadvantaged backgrounds being worse than those from privileged backgrounds]"),
                   
                   h6("Additional takeaway"),
                   p("Young Australians living in areas with greater household disadvantage (as measured by the ABS Index of Household Advantage and Disadvantage) tend to have more difficulty finding employment, with unemployment rates in these areas higher than more advantaged areas. These disadvantaged areas tend to be clustered in regional Australia or the outer rings of the capital cities."))
            
            )
            
          ))),
          
       
        
      
      

    ## Section 5 ####
    a(id = "section-5"),
    tags$section(
    fluidRow(class = "m-3 justify-content-center", 
      column(width = 12, class = "card m-2",
        h3("Youth not in employment, education or training living in disadvantaged areas are a concern"),
        br(),
        fluidRow(
          column(width = 7, class = "m-2",
        
          
          div(
            plotlyOutput("neet")  
          ),
          fluidRow(
            column(width = 6,
                   selectInput("neet_dem", "Select demographic: ",
                                choices = c("Total",
                                            "Gender"
                                            # "Education"
                                            ),
                                selected = "Total")),
            column(width = 6,
                   radioButtons("neet_age", "Select age group: ",
                                choices = c("15-24 years",
                                            "15-19 years",
                                            "20-24 years"),
                                selected = "15-24 years"))
          ))),
          
          fluidRow(
            column(width = 7, class = "m-2",
                   
                   
                   div(
                     plotlyOutput("neet_entry_exit")  
                   ),
                   fluidRow(
                     column(width = 6,
                            selectInput("neet_entry_exit_dem", "Select demographic: ",
                                        choices = unique(df_neet_2$demo_split),    
                                        selected = "Total"))
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
      
    
    fluidRow(
      column(width = 7, class = "m-2",
             
             plotlyOutput("neet_distance"),  
             p("Note there appears to be an issue with the predicted values supplied - 
               they remain constant for each wave.", style = "color: red")
             
      ),
      column(width = 4, class = "m-2",
             h6("First takeaway"),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                    Pellentesque pellentesque, erat ac maximus finibus, neque 
                    magna accumsan eros, vitae faucibus felis velit ac enim. 
                    Proin sit amet diam non nunc vulputate tempor a ut nibh. 
                    Suspendisse placerat, purus nec varius gravida, eros lorem."),
             
    )),
    
    ))),
    

    ## Section 6 ####
    a(id = "section-6"),
    tags$section(
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
                 leafletOutput("area_occupation"),
                 selectInput("age_area",
                             "Select age group:",
                             unique(df_occupation_area$age))
                 
               ),
               br(),
               p("Source: MADIP ATO extracts FY20", 
                 style = "font-size:10pt; color:grey"),
        ),
      ),
        ),
      ),
   
   
   
      )    ))
 
            
           

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
      font = list(color = chart_text_color))
  })
  
  output$pc_mismatched <- renderPlotly({
    
    df_pc_mismatched$Percent_mismatch <- df_pc_mismatched$Percent_mismatch * 100
    format(df_pc_mismatched$Year, "%Y")
    
    pc_mismatched <- df_pc_mismatched %>%
      plot_ly(x = ~Year, y = ~Percent_mismatch, type = 'bar')
    
    pc_mismatched <- pc_mismatched %>% layout(
      title = "Percent of young workers mismatched",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% mismatched", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      annotations = list(text = "Source: [INSERT SOURCE]",
                         showarrow = F,
                         xref = "paper", x = 0,
                         yref = "paper", y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  
  output$helpful_jt <- renderPlotly({
    
    
    df_helpful$Percent <- df_helpful$Percent * 100
    
    helpful_jt <- df_helpful %>% filter(Age == input$helpful_age) %>% 
      plot_ly(x = ~Date, y = ~Percent, type = "scatter", mode = "lines")
    
    helpful_jt <- helpful_jt %>% layout(
      title = "Percent of workers with helpful job transitions",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% workers", zeroline = FALSE, showgrid = F, ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      annotations = list(text = "Source: [INSERT SOURCE]",
                         showarrow = F,
                         xref = "paper", x = 0,
                         yref = "paper", y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  
  output$occupation_intensity <- renderPlotly({
    
    req(input$age_gp)
    
    
    jm_graph <- df_occupation %>% filter(age == input$age_gp) %>% 
      group_by(year)%>%
      slice_max(order_by = percent_total, n = 5)%>%
      ungroup()%>%
      mutate(percent_total = percent_total*100)%>%
      plot_ly(x = ~year, y = ~percent_total, color = ~two_name, type="scatter", 
              mode = "lines") 
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Top 5 occupations by age group",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Percent Total", zeroline = FALSE, showgrid = F,ticksuffix = "%"),
      legend = list(orientation = 'h',
                    yref = "paper", y = -.45),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)
      
    )
  })
  
  ## Section 3 ####
  
  output$duration_unemployed <- renderPlotly({
    
    req(input$age_dur_1)
    req(input$age_dur_2)
    
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
  
  output$duration_v_ue <- renderPlotly({
    
    df_duration_v_ue$UE <- df_duration_v_ue$UE * 100
    
    duration_v_ue <- df_duration_v_ue %>% filter(Date == input$dur_v_ue_date) %>% 
      plot_ly(x = ~UE, y = ~MD, type = "scatter", mode = "markers")
    
    duration_v_ue <- duration_v_ue %>% layout(
      title = "Median unemployment duration v unemployment rate",
      xaxis = list(title = "Unemployment rate", zeroline = FALSE, showgrid = F, ticksuffix = "%"),
      yaxis = list(title = "Median duration unemployed (months)", zeroline = FALSE, showgrid = F),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  output$pc_ue_gained <- renderPlotly({
    
    df_ue_gained$Percent <- df_ue_gained$Percent * 100
    
    pc_ue_gained <- df_ue_gained %>% filter(Duration == input$ue_dur) %>% 
      plot_ly(x = ~Date, y = ~Percent, type = "scatter", mode = "lines")
    
    pc_ue_gained <- pc_ue_gained %>% layout(
      title = "% unemployed who gained employment",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% unemployed", zeroline = FALSE, showgrid = F, ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
 
  output$js_map <- renderLeaflet({
    js <- df_js %>% filter(ag_bckt == input$age_js, 
                                       date == input$timeline_js) 
    domain <- c(0,100)
    
    pal <- colorNumeric("OrRd",domain = domain)
    leaflet(js) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.2,color = ~pal(js$rel_prp),
                  fillOpacity = 0.7) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend("bottomright",opacity = 1, pal = pal,values=~domain, title = "Index (lower is better)")
    
    
    
  })
  
  ## Section 4 ####
  
  output$youth_unem <- renderPlotly({
    
    df_youth_unem$total <- as.numeric(df_youth_unem$total)
    df_youth_unem$date <- as.character(df_youth_unem$date)
   
    
    youth_unem_graph <- df_youth_unem %>% 
      filter(age == input$age_youth_unem, sex == input$sex_youth_unem) %>%
      rename(Date = date) %>% 
      plot_ly(x = ~share_decile_1) %>% 
      add_trace(x = ~share_decile_1, y = ~ue_rate, 
                split = ~age,
                type = "scatter",
                size = ~total,
                mode = "markers", 
                frame = ~Date) %>% 
      add_lines(x = ~share_decile_1, y = ~fv, split = ~age, frame = ~Date, name = "Trendline")
    
    youth_unem_graph <- youth_unem_graph %>% layout(
      showlegend = TRUE,
      title = "Unemployment rate against share of households who are disadvantaged",
      xaxis = list(title = "Share of households in bottom decile of disadvantage", 
                   zeroline = FALSE, showgrid = F, ticksuffix = "%", margin = list(b = 100)),
      yaxis = list(title = "Unemployment rate", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = T),
      annotations = list(text = "Source: ABS",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.5,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
    
    youth_unem_graph <- youth_unem_graph %>% 
      animation_opts(
        frame = 200, transition = 200,  easing = "linear", redraw = F
      ) %>%
      animation_slider(
        currentvalue = list(font = list(size = 12, color = "grey")),
        yref = "paper", y = -.3
      ) %>% 
      animation_button(
        yref = "paper", y = -.3
      ) 
   
    
  })
  
  ## Section 5 ####
  
  output$neet <- renderPlotly({
    
    if(input$neet_dem == "Total"){
      if(input$neet_age == "15-19 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month, y = ~`Total 15-19 years`,
                                       type = "scatter", mode = "lines")
      }
      else if (input$neet_age == "20-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month, y = ~`Total 20-24 years`, 
                                       type = "scatter", mode = "lines")
      }
      else if (input$neet_age == "15-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month, y = ~`Total 15-24 years`, 
                                       type = "scatter", mode = "lines")
      }
    }
    else if (input$neet_dem == "Gender"){
      if(input$neet_age == "15-19 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) %>% 
          add_trace(y = ~`Males 15-19 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-19 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "20-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) %>% 
          add_trace(y = ~`Males 20-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 20-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "15-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) %>% 
          add_trace(y = ~`Males 15-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
    }
    else if (input$neet_dem == "Education"){
      if(input$neet_age == "15-19 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) # note there is no data for this combo
      }
      else if (input$neet_age == "20-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) %>% 
          add_trace(y = ~`Year 9 or below/Never attended school 20-24 years`, type = "scatter", mode = "lines", name = "Year 9 or below") %>% 
          add_trace(y = ~`Year 10 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 10 or equivalent") %>% 
          add_trace(y = ~`Year 11 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 11 or equivalent") %>% 
          add_trace(y = ~`Year 12 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 12 or equivalent")
      }
      else if (input$neet_age == "15-24 years"){
        neet_ts <- df_neet %>% plot_ly(x = ~Month) # note there is no data for this combo
      }
    }
    
    
    neet_ts <- neet_ts %>% layout(
      
      title = "Youth NEET rate by age and demographic group",
      xaxis = list(title = "Date", 
                   zeroline = FALSE, showgrid = F, margin = list(b = 100)),
      yaxis = list(title = "NEET rate", zeroline = FALSE, showgrid = F,
                   tickformat = "1%"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = T),
      annotations = list(text = "Source: ABS Labour Force Survey",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
    
 
    

    
  })
  
  output$neet_entry_exit <- renderPlotly({
    
    df_neet_2$exit <- -df_neet_2$exit
    
    neet_entry_exit <- df_neet_2 %>% filter(demo_split == input$neet_entry_exit_dem) %>% 
      plot_ly(x = ~date, y = ~neet_flow, type = "scatter", mode = "lines", name = "NEET flow") %>% 
      rangeslider(start = min(df_neet_2$date), end = max(df_neet_2$date))
    
    neet_entry_exit <- neet_entry_exit %>% 
      add_trace(x = ~date, y = ~entry, type = 'scatter', fill = 'tozeroy', name = "Entries") %>% 
      add_trace(x = ~date, y = ~exit, type = 'scatter', fill = 'tozeroy', name = "Exits") 
    
    neet_entry_exit <- neet_entry_exit %>% layout(
      
      title = "NEET entry and exit rates",
      xaxis = list(title = "Date", 
                   zeroline = FALSE, showgrid = F, margin = list(b = 100)),
      yaxis = list(title = "NEET rate", zeroline = FALSE, showgrid = F,
                   tickformat = "1%"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = T),
      annotations = list(text = "Source: ABS Labour Force Survey",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.7,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
     
  })
  
  output$neet_distance <- renderPlotly({
    
    neet_distance <- df_neet_distance %>% 
      arrange(mindistance) %>% 
      plot_ly(x = ~mindistance, y = ~pred, frame = ~wave, type = "scatter", mode = "lines")
    
    neet_distance <- neet_distance %>% layout(
      
      title = "Probability of NEET over distance (Males, 18-24)",
      
      xaxis = list(title = "Log distance from nearest capital city", 
                   zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Predicted probability of NEET status", zeroline = FALSE, showgrid = F,
                   tickformat = "1%", dtick = 0.02),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = T),
      annotations = list(text = "Source: HILDA Release 2.0",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.5,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)) %>% 
      animation_opts(
        frame = 200, transition = 200,  easing = "linear", redraw = F
      ) %>%
      animation_slider(
        currentvalue = list(font = list(size = 12, color = "grey")),
        yref = "paper", y = -.3
      ) %>% 
      animation_button(
        yref = "paper", y = -.3
      ) 
    
    
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
    
    data_map <- df_occupation_area %>% filter(age == input$age_area) 
    
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
