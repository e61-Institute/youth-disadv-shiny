library(shiny)
library(bslib)
library(leaflet)
library(data.table)
library(magrittr)
library(tidyverse)
library(sf)
library(plotly)
library(shinyWidgets)
library(leaftime)
library(htmltools)
library(geojsonio)
library(waiter)
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

df_map <- readRDS("data/jobcreation.rds")

df_js <- readRDS("data/js-recipient-share-map.rds") %>% 
  filter(!is.na(date)) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))

df_map2 <- readRDS("data/youth unemployment sa4 map.rds") %>% 
  filter(!is.na(date))

df_occupation_area <- readRDS("data/occupation_area.rds") %>%
  rename(Area = SA3_name, Percent = percent_total, Occupation = two_name) %>%
  group_by(Area, age) %>%
  transmute(
    age, Area,
    Percent = sum(Percent),
    Occupation = paste0(Occupation, collapse = ", <br/>"),
    geometry
  ) %>% 
  dplyr::ungroup() %>% 
  unique()

df_unemp <- read_csv("data/unemployment and E-to-P aggregates.csv")
df_job_mobility <- read_csv("data/job mobility rate aggregates.csv")
df_duration <- read_csv("data/duration unemployed shares.csv")
df_occupation <- read_csv("data/two_digit_occupation_by_age.csv")
df_youth_unem <- read_csv("data/youth-ihad-unemployment.csv")
df_neet <- read_csv("data/aggregate_neet_rate_sa.csv")
df_neet_2 <- read_csv("data/neet-entry-exit-rates.csv")
df_duration_v_ue <- read_csv("data/duration_v_rates_unemployment.csv")
df_pc_mismatched <- read_csv("data/percent_mismatched.csv")
df_helpful <- read_csv("data/percent_helpful_transitions.csv")
df_ue_gained <- read_csv("data/percent_unemployed_gained_emp.csv")
df_educ_emp<-read_csv("data/employment_v_education.csv") %>%
  mutate(Date = lubridate::dmy(Date))
df_neet_distance <- read_csv("data/neet_distance_fitted_values.csv")
# utils

breakerfn <- function(x){
  df <- subset(df_neet_distance,wave %in% (seq(max(c(x-3,3)),x,1)))
  df$frame <- x+2000
  df$Year <- as.character(df$wave +2000)
  return(setDT(df))
}
df_neet_distance <- rbindlist(lapply(c(6:20),breakerfn))
df_neet_distance$Year <- as.factor(df_neet_distance$Year)

# UI ----------------------------------------------------------------------

ui <- shinyUI(
  fluidPage(
    autoWaiter(),
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
    
    
    tags$ul(
      tags$li(a(href = "#section-1", "Recovery progress"), class = "nav-bar-element"),
      tags$li(a(href = "#section-2", "Job mobility"), class = "nav-bar-element"),
      tags$li(a(href = "#section-3", "Long-term unemployed"), class = "nav-bar-element"),
      tags$li(a(href = "#section-4", "Disadvantaged areas"), class = "nav-bar-element"),
      tags$li(a(href = "#section-5", "Youth NEET"), class = "nav-bar-element"),
      tags$li(a(href = "#section-6", "Where are the opportunities?"), class = "nav-bar-element"),
      class = "nav-bar-container"
    ),
    
    
    
    
    
    ## Section 1 - Intro ####
    
    a(id = "section-1"),
    tags$section(fluidRow(
      class = "m-3 justify-content-center",
      ### E-P + U/E time series ####
      column(
        width = 12,
        class = "card m-2",
        h3(
          "The recovery from the pandemic has been uneven for vulnerable groups"
        ),
        br(),
        fluidRow(
          width = 12,
          column(
            width = 7,
            class = "m-2",
            style = "",
            div(
              plotlyOutput("emp_pop_ue_ts"),
              fluidRow(
                class = "card-body",
                column(6,
                       selectInput(
                         "measure", "Select measure:",
                         choices = unique(df_unemp$measure)
                       )),
                column(
                  6,
                  checkboxGroupInput(
                    "ages",
                    "Select age groups:",
                    choices = unique(df_unemp$age_group),
                    selected = c("Total", "15-24 years")
                  )
                ),
              ),
            ),
          ),
          column(
            width = 4,
            class = "m-2",
            h6("Introduction"),
            p(
              "In aggregate terms the recovery has been strong, unemployment is at historic lows, while the employment-to-population ratio is well above pre-pandemic levels. Although aggregate labour market indicators show that as a whole, the labour market is strong, including for young Australians, the recovery has been uneven for some groups of vulnerable young people, which this data visualisation will explore.
"
            ),
h6("Employment-to-population and unemployment"),
p(
  "The employment-to-population ratio for young Australians aged between 15-24 years is now higher than that of the total population after being below the total population rate throughout the 2010s.

However, the unemployment rate for 15-24 year olds continues to be significantly higher than that for the total population, although the absolute rate has declined sharply from pandemic highs to be at the lowest level since 2008. This reflected the overrepresentation of young people in industries -- hospitality and arts and recreation services -- that were most affected by the pandemic.
"
)
          ),
        ),
      ),
    )
),


  ## Section 2 - Job mobility ####
  a(id = "section-2"),
  tags$section(
    fluidRow(
      class = "m-3 justify-content-center",
      column(
        width = 12,
        class = "card m-2",
        h3("Recessions make it harder to find the best match between workers and jobs"),
        br(),
        fluidRow(
          ### Job mobility by age group ####
          column(
            width = 7,
            class = "m-2",
            div(
              plotlyOutput("job_mobility"),
              div(
                checkboxGroupInput(
                  "ages_jm",
                  "Select age groups:",
                  choices = unique(df_job_mobility$age_group),
                  selected = c("Total", "15-24 years")
                ),
                class = "card-body"
              ),
              class = "m-2",
              style = ""
            ),
          ),
          column(
            width = 4,
            class = "m-2",
            h6("Job mobility"),
            p("Recessions cause a decrease in the quality of job matches for two reasons. The relative shortage of high-quality jobs in a downturn forces workers to shift down the job quality ladder and potentially take jobs to which they are less well matched. In addition, recessions often damage labour mobility prospects, which can lead recent entrants to be trapped in poorly matched jobs.  Thus, the incidence of mismatch is likely to be greater following a sustained period of weakness in the labour market."),
            p("Job mobility (the share of workers changing jobs in the past year) has increased in 2022, following declines in 2020 and 2021 relative to pre-pandemic levels. The pandemic constrained the ability of workers to move location and switch to better matched jobs, hampering their ability to climb the job ladder. This effect appears to have eased, although part of the increase in mobility may represent a partial catch-up on previous years.")
          ),
        ),

  fluidRow(
    ### Mismatched young workers ####
    column(
      width = 7,
      class = "m-2",
      plotlyOutput("pc_mismatched")

    ),
    column(
      width = 4,
      class = "m-2",
      h6("Job mismatches"),
      p(
        "Young workers have a greater need to sort into well-matched jobs in the formative years of their careers (Topel and Ward 1992). Young job switchers experience faster wage growth than older job switchers (of around 6.5 percentage points per annum on average). This is consistent with evidence suggesting that 80 per cent of career earnings growth occurs in the first decade of work (Murphy and Welch 1990). Second, disadvantage young workers tend to experience larger wage gains from switching jobs than those that are not disadvantaged."
      )
    ),
  ),

  fluidRow(
    ### Helpful job transitions ####
    column(
      width = 7,
      class = "m-2",
      plotlyOutput("helpful_jt"),
      selectInput(
        "helpful_age",
        "Select age group: ",
        choices = unique(df_helpful$Age),
        selected = "15-24"
      )
    ),
    column(
      width = 4,
      class = "m-2",
      h6("Beneficial job transitions"),
      p("The pandemic was associated with a decline in the quality of – or what may be considered “helpful” – job transitions.  That is, the share of young workers transitioning into better matched jobs – from previously mismatched or matched jobs – declined sharply. This marked decline was not observed for older workers, nor was it observed during the GFC.")
    ),
  ),

  fluidRow(
    height = 700,
    ### Main occupation by age time series ####
    column(
      width = 7,
      class = "m-2",
      div(
        plotlyOutput("occupation_intensity"),
        div(selectInput(
          'age_gp', 'Select age groups:',
          choices = unique(df_occupation$age)
        )),
        class = "card-body"
      ),
      class = "m-2",
      style = ""
    ),
    column(
      width = 4,
      class = "m-2",
      h6("Main youth occupations"),
      p("Employment opportunities for young people are primarily in services, specifically in hospitality, food preparation and sales assistant roles. The share of young people in these roles is highest in younger age groups, reflecting young people taking up these jobs part-time alongside further education or as their first jobs after completing secondary education."),
      p("For 23-25 year olds, the share of employment in these roles is smaller, reflecting a larger share of this cohort having completed tertiary education and working in roles requiring post-secondary qualifications. However, in the lead-up to the pandemic, the share of 23-25 year olds still working in hospitality or sales has been increasing[, potentially driven by a larger share remaining in tertiary education or worse employment prospects in other industries.]")
    )
  )
  )
  ),
  ),


  ## Section 3 - LT unemployed ####
  a(id = "section-3"),
  tags$section(fluidRow(
    class = "m-3 justify-content-center",
    column(
      width = 12,
      class = "m-2 card",
      h3("Labour market outcomes are worse for the long-term unemployed..."),
      br(),
      ### Duration of unemployment by age ####
      fluidRow(column(
        width = 12,
        class = "m-2",
        div(
          plotlyOutput("duration_unemployed"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                "age_dur_1",
                "Select first age group:",
                choices = unique(df_duration$age_group),
                selected = "15-24 years"
              )
            ),
            column(
              width = 6,
              selectInput(
                "age_dur_2",
                "Select second age group:",
                choices = unique(df_duration$age_group),
                selected = "Total"
              )
            ),
            class = "card-body"
          )
        ),
      ), ),
      fluidRow(column(
        width = 6,
        class = "m-2",
        h6("Duration of unemployment by age group"),
        p(
          "Long periods of time out of employment make it more difficult to transition back into employment. An elevated share of 15-24 year olds have been unemployed for 1 year or more relative to the total population. Although the COVID-19 recession exacerbated this problem, this was an ongoing concern well before the pandemic."
        )
      )),
      fluidRow(
        ### U/E duration vs rate ####
        column(
          width = 7,
          class = "m-2",
          
          plotlyOutput("duration_v_ue"),
          selectInput(
            "dur_v_ue_date",
            "Select date: ",
            choices = unique(df_duration_v_ue$date),
            selected = "2022-06-01"
          ),
          selectInput(
            "dur_v_ue_age",
            "Select age group: ",
            choices = unique(df_duration_v_ue$age_bucket),
            selected = "15-24"
          ),
        ),
        column(
          width = 4,
          class = "m-2",
          h6("Unemployment duration and unemployment rate"),
          p(
            "[Pending real data: Areas with a higher unemployment rate tend to have longer median unemployment durations, reflecting the difficulty that the long-term unemployed have when searching for employment.]"
          )
        )
      ),
      
      fluidRow(
        ### U/E regaining employment ####
        column(
          width = 7,
          class = "m-2",
          
          plotlyOutput("pc_ue_gained"),
          selectInput(
            "ue_dur",
            "Select unemployed duration: ",
            choices = unique(df_ue_gained$Duration),
            selected = "1 year +"
          )
        ),
        column(
          width = 4,
          class = "m-2",
          h6("Transitions into employment"),
          p(
            "[Pending real data: Comments on whether this trend has been increasing or decreasing and at times this has changed]"
          )
        )
      ),
      
      div(
        class = "m-2",
        br(),
        h5(
          "Relative intensity of Jobseeker recipients share for 18-24 year olds"
        )
      ),
      fluidRow(
        ### JS relative intensity from illion ####
        column(width = 7, class = "m-2",
               leafletOutput("js_map")),
        column(
          width = 4,
          class = "m-2",
          selectInput(
            "age_js",
            "Select age group: ",
            choices = unique(df_js$age_bucket),
            selected = "18-24"
          ),
          sliderTextInput(
            "timeline_js",
            "Select date: ",
            choices = seq(min(df_js$date), max(df_js$date), by = "months"),
            selected = min(df_js$date),
            animate = animationOptions(interval = 1000, loop = F)
            # Note that animation needs to be fixed - it currently causes the map to reload, which takes too much time
          )
        )
      ),
      div(class = "m-2",
          p("Source: illion", class = "source-text")),
      fluidRow(column(
        width = 6,
        class = "m-2",
        h6("Jobseeker recipient share for 18-24 year olds"),
        p(
          "This map uses illion data to estimate the relative shares of Jobseeker and Youth Allowance (for job seekers) payments in regions across Australia. The data are presented as an index, with higher numbers indicating a greater relative share of the population in that region are on support payments compared to the rest of Australia."
        ),
        p(
          "Whilst the share of individuals on support payments change over time, the general trend is for areas that are associated with greater disadvantage such as outer suburban and regional Australia to have larger relative shares of individuals on support payments."
        )
      ), ),
    )
    )
  ), 
  
  
  ## Section 4 - Disadv areas ####
  a(id = "section-4"),
  tags$section(fluidRow(
    class = "m-3 justify-content-center",
    column(
      width = 12,
      class = "card m-2",
      h3("... and for those in disadvantaged areas"),
      br(),
      fluidRow(
        ### U/E rate vs IHAD disadvantage ####
        column(
          width = 7,
          class = "m-2",
          div(plotlyOutput("youth_unem"),),
          fluidRow(
            column(
              width = 5,
              radioButtons(
                "sex_youth_unem",
                "Select sex:",
                choices = unique(df_youth_unem$sex),
                selected = "Total"
              )
            ),

            column(
              width = 5,
              checkboxGroupInput(
                "age_youth_unem",
                "Select age groups:",
                choices = unique(df_youth_unem$age),
                selected = c("15-24 years", "Total")
              )
            )
          ),
        ),
        column(
          width = 4,
          class = "m-2",
          h6("Unemployment is higher in disadvantaged areas"),
          p(
            "Young Australians living in areas with greater household disadvantage (as measured by the ABS Index of Household Advantage and Disadvantage) tend to have more difficulty finding employment, with unemployment rates in these areas higher than in more advantaged areas. These disadvantaged areas tend to be clustered in regional Australia or the outer rings of the capital cities."
          )
        )
      ),
      div(class = "m-2",
          br(),
          h5("Unemployed share by age group over time")),
      fluidRow(
        ### Unemployed share by age group over time ####
        column(width = 7, class = "m-2",
               leafletOutput("map2"),),
        column(
          width = 4,
          class = "m-2",
          selectInput(
            "age_map",
            "Select age group: ",
            choices = unique(df_map2$age),
            selected = "15-24 years"
          ),
          sliderTextInput(
            "timeline",
            "Select date: ",
            choices = seq(min(df_map2$date), max(df_map2$date), by = "months"),
            selected = min(df_map2$date),
            animate = animationOptions(interval = 1000, loop = F)
            # Note that animation needs to be fixed - it currently causes the map to reload, which takes too much time
          )
        )
      ),
      div(class = "m-2",
          p("Source: [INSERT SOURCE]", class = "source-text")),
      fluidRow(column(
        width = 6,
        class = "m-2",
        h6("Unemployment by age group and region"),
        p(
          "Youth unemployment rates tend to be higher than that of the total population and this is true at the regional level as well."
        )
      )),
      fluidRow(
        ### Employment rate by degree level and industry ####
        # Still need to make this graph
        column(width = 7, class = "m-2",
               div(
                 div(h5(
                   "Employment rate by education level"
                 ),
                 class = "card-body-2"),
                 plotlyOutput("educ_v_emp"),
                 selectInput(
                   "age_educ_v_emp",
                   "Select age group: ",
                   choices = unique(df_educ_emp$Age),
                   selected = "15-24"
                 ),
                 selectInput(
                   "sex_educ_v_emp",
                   "Select Sex: ",
                   choices = unique(df_educ_emp$Sex),
                   selected = "Males"
                 ),
                 div(class = "m-2",
                     p("Source: ABS Detailed Labour Force Survey", class = "source-text"),),
               ),),

        column(
          width = 4,
          class = "m-2",
          h6("First takeaway"),
          p("This graph still needs to be made")
        )
      )

    )
  )
  ),
  
  ## Section 5 - Youth NEET ####
  a(id = "section-5"),
  tags$section(fluidRow(
    class = "m-3 justify-content-center",
    column(
      width = 12,
      class = "card m-2",
      h3(
        "Youth not in employment, education or training living in disadvantaged areas are a concern"
      ),
      br(),
      fluidRow(
        ### Youth NEET by age/demo time series ####
        column(
          width = 7,
          class = "m-2",
          div(plotlyOutput("neet_timeseries")),
          fluidRow(column(
            width = 6,
            selectInput(
              "neet_dem",
              "Select demographic: ",
              choices = c("Total",
                          "Gender"
                          ),
              selected = "Total"
            ),
            column(
              width = 6,
              radioButtons(
                "neet_age",
                "Select age group: ",
                choices = c("15-24 years",
                            "15-19 years",
                            "20-24 years"),
                selected = "15-24 years"
              )
            ))
          ),
          column(
            width = 9,
            class = "m-2",
            h6("Youth NEET tends to increase during economic downturns"),
            p(
              "The share of youth who are not in employment, education or training (NEET) has been broadly steady in Australia since 2000. The share tends to increase during economic downturns, such as the Global Financial Crisis in 2008 and the COVID-19 recession in 2020. In the past two years the aggregate NEET rate has declined back towards historic levels."
            )
          )
        ),
        fluidRow(
          ### NEET entry and exit rates ####
          column(
            width = 7,
            class = "m-2",
            div(plotlyOutput("neet_entry_exit")),
            fluidRow(column(
              width = 6,
              selectInput(
                "neet_entry_exit_dem",
                "Select demographic: ",
                choices = unique(df_neet_2$demo_split),
                selected = "Total"
              )
            )),
          ),

          column(
            width = 4,
            class = "m-2",
            h6("The COVID-19 recession drove a large increase in NEET"),
            p(
              "The flow of people into NEET increased sharply in 2020, driven by the COVID-19 recession. The subsequent rapid recovery in the labour market saw a large flow out of NEET status. This flow was common across capital cities and regions, and across both males and females"
            )
          )
        ),


        fluidRow(
          ### Pr(NEET) by distance from CC ####
          column(
            width = 7,
            class = "m-2",

            plotlyOutput("neet_distance"),
            p(
              "Note there appears to be an issue with the predicted values supplied -
               they remain constant for each wave.",
              style = "color: red"
            )

          ),
          column(
            width = 4,
            class = "m-2",
            h6("Youth in regional areas experience higher probabilities of being NEET"),
            p(
              "The likelihood of young Australians not being in employment, education or training increases with distance from Australia's capital cities. This suggests that economic opportunities are concentrated in the capital cities, and disadvantaged areas are being further left behind."
            )
          )
        ),

      )
    ))),
    
    
    ## Section 6 - Opportunities ####
    a(id = "section-6"),
    tags$section(fluidRow(
      class = "m-3 justify-content-center",
      column(
        width = 12,
        class = "card m-2",
        h3(
          "Where are the opportunities to exit disadvantage and vulnerability?"
        ),
        fluidRow(
          ### Map: Change in jobs by industry/location ####
          column(
            width = 7,
            class = "m-2",
            div(
              h5("Net change in jobs by industry and location"),
              p(em(
                "Change in jobs per 1000 workers between 2002 - 2021"
              )),
              selectInput("name",
                          "Select industry",
                          unique(df_map$industry)),
              leafletOutput("map_change_jobs_industry"),
            ),
            br(),
            p(
              "Source: BLADE Data Industries with less than 10 firms excluded",
              class = "source-text"
            )
          ),

          column(
            width = 4,
            class = "m-2",
            br(),
            h6("Employment opportunities vary by region and industry"),
            p("Employment opportunities vary by industry and across Australia. Young people living in more disadvantaged regions may have to relocate to find opportunities that best match their interests and skills."),
            p("The graph shows net changes in jobs by industry. A net increase in jobs indicates that the industry is growing in a region, while a decrease indicates a decline. The table supplements this with a gross measure that shows how many new jobs were created in an industry, without taking into account job destruction."),
            p("The most common occupation and industries for young people (under 25 years) tend to be in hospitality, sales and carers positions. This is true across most regions, and these industries make up a larger share of employment. In contrast, the most common occupations and industry for older workers vary much more across regions and are less concentrated.")
          ),
        ),
        fluidRow(### Table: Change in jobs by industry/location ####
                 column(
                   width = 8,
                   class = "m-2",
                   div(
                     h5("Table: change in jobs by industry and location"),
                     p(em(
                       "Change in jobs per 1000 workers between 2002 - 2021"
                     )),
                     selectInput("name_area",
                                 "Select location",
                                 unique(df_map$sa3_name_16)),
                     dataTableOutput("change_jobs_industry_table")
                   ),

                   br(),
                   p(
                     "Source: BLADE Data Industries with less than 10 firms excluded",
                     class = "source-text"
                   )
                 ),),
        fluidRow(### Top 3 youth occupations ####
                 column(
                   width = 8,
                   class = "m-2",
                   div(
                     h5("Top 3 occupations worked by Youth (19-29) in region"),
                     p(em("SA3 level")),
                     leafletOutput("area_occupation"),
                     selectInput(
                       "age_area",
                       "Select age group:",
                       unique(df_occupation_area$age)
                     )
                   ),
                   br(),
                   p("Source: MADIP ATO extracts FY20",
                     class = "source-text")
                 ))
      )
    ))
  )
)
 
            
           

# Server  -----------------------------------------------------------------

server <- function(input, output, session) {

  chart_bg_color <- "black"
  chart_text_color <- "white"
  
  ## Section 1 ####
 
  ### E-P + U/E time series ####
  output$emp_pop_ue_ts <- renderPlotly({
  
    req(input$ages)
  
    ue_graph <- df_unemp %>% filter(measure == input$measure, age_group == input$ages, date > "2000-01-01") %>%
      plot_ly(x = ~date, y = ~value, split = ~age_group, type = "scatter", mode = "lines")
    
    ue_graph <- ue_graph %>% layout(
      showlegend = TRUE,
      title = "Measures of employment by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = input$measure, zeroline = FALSE, showgrid = F,
                   ticksuffix = "%", hoverformat = ".2f"),
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
  
  ### Job mobility by age group ####
  output$job_mobility <- renderPlotly({
    
    req(input$ages_jm)
    
    jm_graph <- df_job_mobility %>% filter(age_group == input$ages_jm) %>% 
      plot_ly(x = ~date, y = ~value, split = ~age_group, type = "scatter", mode = "lines")
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Job mobility by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "Job mobility", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%", hoverformat = ".2f"),
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
  
  ### Mismatched young workers ####
  output$pc_mismatched <- renderPlotly({
    
    df_pc_mismatched$Percent_mismatch <- df_pc_mismatched$Percent_mismatch * 100
    format(df_pc_mismatched$Year, "%Y")
    
    pc_mismatched <- df_pc_mismatched %>%
      plot_ly(x = ~Year, y = ~Percent_mismatch, type = 'bar')
    
    pc_mismatched <- pc_mismatched %>% layout(
      title = "Percent of young workers mismatched",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% mismatched", zeroline = FALSE, showgrid = F,
                   ticksuffix = "%", hoverformat = ".2f"),
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
  
  ### Helpful job transitions ####
  output$helpful_jt <- renderPlotly({
    
    
    df_helpful$Percent <- df_helpful$Percent * 100
    
    helpful_jt <- df_helpful %>% filter(Age == input$helpful_age) %>% 
      plot_ly(x = ~Date, y = ~Percent, type = "scatter", mode = "lines")
    
    helpful_jt <- helpful_jt %>% layout(
      title = "Percent of workers with helpful job transitions",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% workers", zeroline = FALSE, showgrid = F, ticksuffix = "%", hoverformat = ".2f"),
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
  
  output$educ_v_emp <- renderPlotly({
    
    
    df_educ_emp$Employment <- df_educ_emp$Employment * 100
    
    educ_emp <- df_educ_emp %>% filter(Age == input$age_educ_v_emp,
                                       Sex == input$sex_educ_v_emp) %>% 
      plot_ly(x = ~Date, y = ~Employment, color = ~Education, type = "scatter", mode = "lines")
    
    educ_emp <- educ_emp %>% layout(
      title = "Employment rates by age and education level",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% population employed", zeroline = FALSE, showgrid = F, ticksuffix = "%", hoverformat = ".2f"),
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
  
  
  ### Main occupation by age time series ####
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
      yaxis = list(title = "Percent Total", zeroline = FALSE, showgrid = F, ticksuffix = "%", hoverformat = ".2f"),
      legend = list(orientation = 'h',
                    yref = "paper", y = -.45),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color)
      
    )
  })
  
  ## Section 3 ####
  
  ### Duration of unemployment by age ####
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
                   ticksuffix = "%", hoverformat = ".2f"),
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
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color),
      legend = list(title = list(text = "Time since last employed"), traceorder = "reversed")
    )
    
    dur_graph <- subplot(dur_graph_1, dur_graph_2, titleX = T, shareY = T, 
                         margin = 0.02)
    
  })
  
  ### U/E duration vs rate ####
  output$duration_v_ue <- renderPlotly({
    
    duration_v_ue <- df_duration_v_ue %>% filter(date ==input$dur_v_ue_date,
                                                 age_bucket==input$dur_v_ue_age) %>%
      plot_ly(x = ~ue, y = ~duration, text=~sa4_name, type = "scatter",  mode = "markers",name="point") %>%
      add_trace(x= ~ue,y=~fitted,mode="lines",name="fitted values")
    
    duration_v_ue <- duration_v_ue %>% layout(
      title = "Median unemployment duration v unemployment rate",
      xaxis = list(
        title = "Unemployment rate",
        zeroline = FALSE,
        showgrid = F,
        ticksuffix = "%",
        hoverformat = ".2f"
      ),
      yaxis = list(
        title = "Median duration unemployed (months)",
        zeroline = FALSE,
        showgrid = F,
        hoverformat = ".2f"
      ),
      margin = list(
        l = 70,
        r = 50,
        t = 50,
        b = 100
      ),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )
    
  })
  
  ### U/E regaining employment ####
  output$pc_ue_gained <- renderPlotly({
    
    df_ue_gained$Percent <- df_ue_gained$Percent * 100
    
    pc_ue_gained <- df_ue_gained %>% filter(Duration == input$ue_dur) %>% 
      plot_ly(x = ~Date, y = ~Percent, type = "scatter", mode = "lines")
    
    pc_ue_gained <- pc_ue_gained %>% layout(
      title = "% unemployed who gained employment",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = F),
      yaxis = list(title = "% unemployed", zeroline = FALSE, showgrid = F, ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
 
  ### JS intensity from illion ####
  output$js_map <- renderLeaflet({
    js <- df_js %>% filter(age_bucket == input$age_js,
                           date == input$timeline_js)
    domain <- c(0,100)
    
    pal <- colorNumeric("OrRd", domain = domain)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Index: %.0f",
      js$sa4_name_2016, js$rel_prop
    ) %>% 
      lapply(htmltools::HTML)
    
    leaflet(js) %>%
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.2,
        color = ~ pal(js$rel_prop),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        fillOpacity = 0.7
      ) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(
        "bottomright",
        opacity = 1,
        pal = pal,
        values =  ~ domain,
        title = "Index (lower is better)"
      )
    
    
    
  })
  
  ## Section 4 ####
  

  ### U/E rate vs IHAD disadvantage ####
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
                   ticksuffix = "%", hoverformat = ".2f"),
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
  
  ### Unemployed share by age group over time ####
  output$map2 <- renderLeaflet({
    Unemployment <- df_map2 %>%
      filter(age == input$age_map,
             date == input$timeline,
             sex == "Total")
    
    domain <- c(0, 50)
    
    pal <- colorNumeric("OrRd", domain = domain)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Unemployed share: %.2f",
      Unemployment$sa4_name, Unemployment$value
    ) %>% 
      lapply(htmltools::HTML)
    
    
    leaflet(Unemployment) %>%
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.2,
        color = ~ pal(Unemployment$value),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        fillOpacity = 0.7
      ) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(
        "bottomright",
        opacity = 1,
        pal = pal,
        values =  ~ domain,
        title = "Unemployed share (%)"
      )
    
    
    
  })
  
  ### Employment rate by degree level and industry ####
  
  # Need to do
  
  ## Section 5 ####
  
  ### Youth NEET by age/demo time series ####
  output$neet_timeseries <- renderPlotly({
    
    if(input$neet_dem == "Total"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 15-19 years`,
                                       type = "scatter", mode = "lines")
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 20-24 years`, 
                                       type = "scatter", mode = "lines")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 15-24 years`, 
                                       type = "scatter", mode = "lines")
      }
    }
    else if (input$neet_dem == "Gender"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) %>% 
          add_trace(y = ~`Males 15-19 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-19 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) %>% 
          add_trace(y = ~`Males 20-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 20-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) %>% 
          add_trace(y = ~`Males 15-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
    }
    else if (input$neet_dem == "Education"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) # note there is no data for this combo
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) %>% 
          add_trace(y = ~`Year 9 or below/Never attended school 20-24 years`, type = "scatter", mode = "lines", name = "Year 9 or below") %>% 
          add_trace(y = ~`Year 10 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 10 or equivalent") %>% 
          add_trace(y = ~`Year 11 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 11 or equivalent") %>% 
          add_trace(y = ~`Year 12 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 12 or equivalent")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date) # note there is no data for this combo
      }
    }

    neet_timeseries <- neet_timeseries %>% layout(
      title = "Youth NEET rate by age and demographic group",
      xaxis = list(title = "Date", 
                   zeroline = FALSE, showgrid = F, margin = list(b = 100)),
      yaxis = list(title = "NEET rate", zeroline = FALSE, showgrid = F,
                   tickformat = "1%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = T),
      annotations = list(text = "Source: ABS Labour Force Survey",
                         showarrow = F,
                         xref = 'paper', x = 0,
                         yref = 'paper', y = -.35,
                         font = list(size = 10, color = "grey")),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  ### NEET entry and exit rates ####
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
                   tickformat = "1%", hoverformat = ".2f"),
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
  
  ### Pr(NEET) by distance from CC ####
  output$neet_distance <- renderPlotly({
    
    p <- df_neet_distance %>% 
      arrange(mindistance) %>% 
      ggplot(aes(x=mindistance, y=pred,color=Year)) +
        geom_line(aes(frame = frame)) +scale_colour_manual(values=heat.colors(18))+
      theme(legend.position='none')
    neet_distance <- ggplotly(p) %>% 
      layout(
      title = "Probability of NEET over distance (18-24)",
      
      xaxis = list(title = "Log distance from nearest capital city", 

                   zeroline = FALSE, showgrid = F, range = list(1,6)),

      yaxis = list(title = "Predicted probability of NEET status", zeroline = FALSE, showgrid = F,
                   tickformat = "1%", dtick = 0.02, hoverformat = ".2f"),
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
  
  ### Map: Change in jobs by industry/location ####
  output$map_change_jobs_industry <- renderLeaflet({
     
    data_map <- df_map %>% 
      filter(industry == input$name) %>%
      rename(Net = net) %>% 
      st_as_sf()
    
    max <- max(c(abs(min(data_map$Net)), max(data_map$Net)))
    domain <- c(-max, max)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Net change: %.2f",
      data_map$sa3_name_16, data_map$Net
      ) %>% 
      lapply(htmltools::HTML)
    
    pal2 <- colorNumeric("RdYlGn", domain = domain)
    
    leaflet(data_map) %>%
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.2,
        color = ~ pal2(data_map$Net),
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(
        "bottomright",
        opacity = 1,
        pal = pal2,
        values = ~ domain,
        title = "Net Change in Jobs"
      )
    
  })
  
  ### Top 3 youth occupations ####
  output$area_occupation <- renderLeaflet({
    
    data_map <- df_occupation_area %>% 
      filter(age == input$age_area) %>%
      st_as_sf()
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = data_map$Percent)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      data_map$Area, data_map$Occupation
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data_map) %>%
      addPolygons(
        stroke = FALSE,
        smoothFactor = 0.2,
        color = ~ pal(Percent),
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(
        pal = pal,
        values = ~ Percent,
        opacity = 0.5,
        title = " % Total Employment",
        position = "bottomright"
      )
    
    
  })
  

  ### Table: Change in jobs by industry/location ####
  output$change_jobs_industry_table <- renderDataTable({
    df_map %>%
      as.data.frame() %>%
      filter(sa3_name_16 == input$name_area) %>%
      select(industry, net, gross) %>%
      rename(Net = net,
             Gross = gross,
             Industry = industry) %>% 
      mutate(Net = round(Net, 2), Gross = round(Gross, 2))
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
