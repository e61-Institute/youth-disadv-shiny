library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
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

# Read in data ------------------------------------------------------------

options(readr.show_col_types = FALSE)

df_map <- readRDS("data/jobcreation.rds")

df_js <- readRDS("data/js-recipient-share-map.rds") %>% 
  filter(!is.na(date) & date >= "2021-01-01") %>% 
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

df_unemp <- read_csv("data/unemployment and E-to-P aggregates.csv") %>% 
  filter(date >= "2005-01-01")
df_job_mobility <- read_csv("data/job mobility rate aggregates.csv")
df_duration <- read_csv("data/duration unemployed shares.csv")
df_occupation <- read_csv("data/two_digit_occupation_by_age.csv")
df_youth_unem <- read_csv("data/youth-ihad-unemployment.csv")
df_neet <- read_csv("data/aggregate_neet_rate_sa.csv")
df_neet_2 <- read_csv("data/neet-entry-exit-rates.csv")
df_duration_v_ue <- read_csv("data/duration_v_rates_unemployment.csv")
df_pc_mismatched <- read_csv("data/distribution_of_skill_mismatch_by_age.csv")
df_jobswitchers <- read_csv("data/jobswitchers_by_mismatch_status.csv")
df_ue_gained <- read_csv("data/unemployed-entry-into-employment.csv")
df_educ_emp <- read_csv("data/employment_v_education.csv") %>%
  mutate(Date = lubridate::dmy(Date)) %>% 
  mutate(Employment = Employment * 100)
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
    autoWaiter(html = bs5_spinner()),
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
        h3("The recovery from the pandemic has been uneven for vulnerable groups"),
        br(),
        fluidRow(
          width = 12,
          column(
            width = 7,
            class = "m-2",
            style = "",
            div(
              plotlyOutput("emp_pop_ue_ts"),
              p("Source: ABS Labour Force, Detailed",
                class = "source-text"),
              fluidRow(
                class = "card-body",
                column(6,
                       radioButtons(
                         "agg_measure", 
                         "Select measure:",
                         choices = unique(df_unemp$measure),
                         selected = "Employment to population"
                       )),
                column(
                  6,
                  checkboxGroupInput(
                    "agg_ages",
                    "Select age groups:",
                    choices = unique(df_unemp$age_group),
                    selected = c("15-24 years", "25-64 years")
                  )
                ),
              )
            ),
          ),
          column(
            width = 4,
            class = "m-2",
            h6("The labour market is in a strong recovery phase "),
            tags$ul(
              tags$li("The Australian labour market has rebounded strongly from the initial shock of the COVID-19 pandemic, with the unemployment rate at historic lows and the employment-to-population ratio well above pre-pandemic levels."), 
              tags$li("These aggregate labour market indicators partly mask the fact that the recovery has been somewhat uneven with some evidence that certain groups of vulnerable young people have been left behind – a situation this data visualisation will explore.")
              ),
            h6("The employment-to-population ratio is high and unemployment is low"),
            tags$ul(
              tags$li("The employment-to-population ratio for young Australians aged between 15-24 years has increased well above pre-pandemic levels. This improvement has been stronger than that of the 25-64 year old population."),
              tags$li("Labour market outcomes for young people are more sensitive to economic downturns and recoveries, as shown by the sharp increase and rapid decline in the unemployment rate for 15-24 year olds during the pandemic.") 
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
              p("Source: ABS Participation, Job Search and Mobility survey",
                class = "source-text"),
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
            h6("Job mobility in the youth labour market has recovered from the initial shock of the COVID-19 pandemic, though it remains below pre-GFC levels"),
            tags$ul(
              tags$li("The ability to move between jobs allows workers to find work that better matches their skills and interests and this, in turn, typically leads to higher pay. In the early days of the pandemic, opportunities to move to better matched jobs were limited and many workers stayed with their employers."),
              tags$li("Job mobility has recovered more recently, though the rate of job mobility for young workers remains below the levels observed in the period before the Global Financial Crisis.")
            )
          ),
        ),

  fluidRow(
    ### Mismatched young workers ####
    column(
      width = 7,
      class = "m-2",
      plotlyOutput("pc_mismatched"),
      p("Source: ABS Labour Force microdata",
        class = "source-text"
        ),
      radioButtons(
        "mismatched_age",
        "Select age group: ",
        choices = unique(df_pc_mismatched$age_bucket),
        selected = "15-24",
        inline = TRUE
      )
    ),
    column(
      width = 4,
      class = "m-2",
      h6("A significant share of young workers report being mismatched to their jobs"),
      tags$ul(
        tags$li("Job mismatch is especially concerning for young workers, with evidence suggesting that 80 per cent of career earnings growth occurs in the first decade of work (Murphy and Welch 1990) and the ability to switch jobs to `find the right fit’ is important in generating faster earnings growth."),
        tags$li("The share of young workers that report being mismatched has not changed much over recent years and the COVID-19 recession had limited effects on mismatch rates.")
      )
    ),
  ),

  fluidRow(
    ### Job switching for mismatched workers ####
    column(
      width = 7,
      class = "m-2",
      plotlyOutput("jobswitchers"),
      p(
        "Source: ABS Labour Force microdata",
        class = "source-text"
      ),
      radioButtons(
        "jobswitch_age",
        "Select age group: ",
        choices = unique(df_jobswitchers$age_bucket),
        selected = "15-24",
        inline = TRUE
      )
    ),
    column(
      width = 4,
      class = "m-2",
      h6("The rate of 'helpful’ job transitions for mismatched workers is steady"),
      tags$ul(
        tags$li("Higher rates of job switching are particularly important for young workers in the wrong jobs (mismatched workers). The pandemic initially caused a sharp decline in the rate of job-switching for mismatched workers, though the rate of switching has recovered since then.")
      )
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
        p("Source: MADIP",
          class = "source-text"),
        div(
          radioButtons(
            "age_gp", "Select age groups:",
            choices = unique(df_occupation$age),
            selected = "Under 18",
            inline = TRUE
            )
          ),
        class = "card-body"
      ),
      class = "m-2",
      style = ""
    ),
    column(
      width = 4,
      class = "m-2",
      h6("Young people increasingly work in lower-skilled services roles"),
      tags$ul(
        tags$li("Employment opportunities for young people are primarily in services, specifically in hospitality, food preparation and sales assistant roles. The share of young people in these roles is highest in younger age groups, reflecting young people taking up these jobs part-time alongside further education or as their first jobs after completing secondary education."),
        tags$li("For 23-25 year olds, the share of employment in these roles is smaller, reflecting a larger share of this cohort having completed tertiary education and working in roles requiring post-secondary qualifications. However, in the lead-up to the pandemic, the share of 23-25 year olds still working in hospitality or sales has been increasing, potentially driven by a larger share remaining in tertiary education or worse employment prospects in other industries.")
      )
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
          p("Source: ABS Labour Force, Detailed",
            class = "source-text"),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                "age_dur_1",
                "Select first age group:",
                choices = unique(df_duration$age_group),
                selected = "15-24 years",
                inline = TRUE
              )
            ),
            column(
              width = 6,
              radioButtons(
                "age_dur_2",
                "Select second age group:",
                choices = unique(df_duration$age_group),
                selected = "Total",
                inline = TRUE
              )
            ),
            class = "card-body"
          )
        ),
      ), ),
      fluidRow(column(
        width = 6,
        class = "m-2",
        h6("There is some evidence of a growing share of young people that have been out of work for a lengthy period of time"),
        tags$ul(
          tags$li("Since the pandemic there has been a significant increase in the share of young workers that have been unemployed for more than a year. The pandemic has exacerbated the long-term youth unemployment problem that has been apparent since the GFC.")
        )
      )),
      ### U/E duration vs rate ####
      fluidRow(
        column(
          width = 7,
          class = "m-2",
          plotlyOutput("duration_v_ue"),
          p("Source: ABS Labour Force microdata",
            class = "source-text"),
          sliderTextInput(
            "dur_v_ue_year_1",
            "Select first year: ",
            choices = unique(df_duration_v_ue$year),
            selected = min(df_duration_v_ue$year)
          ),
          sliderTextInput(
            "dur_v_ue_year_2",
            "Select second year: ",
            choices = unique(df_duration_v_ue$year),
            selected = max(df_duration_v_ue$year)
          ),
          radioButtons(
            "dur_v_ue_age",
            "Select age group: ",
            choices = unique(df_duration_v_ue$age_bucket),
            selected = "15-24",
            inline = TRUE
          )
        ),
        column(
          width = 4,
          class = "m-2",
          h6("The regions of Australia with the highest rates of unemployment are also those in which people are spending the most time out of work"),
          tags$ul(
            tags$li("The positive correlation between unemployment rates and unemployment duration across regions indicates that some regions are particularly susceptible to long-term scarring effects from the pandemic-induced recession.")
          )
        )
      ),

      fluidRow(
        ### U/E regaining employment ####
        column(
          width = 7,
          class = "m-2",
          plotlyOutput("pc_ue_gained"),
          p("Source: ABS Labour Force microdata",
            class = "source-text"),
          radioButtons(
            "ue_to_emp_measure",
            "Select measure: ",
            choices = unique(df_ue_gained$measure),
            selected = "Duration of job search",
            inline = TRUE
          ),
          radioButtons(
            "ue_to_emp_age_bucket",
            "Select age group: ",
            choices = unique(df_ue_gained$age_bucket),
            selected = "15-24",
            inline = TRUE
          )
        ),
        column(
          width = 4,
          class = "m-2",
          h6("The long-term unemployed face greater difficulty finding work"),
          tags$ul(
            tags$li("This graph shows the proportion of unemployed individuals who managed to get a job over time. There are three key areas where there is a risk that job finding may have become more difficult: for those in regional areas, for those who have been searching for work for a long time, and for those who have never worked."),
            tags$li("There are concerns that labour markets in regional areas are weaker than in cities. However, Tthe share of unemployed workers finding employment in a given month is broadly similar across urban and regional areas, with unemployed workers in urban and regional areas benefiting from the strong labour market recovery."),
            tags$li("Workers who have been searching for work for more than six months face greater difficulty transitioning back into employment. Similarly, people who have never worked or have not worked in over 6 months, also have lower probabilities of entering employment compared to people who have worked more recently. These trends have remained consistent over the past 15 years.")
          )
        )
      ),
      div(
        class = "m-2",
        br(),
        h5("Relative intensity of Jobseeker recipients share for 18-24 year olds")
      ),
      fluidRow(
        ### JS relative intensity from illion ####
        column(width = 7, class = "m-2",
               leafletOutput("js_map")),
        column(
          width = 4,
          class = "m-2",
          h6("Young Jobseeker recipients are highly concentrated in disadvantaged regions"),
          tags$ul(
            tags$li("The share of young people that are receiving JobSeeker payments is concentrated in areas associated with greater disadvantage, such as outer suburban and regional Australia."),
            tags$li("This map uses illion data to estimate the relative shares of Jobseeker and Youth Allowance (for job seekers) payments in regions across Australia. The data are presented as an index between 0 and 100, with higher numbers indicating a greater relative share of the population in that region are on support payments compared to the rest of Australia.")
          )
        )
      ),
      div(class = "m-2",
          p("Source: illion", class = "source-text")),
      fluidRow(column(
        width = 6,
        class = "m-2",
        radioButtons(
          "age_js",
          "Select age group: ",
          choices = unique(df_js$age_bucket),
          selected = "18-24",
          inline = TRUE
        ),
        sliderTextInput(
          "timeline_js",
          "Select date: ",
          choices = seq(min(df_js$date), max(df_js$date), by = "months"),
          selected = min(df_js$date),
          animate = animationOptions(interval = 1000, loop = FALSE)
          # Note that animation needs to be fixed - it currently causes the map to reload, which takes too much time
        )
      ),
      ),
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
          div(plotlyOutput("youth_unem"),
              br(),
              p("Source: ABS Labour Force, Detailed",
                class = "source-text"),
              ),
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
          tags$ul(
            tags$li("Young Australians living in areas with greater household disadvantage tend to have more difficulty finding employment, with unemployment rates in these areas higher than in more advantaged areas. These disadvantaged areas tend to be clustered in regional Australia or the outer rings of the capital cities.")
          )
        )
      ),
      div(class = "m-2",
          br(),
          h5("Unemployed share by age group over time")),
      fluidRow(
        ### Unemployed share by age group over time ####
        column(width = 7, class = "m-2",
               leafletOutput("map2"),
               ),
        column(
          width = 4,
          class = "m-2",
          h6("Unemployment is higher for youth"),
          tags$ul(
            tags$li("Across Australia, youth unemployment is higher than that of the total labour market. This trend holds in both capital cities and regional Australia, and across time.")
            )
      ),
      div(class = "m-2",
          p("Source: ABS Labour Force, Detailed", class = "source-text")),
      fluidRow(column(
        width = 6,
        class = "m-2",
        radioButtons(
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
          animate = animationOptions(interval = 1000, loop = FALSE)
          # Note that animation needs to be fixed - it currently causes the map to reload, which takes too much time
        )
      )),
      fluidRow(
        ### Employment rate by degree level and industry ####
        column(width = 7, class = "m-2",
               div(
                 div(h5(
                   "Employment rate by education level"
                 ),
                 class = "card-body-2"),
                 plotlyOutput("educ_v_emp"),
                 br(),
                 p("Source: ABS Labour Force, Detailed",
                   class = "source-text"),
                 radioButtons(
                   "age_educ_v_emp",
                   "Select age group: ",
                   choices = unique(df_educ_emp$Age),
                   selected = "15-24"
                 ),
                 radioButtons(
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
          h6("More education tends to increase employment"),
          tags$ul(
            tags$li("Across age and gender groups, higher levels of education are associated with higher employment rates. People who have not completed high school have the lowest employment rates, whereas people who have completed Bachelor-level or higher qualifications have the strongest rates of employment."),
            tags$li("Note that the employment rates for 15-24 year old postgraduates is highly variable due to a small sample and the fact that many people in this age group would still be in education rather than having completed postgraduate qualifications.")
          )
        )
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
      h3("Youth not in employment, education or training living in disadvantaged areas are a concern"),
      br(),
      fluidRow(
        ### Youth NEET by age/demo time series ####
        column(
          width = 7,
          class = "m-2",
          div(plotlyOutput("neet_timeseries"),
              br(),
              p("Source: ABS Labour Force, Detailed",
                class = "source-text")
              ),
          fluidRow(column(
            width = 10,
            radioButtons(
              "neet_dem",
              "Select demographic: ",
              choices = c("Total", "Gender"),
              selected = "Total",
              inline = TRUE
            ),
            column(
              width = 10,
              radioButtons(
                "neet_age",
                "Select age group: ",
                choices = c("15-24 years", "15-19 years", "20-24 years"),
                selected = "15-24 years",
                inline = TRUE
              )
            ))
          )
        ),
        column(
          width = 4,
          class = "m-2",
          h6("The share of young men that are not working or studying full-time has been elevated since the start of the pandemic"),
          tags$ul(
            tags$li("The share of youth that are not working or in full-time study increased sharply during the pandemic and remains somewhat elevated, most notably for men aged 20-24 years."),
            tags$li("This graph plots the percentage of young people who are currently not in employment, education or training (NEET) in each month, breaking it down by gender and age group.  The NEET rate (percentage of individuals in NEET out of the population) is a key indicator of engagement in the labour market, tending to grow during economic downturns such as the GFC.")
          )
        ),
        fluidRow(
          ### NEET entry and exit rates ####
          column(
            width = 7,
            class = "m-2",
            div(plotlyOutput("neet_entry_exit"),
                br(),
                p("Source: ABS Labour Force, Detailed",
                  class = "source-text")),
            fluidRow(column(
              width = 6,
              radioButtons(
                "neet_entry_exit_dem",
                "Select demographic: ",
                choices = unique(df_neet_2$demo_split),
                selected = "Total",
                inline = TRUE
              )
            )),
          ),

          column(
            width = 4,
            class = "m-2",
            h6("The COVID-19 recession drove a large increase in NEET"),
            tags$ul(
              tags$li("The increase in the share of young people that are NEET is due to both more people becoming NEET and more people staying NEET."),
              tags$li("This graph breaks down the monthly entry and exit from NEET status by gender and geographic area as a proportion of the population. The NEET flow rate represents the difference between NEET entry and exit."),
              tags$li("The flow of people into NEET increased sharply in 2020, driven by the COVID-19 recession. The subsequent rapid recovery in the labour market saw a large flow out of NEET status. This flow was common across capital cities and regions, and across both males and females.")
            )
        )
        ),


        fluidRow(
          ### Pr(NEET) by distance from CC ####
          column(
            width = 7,
            class = "m-2",
            plotlyOutput("neet_distance"),
            br(),
            p("Source: HILDA Release 20",
              class = "source-text"),
          ),
          column(
            width = 4,
            class = "m-2",
            h6("Young people living in regional areas are increasingly not studying or working"),
            tags$ul(
              tags$li("The probability of a young Australian being NEET increases with distance from Australia's capital cities. Furthermore, this difference has increased through time. This suggests that economic opportunities are concentrated in capital cities, and young people from disadvantaged backgrounds living in regional areas are being increasingly left behind in the labour market recovery.")
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
      h3("Opportunities to limit scarring effects"),
      fluidRow(
        ### Map: Change in jobs by industry/location ####
        column(
          width = 7,
          class = "m-2",
          div(
            h5("Net change in jobs by industry and location"),
            p(em("Change in jobs per 1000 workers between 2002 - 2021")),
            selectInput("name",
                        "Select industry",
                        unique(df_map$industry)),
            leafletOutput("map_change_jobs_industry"),
          ),
          br(),
          p(
            "Source: ABS BLADE Data Industries with less than 10 firms excluded",
            class = "source-text"
          )
        ),

        column(
          width = 4,
          class = "m-2",
          br(),
          h6("Employment opportunities vary by region and industry"),
          tags$ul(
            tags$li("Employment opportunities vary by industry and across Australia. Young people living in more disadvantaged regions may have to relocate to find opportunities that best match their interests and skills."),
            tags$li("The graph shows net changes in jobs by industry. A net increase in jobs indicates that the industry is growing in a region, while a decrease indicates a decline. The table supplements this with a gross measure that shows how many new jobs were created in an industry, without taking into account job destruction."),
            tags$li("The most common occupation and industries for young people (under 25 years) tend to be in hospitality, sales and carers positions. This is true across most regions, and these industries make up a larger share of employment. In contrast, the most common occupations and industry for older workers vary much more across regions and are less concentrated.")
          )
        ),
      ),
      ### Table: Change in jobs by industry/location ####
      fluidRow(
               column(
                 width = 8,
                 class = "m-2",
                 div(
                   h5("Table: change in jobs by industry and location"),
                   p(em("Change in jobs per 1000 workers between 2002 - 2021")),
                   selectInput("name_area",
                               "Select location",
                               unique(df_map$sa3_name_16)),
                   dataTableOutput("change_jobs_industry_table")
                 ),
                 br(),
                 p("Source: BLADE Data Industries with less than 10 firms excluded",
                   class = "source-text")
               ),),
      ### Top 3 youth occupations ####
      fluidRow(
               column(
                 width = 8,
                 class = "m-2",
                 div(
                   h5("Top 3 occupations worked by Youth (19-29) in region"),
                   p(em("SA3 level")),
                   leafletOutput("area_occupation"),
                   radioButtons(
                     "age_area",
                     "Select age group:",
                     unique(df_occupation_area$age),
                     selected = "Under 25",
                     inline = TRUE
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
  
    ue_graph <-
      df_unemp %>% filter(measure == input$agg_measure,
                          age_group %in% input$agg_ages) %>%
      plot_ly(
        x = ~ date,
        y = ~ value,
        split = ~ age_group,
        type = "scatter",
        mode = "lines",
        fill = ~""
      ) %>% 
      rangeslider(start = min(df_unemp$date), end = max(df_unemp$date))
    
    ue_graph <- ue_graph %>% layout(
      showlegend = TRUE,
      title = "Measures of employment by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = input$agg_measure, zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )
  

  })
  
  ## Section 2 ####
  
  ### Job mobility by age group ####
  output$job_mobility <- renderPlotly({
    
    req(input$ages_jm)
    
    jm_graph <-
      df_job_mobility %>% filter(age_group == input$ages_jm) %>%
      plot_ly(
        x = ~ date,
        y = ~ value,
        split = ~ age_group,
        type = "scatter",
        mode = "lines",
        fill = ~""
      )
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Job mobility by age group",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "Job mobility", zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
  })
  
  ### Mismatched young workers ####
  output$pc_mismatched <- renderPlotly({
    
    pc_mismatched <- df_pc_mismatched %>%
      filter(age_bucket == input$mismatched_age) %>% 
      plot_ly(
        x = ~ date,
        y = ~ share,
        color = ~skill_level,
        colors = c("#1b9e77", "#d95f02", "#7570b3"),
        type = "scatter",
        mode = "lines",
        fill = ~""
      )
    
    pc_mismatched <- pc_mismatched %>% layout(
      title = "Share of employed people by skill matching status",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "% of workers", zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  ### Job switching for mismatched workers ####
  output$jobswitchers <- renderPlotly({
    
    jobswitchers <-
      df_jobswitchers %>% filter(age_bucket == input$jobswitch_age) %>%
      plot_ly(
        x = ~ date,
        y = ~ prop,
        color = ~ matched_last_period,
        colors = c("#1b9e77", "#d95f02"),
        type = "scatter",
        mode = "lines",
        fill = ~""
      ) %>%
      rangeslider(start = min(df_jobswitchers$date), end = max(df_jobswitchers$date))
    
    jobswitchers <- jobswitchers %>% layout(
      title = "Job switching for workers with mismatched employment in previous 12 months",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "% workers", zeroline = FALSE, showgrid = FALSE, ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
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
      plot_ly(
        x = ~ year,
        y = ~ percent_total,
        color = ~ two_name,
        type = "scatter",
        mode = "lines",
        fill = ~""
      )
    
    jm_graph <- jm_graph %>% layout(
      showlegend = TRUE,
      title = "Top 5 occupations by age group",
      xaxis = list(title = "Year", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "Percent Total", zeroline = FALSE, showgrid = FALSE, ticksuffix = "%", hoverformat = ".2f"),
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
                              measure == "More than 1 year ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "More than 1 year ago",
                name = ~measure, showlegend = FALSE, fillcolor = "#fde725") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "3 months to 1 year ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3 months to 1 year ago",
                name = ~measure, showlegend = FALSE, fillcolor = "#21918c") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Less than 3 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Less than 3 months ago",
                name = ~measure, showlegend = FALSE, fillcolor = "#3b528b") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_1,
                              measure == "Never worked before"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Never worked before",
                name = ~measure, showlegend = FALSE, fillcolor = "#440154")
    
    
    dur_graph_1 <- dur_graph_1 %>% layout(
      
      title = "Duration of unemployment by age group",
      xaxis = list(title = input$age_dur_1, zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "Share of unemployed", zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      paper_bgcolor = chart_bg_color,
      font = list(color = chart_text_color)
    )

    
    dur_graph_2 <- df_duration %>% filter(age_group == input$age_dur_2) %>% 
      plot_ly(x = ~date) %>% 
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "More than 1 year ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "More than 1 year ago",
                name = ~measure, showlegend = TRUE, fillcolor = "#fde725") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "3 months to 1 year ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "3 months to 1 year ago",
                name = ~measure, showlegend = TRUE, fillcolor = "#21918c") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "Less than 3 months ago"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Less than 3 months ago",
                name = ~measure, showlegend = TRUE, fillcolor = "#3b528b") %>%
      add_trace(data = filter(df_duration, age_group == input$age_dur_2,
                              measure == "Never worked before"), 
                x = ~date, y = ~share_of_ue, type = "scatter", mode = "none", 
                stackgroup = "one", legendgroup = "Never worked before",
                name = ~measure, showlegend = TRUE, fillcolor = "#440154")
    

    dur_graph_2 <- dur_graph_2 %>% layout(
      title = "Duration of unemployment by age group",
      xaxis = list(title = input$age_dur_2, zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "Share of unemployed persons", zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color),
      legend = list(title = list(text = "Time since last employed"), traceorder = "reversed")
    )
    
    dur_graph <- subplot(dur_graph_1, dur_graph_2, titleX = TRUE, shareY = TRUE, 
                         margin = 0.02)
    
  })
  
  ### U/E duration vs rate ####
  output$duration_v_ue <- renderPlotly({
    
    duration_v_ue <-
      df_duration_v_ue %>% 
      filter(
        year %in% c(input$dur_v_ue_year_1, input$dur_v_ue_year_2) & 
          age_bucket == input$dur_v_ue_age) %>%
      plot_ly(x = ~ ue) %>% 
      add_trace(
        x = ~ ue,
        y = ~ duration,
        split = ~year,
        size = ~pop,
        text = ~ sprintf("%s (%s %s)", 
                         sa4_name, lubridate::month(date, label = TRUE), year(date)),
        name = ~year,
        hoverinfo = "text",
        type = "scatter",
        mode = "markers",
        fill = ~""
        ) %>% 
      add_lines(x = ~ue, y = ~fitted, split = ~year, name = ~year)

    duration_v_ue <- duration_v_ue %>% layout(
      title = "Median unemployment duration v unemployment rate",
      xaxis = list(
        title = "Unemployment rate",
        zeroline = FALSE,
        showgrid = FALSE,
        ticksuffix = "%",
        hoverformat = ".2f"
      ),
      yaxis = list(
        title = "Median duration unemployed (months)",
        zeroline = FALSE,
        showgrid = FALSE,
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
    
    pc_ue_gained <- df_ue_gained %>%
      filter(measure == input$ue_to_emp_measure & age_bucket == input$ue_to_emp_age_bucket) %>%
      plot_ly(
        x = ~ date,
        y = ~ value,
        color = ~ variable,
        colors = c("#1b9e77", "#d95f02", "#7570b3"),
        type = "scatter",
        mode = "lines",
        connectgaps = TRUE,
        fill = ~""
      ) %>%
      rangeslider(start = min(df_ue_gained$date),
                  end = max(df_ue_gained$date))
    
    pc_ue_gained <- pc_ue_gained %>% layout(
      title = paste0("% unemployed who gained employment by ", input$ue_to_emp_measure),
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "% unemployed", zeroline = FALSE, showgrid = FALSE, ticksuffix = "%", hoverformat = ".2f"),
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
      ) %>% 
      addFullscreenControl()
    
  })
  
  ## Section 4 ####
  

  ### U/E rate vs IHAD disadvantage ####
  output$youth_unem <- renderPlotly({
    
    df_youth_unem$total <- as.numeric(df_youth_unem$total)
    df_youth_unem$date <- as.character(df_youth_unem$date)
   
    
    youth_unem_graph <- df_youth_unem %>% 
      filter(age %in% input$age_youth_unem, sex == input$sex_youth_unem) %>%
      rename(Date = date) %>% 
      plot_ly(x = ~share_decile_1) %>% 
      add_trace(x = ~share_decile_1, y = ~ue_rate, 
                split = ~age,
                type = "scatter",
                size = ~total,
                mode = "markers", 
                frame = ~Date,
                fill = ~"") %>% 
      add_lines(x = ~share_decile_1, y = ~fv, split = ~age, frame = ~Date, name = "Trendline")
    
    youth_unem_graph <- youth_unem_graph %>% layout(
      showlegend = TRUE,
      title = "Unemployment rate against share of households who are disadvantaged",
      xaxis = list(title = "Share of households in bottom decile of disadvantage", 
                   zeroline = FALSE, showgrid = FALSE, ticksuffix = "%", margin = list(b = 100)),
      yaxis = list(title = "Unemployment rate", zeroline = FALSE, showgrid = FALSE,
                   ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = TRUE),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
    
    youth_unem_graph <- youth_unem_graph %>% 
      animation_opts(
        frame = 200, transition = 200,  easing = "linear", redraw = FALSE
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
      ) %>% 
      addFullscreenControl()
    
  })
  
  ### Employment rate by degree level and industry ####
  
  output$educ_v_emp <- renderPlotly({
    
    educ_emp <- df_educ_emp %>% 
      filter(Age == input$age_educ_v_emp,
             Sex == input$sex_educ_v_emp) %>%
      plot_ly(
        x = ~ Date,
        y = ~ Employment,
        color = ~ Education,
        type = "scatter",
        mode = "lines",
        fill = ~""
      )

    educ_emp <- educ_emp %>% layout(
      title = "Employment rates by age and education level",
      xaxis = list(title = "Date", zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "% population employed", zeroline = FALSE, showgrid = FALSE, ticksuffix = "%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  

  ## Section 5 ####
  
  ### Youth NEET by age/demo time series ####
  output$neet_timeseries <- renderPlotly({
    
    if(input$neet_dem == "Total"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 15-19 years`,
                                       type = "scatter", mode = "lines", fill = ~"")
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 20-24 years`, 
                                       type = "scatter", mode = "lines", fill = ~"")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, y = ~`Total 15-24 years`, 
                                       type = "scatter", mode = "lines", fill = ~"")
      }
    }
    else if (input$neet_dem == "Gender"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") %>% 
          add_trace(y = ~`Males 15-19 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-19 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") %>% 
          add_trace(y = ~`Males 20-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 20-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") %>% 
          add_trace(y = ~`Males 15-24 years`, type = "scatter", mode = "lines", name = "Males") %>% 
          add_trace(y = ~`Females 15-24 years`, type = "scatter", mode = "lines", name = "Females")
      }
    }
    else if (input$neet_dem == "Education"){
      if(input$neet_age == "15-19 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") # note there is no data for this combo
      }
      else if (input$neet_age == "20-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") %>% 
          add_trace(y = ~`Year 9 or below/Never attended school 20-24 years`, type = "scatter", mode = "lines", name = "Year 9 or below") %>% 
          add_trace(y = ~`Year 10 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 10 or equivalent") %>% 
          add_trace(y = ~`Year 11 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 11 or equivalent") %>% 
          add_trace(y = ~`Year 12 or equivalent 20-24 years`, type = "scatter", mode = "lines", name = "Year 12 or equivalent")
      }
      else if (input$neet_age == "15-24 years"){
        neet_timeseries <- df_neet %>% plot_ly(x = ~date, fill = ~"") # note there is no data for this combo
      }
    }

    neet_timeseries <- neet_timeseries %>% 
      rangeslider(start = min(df_neet$date), end = max(df_neet$date))
    
    neet_timeseries <- neet_timeseries %>% layout(
      title = "Youth NEET rate by age and demographic group",
      xaxis = list(title = "Date", 
                   zeroline = FALSE, showgrid = FALSE, margin = list(b = 100)),
      yaxis = list(title = "NEET rate", zeroline = FALSE, showgrid = FALSE,
                   tickformat = "1%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = TRUE),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor = chart_bg_color,
      font = list(color = chart_text_color))
    
  })
  
  ### NEET entry and exit rates ####
  output$neet_entry_exit <- renderPlotly({
    
    df_neet_2$exit <- -df_neet_2$exit
    
    neet_entry_exit <-
      df_neet_2 %>% filter(demo_split == input$neet_entry_exit_dem) %>%
      plot_ly(
        x = ~ date,
        y = ~ neet_flow,
        type = "scatter",
        mode = "lines",
        name = "NEET flow", 
        fill = ~""
      ) %>%
      rangeslider(start = min(df_neet_2$date), end = max(df_neet_2$date))
    
    neet_entry_exit <- neet_entry_exit %>% 
      add_trace(x = ~date, y = ~entry, type = 'scatter', fill = 'tozeroy', name = "Entries") %>% 
      add_trace(x = ~date, y = ~exit, type = 'scatter', fill = 'tozeroy', name = "Exits") 
    
    neet_entry_exit <- neet_entry_exit %>% layout(
      
      title = "NEET entry and exit rates",
      xaxis = list(title = "Date", 
                   zeroline = FALSE, showgrid = FALSE, margin = list(b = 100)),
      yaxis = list(title = "NEET rate", zeroline = FALSE, showgrid = FALSE,
                   tickformat = "1%", hoverformat = ".2f"),
      margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = TRUE),
      paper_bgcolor = chart_bg_color,
      plot_bgcolor= chart_bg_color,
      font = list(color = chart_text_color))
     
  })
  
  ### Pr(NEET) by distance from CC ####
  output$neet_distance <- renderPlotly({
    
    p <- df_neet_distance %>%
      mutate(label = sprintf("Current year: %s\nComparison year: %s\nProbability: %s", frame, Year, round(pred, 2))) %>% 
      arrange(mindistance) %>%
      ggplot(aes(
        x = mindistance,
        y = pred,
        color = Year
      )) +
      geom_line(aes(
        frame = frame,
        label = label
      )) + 
      scale_colour_manual(values = heat.colors(18))+
      theme(legend.position = "none")
    
    neet_distance <- ggplotly(p, tooltip = "label") %>% 
      layout(
        title = "Probability of NEET over distance (18-24)",
        xaxis = list(title = "Log distance from nearest capital city", 
                     zeroline = FALSE, showgrid = FALSE, range = list(1, 6),
                     hoverformat = ".2f"),
        yaxis = list(title = "Probability of NEET status", zeroline = FALSE, 
                     showgrid = FALSE, tickformat = "1%", dtick = 0.02, hoverformat = ".2f"),
        margin = list(l = 70, r = 50, t = 50, b = 100, autoexpand = TRUE),
        paper_bgcolor = chart_bg_color,
        plot_bgcolor= chart_bg_color,
        font = list(color = chart_text_color)
        ) %>% 
      animation_opts(
        frame = 200, transition = 200,  easing = "linear", redraw = FALSE
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
      ) %>% 
      addFullscreenControl()
    
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
      ) %>% 
      addFullscreenControl()
    
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
    searching = FALSE,
    paging = FALSE,
    scrollY = 200,
    scrollCollapse = TRUE,
    fixedHeader = TRUE
  ))
 }

shinyApp(ui, server)
