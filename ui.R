  ### Shiny app for household Projections ---
  # based on Vicky population app
  # update with better comments as version of packages
########################.  
  ##Packages ----
########################.
require(shiny)
require(shinydashboard)
require(tidyverse)
library(dplyr)
library(tidyr) 
library(ggplot2)
require(stringr)
require(ggrepel)
library(magrittr)
########################.  
  ##Datasets ----
########################.

load("projection2016.RData")
#load("dummies.RData")
  
list_of_places = as.list(as.character(unique(data1$areacode)))
list_of_variants = as.list(as.character(unique(data1$variant)))
list("Absolute change", "Percentage change")
list_of_age_group = as.list(as.character(unique(data3$agegroup)))

########################.  
  ##Common Features ----
########################.
g1<-"#536f1b"
g2<-"#bac5a3"
theme_set(theme_minimal(base_size = 16))

########################.  
  ##User Interface ----
########################.

ui <- dashboardPage(
  
  title="Household projections of Scotland - National Records of Scotland", 
  dashboardHeader(title = tags$a(href='http://www.nrscotland.gov.uk',
                                 tags$img(height="45", alt="NRS", src="logo.png"),
                                 tags$script(HTML('
                                 $(document).ready(function() {
                                 $("header").find("nav").append(\'<span class="myClass"> Household Projections for Scotland, 2016-based </span>\');
                                 })
                                                  ')),
      tags$head(tags$style(HTML(
       '.myClass { 
       font-size: 17px;
       line-height: 50px;
       text-align: left;
       padding: 0 15px;
       overflow: hidden;
       color: white;
       font-family: "Roboto", sans-serif !important; font-weight:400;
       }
       ')),
       HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Roboto: 100,200,300,400,500,900">')
       ),
       HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Open+Sans:400,700,800italic|Roboto: 100,200,300, 400,500,900|Oswald:400,300|Lato:400,100">')
       )),
        dashboardSidebar(
   sidebarMenu(
     menuItem("Introduction", tabName = "tab0", icon = icon("th")),
     #here add name of the menu items     
     menuItem("Projection variants", tabName = "tab1", icon = icon("line-chart")),  
     menuItem("Household size", tabName = "tab2", icon = icon("bar-chart")),
     menuItem("Age of head of household", tabName = "tab3", icon = icon("line-chart")),
     menuItem("More information", tabName = "tab4", icon = icon("info"))
     )
 ),
 
 # 
 dashboardBody(
   
   HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
   HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
   tags$script(src = "cookie_control_config.js"),
   
   #adding css file
   tags$head(
   includeCSS("style.css")
   ),
   tabItems(
########################.  
 ##Introduction (tab0) ----
########################.
tabItem(tabName = "tab0",
        fluidPage(
        titlePanel("Introduction"),
        br(),
        h4("This interactive visualisation shows the", strong("projected number of households"), "for Scottish Council Areas from 2016 to 2041. The total figures can be broken down and compared at local authority level."),
        br(),
        h4("Under the principal projection the number of households is projected to increase to 2,76 million by 2041. An increase of 317,000 (13%) households over the next 25 years."),
        br(),
        h4("The largest projected percentage increases are in Midlothian (+36%), East Lothian (+26%) and the City of Edinburgh (+26%)."),
        br(),
        h4("Household numbers are projected to fall in just four council areas: Na h-Eileanan Siar(-6%), Inverclyde(-5%), Argyll and Bute (-2%) and North Ayrshire (-1%)."),
        br()
         )),
########################.  
##PROJECTION vARIANTS (tab1) ----
########################.
    tabItem(tabName = "tab1",
      fluidPage(
      titlePanel("Projection variants"),
      fluidRow(
# Side menu 
    column(3,
    wellPanel(
    helpText("Please select options for chart"),
             br(),
    selectInput(inputId="Area1", label="Select area:",
                choices=list(" "=list_of_places[26], "Scottish areas"=list_of_places[-26]), 
                selected="Scotland", 
                selectize=TRUE,  multiple=TRUE),
             br(),
             br(),
      sliderInput(inputId="end_year1",
      "Choose year (line):",
      value = max(data1$year),
      min = min(data1$year),
      max = max(data1$year),
      step=1, sep = ""),
      br(),
      radioButtons(inputId = "Type1", label = "Type of change",
      choiceNames = list("Absolute change", "Percentage change"), 
      choiceValues = list("Absolute change", "Percentage change"),
      selected="Percentage change"),
     br(),
     checkboxGroupInput(inputId="Variants1", 
     label = "Projection variants",
     choices=list("Principal (solid line)"="Principal", "Low migration (dot line)"="Low Migration", "High migration (dash line)"="High migration"), 
     selected = "Principal"))
     ),
    # - Plot ---------------------------------------------------------------
    column(9, 
    wellPanel(
    plotOutput("my_plot1"))
      ))
        
    )),
# - Tab 2 ----------------------------------------------------------------------
    tabItem(tabName = "tab2",
      fluidPage(
        titlePanel("Household size"),
        fluidRow(
        column(3,
        wellPanel(
        selectInput(inputId="Area2", label="Select area:",
                    choices=list(" "=list_of_places[26], "Scottish areas"=list_of_places[-26]), 
                    selected="Scotland", 
                    selectize=TRUE, multiple=TRUE),
        br(),
        sliderInput(inputId="end_year2",
       "Choose year:",
       value = max(data2$year),
       min = min(data2$year),
       max = max(data2$year),
       step=1,
       sep = ""),
       br(),
       br(),
       br() 
          )), #end column4
          column(9,
            wellPanel(
              plotOutput("my_plot2a"),
              br(),
              plotOutput("my_plot2b")
              #,
              #br(),
              #br(),
              #DT::dataTableOutput("dat1")
        ))
      ))), # tab2 end 

# - Tab 3 ----------------------------------------------------------------------
      tabItem(tabName = "tab3",
        fluidPage(
          titlePanel("Age of head of household"),
          
          fluidRow(
            column(3,helpText("Please select options for chart"),
                   br(),
                   selectInput(inputId="Area3", label="Select area:",
                               choices=list(" "=list_of_places[26], "Scottish areas"=list_of_places[-26]), 
                               selected="Scotland", 
                               selectize=TRUE, multiple=TRUE),
                   br(),
                   br(),
                   sliderInput(inputId="end_year3",
                               "Choose year (line):",
                               value = max(data1$year),
                               min = min(data1$year),
                               max = max(data1$year),
                               step=1, sep = ""),

                 br()
          ),
          column(9,
            wellPanel(
              plotOutput("my_plot3")
              ))
          )
        )),

## TODO: change the text -----
# - Tab 3 ----------------------------------------------------------------------
# More information on HH estimates and links to the data and to the 
tabItem(tabName = "tab4",
        fluidPage(
          titlePanel("More information"),
          fluidRow(
            column(12, 
                   br(),
                   h4("Household projections are calculations showing what may happen to the number of households using the projected population and the current number of households."),
                   br(),
                   
                   h4("The assumptions used for the population projections, such as future migration, will therefore affect the household projections."),
                   br(),
                   
                   h4("These household projections are trend-based and are not, therefore, policy-based forecasts of what the Government expects to happen. Variant projections give an idea of the uncertainty around projections, and show what might be expected to happen under different plausible assumptions about migration."), 
                   br(),
                   
                   h4("The household projections incorporate the latest", a( " population projections" , href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections", target="_blank"), "and the 2016 and 2017 ", a("household estimates." , href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates", target="_blank"), "Information from Scotland's Census 1991, 2001 and 2011 and the", a(" 2016 Scottish Household Survey", href="http://www.gov.scot/Topics/Statistics/16002", target="_blank"), "is used to project trends in the types of households that people are living in. Data from a range of sources on residents in communal establishments, e.g. care homes or prison, are also included."),
                   br(),
                   
                   h4("More information about the projections, their uses and their limitations can be found on the", a(" Household Projections page.",href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-projections", target="_blank"), "page on the NRS website."),
                   br(),
                   br()),
                  fluidRow(
                     column(4,  
                            wellPanel(
                              h6(strong("More information")),
                              h6("Data: ", a("Household projections, 2016-based",
                                             href="https://www.nrscotland.gov.uk/files//statistics/nrs-visual/hh-proj-16/2016-based-household-projections-vis-source-table.xlsx", target="_blank")),
                              h6("Publication: ", a("Household Projections, 2016-based",
                                                    href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-projections/2016-based-household-projections", target="_blank")))),
                     column(4, 
                            wellPanel(
                              br(),
                              h6("Follow us on Twitter - ", 
                                 a("@NatRecordsScot", 
                                   href="https://twitter.com/NatRecordsScot",
                                   target="_blank")),
                              h6("See more ", 
                                 a("Infographics & Visualisations", 
                                   href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations", target="_blank")))),
                     column(4, 
                            wellPanel(
                              h6(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", target="_blank")),
                              h6("\U00A9 Crown Copyright 2018 - ",
                                 a("Copyright conditions", 
                                   href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank")))
                     )),
                   fluidRow(
                     column(4,
                     wellPanel(
                       h4("Any feedback about this visualisation?", a("Get in touch!", href="mailto:victoria.avila@nrscotland.gov.uk?subject=Scotland Household Projections visualisation &cc=statisticscustomerservices@nrscotland.gov.uk", target="_blank")
                          
                       )))
                   )# End of fluidRow
                   )#Fluidpage
        )#tabItem
# # - Tab 3 ----------------------------------------------------------------------
# tabItem(tabName = "tab5",
#         fluidPage(
#           titlePanel("Change by age group"),
#           
#           fluidRow(
#             column(3,helpText("Please select options for chart"),
#                    br(),
#                    selectInput(inputId="Area4", label="Select area:",
#                                choices=list_of_places, selected="Scotland"),
#                    br(),
#                    br(),
#                    sliderInput(inputId="end_year4",
#                                "Choose year (line):",
#                                value = max(data1$year),
#                                min = min(data1$year),
#                                max = max(data1$year),
#                                step=1, sep = ""),
#                    
#                    br()
#             ),
#             column(9,
#                    wellPanel(
#                      plotOutput("my_plot4a")
#                    ))
#           )
#         ))


)#TabItems

 )#
 ))#end
