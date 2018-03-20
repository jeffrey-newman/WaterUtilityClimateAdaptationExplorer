#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ODB)
library(purrr)
decision_database_path <- '/home/a1091793/Developer/WaterUtilityClimateAdaptationDecisionDatabaseExplorer/WaterUtilityClimateAdaptationDecisionDatabase.odb'
core_area <- 'Natural environments'
odb <- odb.open(decision_database_path)
receivers_query <- paste("SELECT \"Class type\" FROM \"Vulnerabilities\" WHERE \"Core area\" = '", core_area, "'", sep="")
odb.queries(odb,"receivers_query") = receivers_query
receivers_list <- unique(odb.read(odb, odb.queries(odb, "receivers_query")))
names(receivers_list) <- NULL

climatePressures <- function(odb, receivers_list)
{
    climate_pressures_query <- paste("SELECT \"Class type\" FROM \"Vulnerabilities\" WHERE \"Core area\" = '", core_area, "'", sep="")
    map(receivers_list, {function(receiver) return(paste("AND \"Class type\" = 'Waterway health'"))})
}

source_water_impact_receivers <- c("Surface water", "Stormwater", "Rainwater")
impact_receivers_choice <- "Surface water"
surface_water_impacts <- c("Change in yield", "Change in peak demand", "Competition with environmental water")
stormwater_impacts <- c("Change in yield", "Change in peak demand")
rainwater_impacts <- c("Change in yield", "Change in peak demand")
impacts_list <- surface_water_impacts

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Approaches, models and data explorer:\n Water utility source waters"),
   
   tabsetPanel(
     id = "main_panel",
     tabPanel(
       "Receiver of impact",
       checkboxGroupInput(
         "receiver_select", 
         "Select the climate impact receivers you are assessing", 
         choices=as.list(receivers_list[1]),
         width = '100%'
         ),
       # A text output for debugging.  Comment out following line when releasing
       verbatimTextOutput("receiver_choice_text"),
       actionButton("receiver_next", "Next")
     ),
     tabPanel(
       "Climate vulnerability/impact",
       # The impacts tab will allow the user to select the climate change impacts to the utility that they are assessing. 
       # These are termed vulnerabilities in Appendix A of the WSAA Guidleines.
       checkboxGroupInput(
         "impact_select",
         "Select what climate vulnerabilities/impacts you are assessing",
         choices = impacts_list
       ),
       # A text output for debugging.  Comment out following line when releasing
       verbatimTextOutput("impact_choice_text"),
       actionButton("impacts_next", "Next")
     ),
     tabPanel(
       "Climate pressures"
     ),
     tabPanel(
       "Level of detail",
       actionButton("scan", "Scan level"),
       actionButton("delve_deeper", "Delve deeper")
     ),
     tabPanel(
       "Identified approaches, methods and data",
       # Sidebar with a slider input for number of bins 
       sidebarLayout(
         sidebarPanel(
           sliderInput("bins",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30)
         ),
         
         # Show a plot of the generated distribution
         mainPanel(
           plotOutput("distPlot")
         )
       )
     )
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # observe({
  #   x <- input$receiver_select
  #   
  #   # Can use character(0) to remove all choices
  #   if (is.null(x))
  #     x <- character(0)
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "impact_select",
  #                     label = "Select what climate vulnerabilities/impacts you are assessing",
  #                     choices = x,
  #                     selected = tail(x, 1)
  #   )
  # })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   impact_receivers_choice <- observeEvent(input$receiver_next, {
     if(input$receiver_select[1] == "Surface water") impacts_list <- surface_water_impacts
     if(input$receiver_select[1] == "Stormwater") impacts_list <- stormwater_impacts
     if(input$receiver_select[1] == "Rainwater") impacts_list <- rainwater_impacts
     updateCheckboxGroupInput(session, "impact_select", choices = impacts_list)
     output$receiver_choice_text <- renderText(impacts_list)
     updateTabsetPanel(session,"main_panel","Climate vulnerability/impact")
     })
   
   # output$receiver_choice_text <- renderText(impact_receivers_choice)
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

