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
library(dplyr)
#Work Ubuntu
#decision_database_path <- '/home/a1091793/Developer/WaterUtilityClimateAdaptationDecisionDatabaseExplorer/WaterUtilityClimateAdaptationDecisionDatabase.odb'
#Home Ubuntu
decision_database_path <- '/home/a1091793/Developer/Water-Utility-Climate-Adaptation-Decision-Database/WaterUtilityClimateAdaptationDecisionDatabase.odb'
core_area <- 'Natural environments'
odb <- odb.open(decision_database_path)

# Makes a list of the entities which are impacted by climate change for a specific core area
# Returns list of these entitites
# odb - an open odb database (formal class ODB from package ODB)
# core_area - chr representation of the core area
impactReceivers <- function(core_area, odb)
{
  receivers_query <- paste("SELECT \"Class type\" FROM \"Vulnerabilities\" WHERE \"Core area\" = '", core_area, "'", sep="")
  receivers_list <- unique(odb.read(odb, receivers_query))
  return(receivers_list)
}

# Makes a list of vulnerabilities/risks that result from climate change pressures impacting on a list of entities (impact receivers)
# Returns a list of climate vulnerabilities
# odb - an open odb database (formal class ODB from package ODB)
# core_area - chr representation of the core area
# impact_receivers - the entities on which climate vulnerabilities are sought (i.e. the return from the impactReceivers function)
climateVulnerabilities <- function(impact_receivers, core_area, odb)
{
  # DEBUG prints
  # print(impact_receivers)
  # print(typeof(impact_receivers))
  # print(length(impact_receivers))
  # template example: SELECT "Impact" FROM "Vulnerabilities" WHERE ( "Core area" = 'Natural environments' AND ( "Class type" = 'Waterway health' OR "Class type" = 'Groundwater health' ) )
  vulnerabilities_query <- paste("SELECT \"Impact\" FROM \"Vulnerabilities\" WHERE ( \"Core area\" = '", core_area, "' AND ( \"Class type\" = '", impact_receivers[[1]], "' ", sep="")
  for (receiver in impact_receivers[-1])
  {
    vulnerabilities_query <- paste(vulnerabilities_query, "OR \"Class type\" = '", receiver, "' ", sep="")
  }
  # DEBUG prints
  # print(vulnerabilities_query)
  vulnerabilities_query <- paste(vulnerabilities_query, ") )", sep="")
  vulnerabilities_list <- unique(odb.read(odb, vulnerabilities_query))
  return(vulnerabilities_list)
}

# Makes a list of vulnerabilities/risks that result from climate change pressures impacting on a list of entities (impact receivers)
# Returns a list of climate vulnerabilities
# odb - an open odb database (formal class ODB from package ODB)
# core_area - chr representation of the core area
# impact_receivers - the entities on which climate vulnerabilities are sought (i.e. the return from the impactReceivers function)
climatePressures <- function(impacts, impact_receivers, core_area, odb)
{
  pressures_query <- paste("SELECT * FROM \"Vulnerabilities\" WHERE ( \"Core area\" = '", core_area, "' AND ( \"Class type\" = '", impact_receivers[[1]], "' ", sep="")
  for (receiver in impact_receivers[-1])
  {
    pressures_query <- paste(pressures_query, "OR \"Class type\" = '", receiver, "' ", sep="")
  }
  pressures_query <- paste(pressures_query, ") AND ( \"Impact\" = '", impacts[[1]], "' ", sep="")
  for (impact in impacts[-1])
  {
    pressures_query <- paste(pressures_query, "OR \"Impact\" = '", impact, "' ", sep="")
  }
  pressures_query <- paste(pressures_query, ") )", sep="")
  # DEBUG prints
  # print(pressures_query)
  pressures_df <- odb.read(odb, pressures_query)
  
  # Make a list of climate impacts, and those which have been identified as relevant
  # There probably is a nice way of doing this in R, but I do not know it.
  # So we just will use nested for loops.
  identified_pressures <- vector(mode = "logical", length = ncol(pressures_df))
  pressures_names_identified <- vector(mode = "character")
  pressures_names_unidentified <- vector(mode = "character")
  for (i in 5:ncol(pressures_df))
  {
    for (j in 1:nrow(pressures_df))
    {
      if(!is.na(pressures_df[j,i]))
      {
        identified_pressures[i] <- TRUE
      }
    }
  }
  # DEBUG prints
  # print(identified_pressures)
  for (i in 5:length(identified_pressures))
  {
    if(identified_pressures[i] == FALSE)
    {
      pressures_names_unidentified <- append(pressures_names_unidentified, substring(colnames(pressures_df)[i], 20))
    }
    else
    {
      pressures_names_identified <- append(pressures_names_identified, substring(colnames(pressures_df)[i], 20))
    }
  }
  # DEBUG prints
  print(pressures_names_identified)
  pressures <- list(pressures_names_identified=pressures_names_identified, pressures_names_unidentified=pressures_names_unidentified)
  return(pressures)
}

receivers_list <- impactReceivers(core_area, odb)
scan_level <- FALSE
delve_deeper_level <- FALSE

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
         choices=receivers_list[[1]],
         width = '100%'
         ),
       # A text output for debugging.  Comment out following line when releasing
       # verbatimTextOutput("receiver_choice_text"),
       actionButton("receiver_next", "Next")
     ),
     tabPanel(
       "Climate vulnerability/impact",
       # The impacts tab will allow the user to select the climate change impacts to the utility that they are assessing. 
       # These are termed vulnerabilities in Appendix A of the WSAA Guidleines.
       checkboxGroupInput(
         "impact_select",
         "Select what climate vulnerabilities/impacts you are assessing",
         width = '100%'
       ),
       # A text output for debugging.  Comment out following line when releasing
       # verbatimTextOutput("impact_choice_text"),
       actionButton("impacts_next", "Next")
     ),
     tabPanel(
       "Climate pressures",
       # The climate pressures tab will allow the user to select the climate pressures that 
       #   are relevant for the vulnerabilities the user has select in the previous tabs
       #  These are based on the identified impacts in Appendix A of the WSAA Guidelines.
       checkboxGroupInput(
         "pressure_select",
         "Select what climate change pressures you will be assessing vulnerability against",
         width = '100%'
       ),
       actionButton("pressures_next", "Next")
     ),
     tabPanel(
       "Level of detail",
       p("Please select the level of detail of your assessment, whether it will be a 'scan' or a 'delve deeper'"),
       actionButton("scan_next", "Scan level", width = '100%'),
       actionButton("delve_deeper_next", "Delve deeper", width = '100%')
     ),
     tabPanel(
       "Identified approaches, methods and data",
       fluidRow(
         column(width=12,
                p("Approaches for assessment"),
                p("Suitable modelling tools"),
                p("Climate data requirements"),
                p("Suitable datasets"),
                p("Services available")
         )
       )
       
       # Template code: Sidebar with a slider input for number of bins 
       # sidebarLayout(
       #   sidebarPanel(
       #     sliderInput("bins",
       #                 "Number of bins:",
       #                 min = 1,
       #                 max = 50,
       #                 value = 30)
       #   ),
         
         # Template code: Show a plot of the generated distribution
         # mainPanel(
         #   plotOutput("distPlot")
         # )
         
       
     )
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Template code  
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
   
   # Get what impact receivers were selected by the user and update what climate change impacts are relevant in the impacts panel. 
   impact_receivers_choice <- observeEvent(input$receiver_next, {
     # output$receiver_choice_text <- renderText(input$receiver_select)
     vulnerabilities_list <- climateVulnerabilities(input$receiver_select, core_area, odb)
     updateCheckboxGroupInput(session, "impact_select", choices = vulnerabilities_list[[1]])
     updateTabsetPanel(session,"main_panel","Climate vulnerability/impact")
     })
   
   # Get what impacts were selected by the user (for the receivers previously selected) and update the pressures as to what is relevant in the pressures panel.
   impacts_choice <- observeEvent(input$impacts_next, {
     pressures_list <- climatePressures(input$impact_select, input$receiver_select, core_area, odb)
     print(pressures_list)
     updateCheckboxGroupInput(session, "pressure_select", choices = pressures_list$pressures_names_identified)
     updateTabsetPanel(session,"main_panel","Climate pressures")
   })
   
   pressures_choice <- observeEvent(input$pressures_next, {
     updateTabsetPanel(session,"main_panel","Level of detailex")
   })
   
   level_of_detail_scan <- observeEvent(input$scan_next, {
     scan_level <- TRUE
     delve_deeper_level <- FALSE
     # Get applicable information and render it 
     updateTabsetPanel(session, "main_panel", "Identified approaches, methods and data")
   })
   
   delve_deeper_scan <- observeEvent(input$delve_deeper_next, {
     scan_level <- TRUE
     delve_deeper_level <- FALSE
     # Get applicable information and render it 
     updateTabsetPanel(session, "main_panel", "Identified approaches, methods and data")
   })
   
   # output$receiver_choice_text <- renderText(impact_receivers_choice)
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

