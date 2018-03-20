#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
     tabPanel(
       "Receiver of impact",
       checkboxGroupInput(
         "receiver_select", 
         "Select the climate impact receivers you are assessing", 
         choices=source_water_impact_receivers
         ),
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
       actionButton("impacts_next", "Next"),
       verbatimTextOutput("impacts_choice_text")
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
   
   impact_receivers_choice <- eventReactive(input$receiver_next, {
     if(input$receiver_select[1] == "Surface water") impacts_list <- surface_water_impacts
     if(input$receiver_select[1] == "Stormwater") impacts_list <- stormwater_impacts
     if(input$receiver_select[1] == "Rainwater") impacts_list <- rainwater_impacts
     updateCheckboxGroupInput(session, "impact_select", choices = impacts_list)
     return(impacts_list)
     })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

