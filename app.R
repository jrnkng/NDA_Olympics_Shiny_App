library("shiny")
library(igraph)
library(dplyr)
library(ggplot2)
library(DT)
setwd("/Users/jeroenkoning/Desktop/own_projects/nda/NDA_Olympics_Shiny_App")
noc.regions <- read.csv('./data/noc_regions.csv')
athlete.events <- as.data.frame(read.csv('./data/athlete_events.csv'))

athlete.name.games <- list(athlete.events$Name, athlete.events$Games)

all.games <- unique(athlete.events$Games)
all.names <- unique(athlete.events$Name)
print(min(athlete.events$Year))

ui <- fluidPage(
  DT::dataTableOutput('x1')
)


data("iris")
server <- function(input, output, session){
  output$x1 = DT::renderDataTable(athlete.events, server = FALSE)
  
  df.athlete <- reactive(athlete.events[athlete.events$Name == input$name, ])
  output$summary <- renderPrint({
    
  }) 
}

options(shiny.autoreload = TRUE, shiny.launch.browser = TRUE)
shinyApp(ui, server)

