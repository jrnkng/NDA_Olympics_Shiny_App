library("shiny")
library(visNetwork)
library("shinydashboard")
library(igraph)
library(dplyr)
library(ggplot2)
library(DT)
library("data.table")

noc.regions <- read.csv('./data/noc_regions.csv')
athlete.events <- as.data.table(read.csv('./data/athlete_events.csv'))

load('athletes.RData')

athlete.name.games <- list(athlete.events$Name, athlete.events$Games)

all.games <- unique(athlete.events$Games)
all.names <- unique(athlete.events$Name)
all.events <- unique(athlete.events$Event)
all.sports <- unique(athlete.events$Sport)

ui <- fluidPage(
  # Import CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = 'header-row',
  fluidRow(
    column(1,
      img(src = "./olympic_rings.png", height = 30, width = 100),
    ),
    titlePanel("Olympiata"),
  ),
  h3('Finding out what and who matters in olympic events'),
  ),

  tabsetPanel(
    tabPanel("Athletes",
    
    ),
    tabPanel("Events",
   sidebarLayout(
    sidebarPanel(
      # selectizeInput(
      #   "sport",
      #   "Choose a sport",
      #   all.sports,
      #   multiple=TRUE,
      #   options = list(maxItems = 1)
      # ),
      selectizeInput(
        "event",
        "Choose an event",
        selected = "Athletics Women's 4 x 100 metres Relay",
        all.events
      )
    ),
  mainPanel(
    visNetworkOutput('athlete.vis'),
    checkboxInput('medals', 'Medal-winners only'),
    tabsetPanel(type = "tabs",
      # Athlete performance tab
      tabPanel("General",
        fluidRow(
        column(4, 
          h2('Athlete with most participations'),
          #
          h2('Athlete with most medals'),
          #
          h2('Olympic season'),
          #
          h2('Olympic event since'),
          #
          h2('Clustering coefficient within network'),
          textOutput('transitivity')
        ),
        column(8,
        )

      ),
      ),
    tabPanel("Centrality",
      fluidRow(
        column(6, 
            h5('Athletes ranked by degree'),
            DT::dataTableOutput("athlete.degree.dt"),
            h5('Athletes ranked by betweenness'),
            textOutput('athlete.betweenness'),
            DT::dataTableOutput("athlete.betweenness.dt"),
            h5('Athletes ranked by closeness'),
            DT::dataTableOutput("athlete.closeness.dt"),
            textOutput('athlete.closeness')
        ),
        column(6,
          plotOutput('degree.distribution'), 
          textOutput('text.degree.distribution')
        ),
      ),
    ),
        # Medal trends ta'b
    tabPanel("Homophily",
      fluidRow(
        column(4,
          h2('Level of homophily in the network'),
          p(
            'These values indicate the alikeness of athletes in the network with respects to physical attributes.
            The higher the positive value, the more the ahtletes are a like. 
            For many sports, you can see an increase in homophily between all athletes versus the medalists.'
          ),
          h3('Height homophily'),
          textOutput('height_homophily'),
          h3('Age homophily'),
          textOutput('age_homophily'),
          h3('Weight homophily'),
        )
      )
    ),

    ### SPORTSFINDER TAB 
    tabPanel("Sport finder",
      p('Are you an aspiring athlete? Or just wondering what you could have excelled at given your body type?
      The sports finder is here to help you find the sport that you are perfect for!
      '),
      numericInput('height', "Height in centimeters:", 180, min = 60, max = 260),
      numericInput('weight', "Weight in kilograms:", 65, min = 20, max = 200),
      actionButton('find.sport.button', 'Find your sport!'),
      textOutput('found.sport.details'),
    )
    )
  )
  ), 
    )
  )
  # Events exploration
)


server <- function(input, output, session) {
  selected.event <- reactive({
    print(input$sport)
    df <- athlete.events[Event == input$event]
    if(input$medals){
      df <- df[!is.na(Medal)]
    }
    return(df)
  })

  athletes.per.event <- reactive(select(selected.event(), Name, Games, Height, Weight))

  all.athletes <- reactive(select(athletes.per.event(), Name, Height, Weight) %>% unique(by = 'Name') %>% cbind(type = TRUE))

  all.games <-  reactive(select(athletes.per.event(), Name = Games) %>% unique(by = "Name") %>% cbind(type = FALSE))

  all.vertices <- reactive(rbind(all.athletes(), all.games(), fill=TRUE))

  g <- reactive(graph_from_data_frame(athletes.per.event(), directed=FALSE, vertices=all.vertices()))

  g.projection.athletes <- reactive(bipartite.projection(g())$proj2)

  g.athletes <- reactive(induced.subgraph(g.projection.athletes(), degree(g.projection.athletes()) > 0)) 


  g.projection.games <- reactive(bipartite.projection(g())$proj1)

  g.games <- reactive(induced.subgraph(g.projection.games(), degree(g.projection.games()) > 0))

  highest.degree.athlete <- reactive(sort(degree(g.athletes())))

  # General
  output$transitivity <- renderText({
    transitivity(g.athletes())
  })
  output$athlete.vis <- renderVisNetwork({
   visIgraph(g.athletes(), idToLabel = TRUE)
  })

  output$degree.distribution <- renderPlot({
    g <- g.athletes()
    ggplot() + geom_histogram(aes(x=degree(g)),fill="orange", color="orange")
    
  })

  output$text.degree.distribution <- renderText({
    paste('Degree distribution: how many other athletes has athlete competed with in', input$event)
  })
  output$athlete.degree <- renderText({
    g <- g.athletes()
    V(g)$degree <- degree(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-degree)]
    print(dt.top.athletes)
    paste(dt.top.athletes[1]$name, '---', as.character(dt.top.athletes[1]$degree))
  })

  output$athlete.degree.dt <- DT::renderDataTable({
    g <- g.athletes()
    V(g)$degree <- degree(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-degree)]
    return(dt.top.athletes)
  })

  output$athlete.betweenness.dt <- DT::renderDataTable({
    g <- g.athletes()
    V(g)$betweenness <- betweenness(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-betweenness)]
    return(dt.top.athletes)
  })
 
  output$athlete.closeness.dt <- DT::renderDataTable({
    g <- g.athletes()
    V(g)$closeness <- closeness(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-closeness)]
    return(dt.top.athletes)
  })

  output$height.homophily <- renderText({
    g <- g.athletes()
    g.all.height <- induced.subgraph(g, !is.na(V(g)$Height))
    print(V(g.all.height)$Height)
    homophily <- assortativity(g.all.height, V(g.all.height)$Height)
    paste(homophily)
  })

  # Sports finder!

  convert.kg.to.lbs <- function(kg){
    one.kg.in.lbs <- 2.2
    return(kg * one.kg.in.lbs)
  }

  found.sport <- eventReactive(input$find.sport.button, {
    user.height <- input$height
    user.weight <- convert.kg.to.lbs(input$weight)
    print(user.height)
    return(user.weight)
  })

  output$found.sport.details <- renderText({
    paste(found.sport())
  })

}

options(shiny.autoreload = TRUE)
shinyApp(ui, server)