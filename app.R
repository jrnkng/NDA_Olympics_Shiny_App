library("shiny")
library(visNetwork)
library("shinydashboard")
library(igraph)
library(dplyr)
library(ggplot2)
library(DT)
library("data.table")
# setwd("/Users/jeroenkoning/Desktop/own_projects/nda/NDA_Olympics_Shiny_App/")
noc.regions <- read.csv('./data/noc_regions.csv')
athlete.events <- as.data.table(read.csv('./data/athlete_events.csv'))

athlete.name.games <- list(athlete.events$Name, athlete.events$Games)

all.games <- unique(athlete.events$Games)
all.names <- unique(athlete.events$Name)
all.events <- unique(athlete.events$Event)
all.sports <- unique(athlete.events$Sport)

ui <- fluidPage(
  img(src = "./olympic_rings.png", height = 30, width = 100),
  titlePanel("Exploring the ..."),

  tabsetPanel(
    tabPanel("Athletes",
    
    ),
    tabPanel("Events",
   sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "sport",
        "Choose a sport",
        all.sports,
        multiple=TRUE,
        options = list(maxItems = 1)
      ),
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
        column(4, 
            h5('Athletes ranked by degree'),
            textOutput('athlete_degree'),
            h5('Athletes ranked by betweenness'),
            textOutput('athlete_betweenness'),
            h5('Athletes ranked by closeness'),
            textOutput('athlete_closeness')
        ),
        column(8,
          plotOutput('degree_distribution'), 
          textOutput('text_degree_distribution')
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

  output$degree_distribution <- renderPlot({
    g <- g.athletes()
    ggplot() + geom_histogram(aes(x=degree(g)),fill="orange", color="orange")
    
  })

  output$text_degree_distribution <- renderText({
    paste('Degree distribution: how many other athletes has athlete competed with in', input$event)
  })
  output$athlete_degree <- renderText({
    g <- g.athletes()
    V(g)$degree <- degree(g)
    athletes.per.event()
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-degree)]
    paste('i')
    paste(dt.top.athletes[1]$name, '---', as.character(dt.top.athletes[1]$degree))
  })

  output$athlete_betweenness <- renderText({
    g <- g.athletes()
    V(g)$betweenness <- betweenness(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-betweenness)]
    paste(dt.top.athletes[1]$name, '---', as.character(dt.top.athletes[1]$betweenness))
  })

  output$athlete_closeness<- renderText({
    g <- g.athletes()
    V(g)$closeness <- closeness(g)
    dt.top.athletes <- data.table(get.data.frame(g, what='vertices'))[order(-closeness)]
    paste(dt.top.athletes[1]$name, '---', as.character(dt.top.athletes[1]$closeness))
  })

  output$height_homophily <- renderText({
    g <- g.athletes()
    g.all.height <- induced.subgraph(g, !is.na(V(g)$Height))
    print(V(g.all.height)$Height)
    homophily <- assortativity(g.all.height, V(g.all.height)$Height)
    paste(homophily)
  })
  



}

options(shiny.autoreload = TRUE)
shinyApp(ui, server)