# athlete.events <- as.data.table(read.csv('./data/athlete_events.csv'))

# athletes.per.event <- athlete.events[Event == input$event]

# athletes.per.event <- select(athletes.per.event, Name, Games, Height, Weight)
# all.athletes <- select(athletes.per.event, Name, Height, Weight) %>% unique(by = 'Name') %>% cbind(type = TRUE)

#   # all.games <- df.athletes()[, list(name=unique(Games), type=FALSE)])


#   all.games <-  select(athletes.per.event, Name = Games) %>% unique(by = "Name") %>% cbind(type = FALSE)

#   all.vertices <- rbind(all.athletes, all.games, fill=TRUE)

#   g <- graph_from_data_frame(athletes.per.event, directed=FALSE)
#   V(g)
# athletes.per.event
