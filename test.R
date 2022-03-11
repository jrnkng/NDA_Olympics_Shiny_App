# library("shiny")
# library(igraph)
# library(dplyr)
# library(ggplot2)
# library(data.table)

# setwd("/Users/jeroenkoning/Desktop/own_projects/nda/NDA_Olympics_Shiny_App")
# noc.regions <- read.csv('./data/noc_regions.csv')
# athlete.events <- as.data.table(read.csv('./data/athlete_events.csv'))
# View(athlete.events)
# event <- "Swimming Men's 100 metres Butterfly"

# athletes.per.event <- athlete.events[Event == event & !is.na(Medal)]
# unique(athletes.per.event, by = 'Name')
# athletes.per.event.with.medal <- athletes.per.event[!is.na(Medal)]
# df.g.medals <- select(athletes.per.event, Name, Games)
# df.g <- select(athletes.per.event, Name, Games) 
# df.g

# all.athletes <- df.g[, list(name=unique(Name), type=TRUE)]
# all.events <- df.g[, list(name=unique(Games), type=FALSE)]
# View(df.g)
# all.vertices <- rbind(all.athletes, all.events)
# all.vertices[1]
# all.vertices

# g <- graph_from_data_frame(df.g.medals, directed=TRUE)
# V(g)
# plot.igraph(g)
# # Graph with each node either a game or athlete
# V(g)$type
# V(g)$color <- V(g)$type + 1
# plot.igraph(g, vertex.size = 2, vertex.label=NA)

# # Bipartite plot, showing athletes to which olympic games they participated
# g %>% add_layout_(as_bipartite()) %>% plot(vertex.size = 2)

# # Graph connection athletes based on event participation
# g <- graph_from_data_frame(df.g.medals, directed=FALSE, vertices = all.vertices)
# g.athletes <- bipartite.projection(g)$proj2
# g <- induced.subgraph(g.athletes, degree(g.athletes) > 0)
# plot.igraph(g, vertex.size = 1, vertex.label=NA)
# transitivity(g, type="average")
# betweenness(g)
# sort(degree(g))
# # plot.igraph(as_bipartite(g).athletes)
# athletes.per.event.with.medal


# #######3
# athletes.per.event <- athlete.events[Event == event]
# unique(athletes.per.event, by = 'Name')
# athletes.per.event.with.medal <- athletes.per.event[!is.na(Medal)]
# df.g.medals <- select(athletes.per.event.with.medal, Name, Games, Height, Weight)
# df.g.medals
# df.g <- select(athletes.per.event, Name, Height, Weight) 
# df.g$type = TRUE
# df.g.incl.games <- select(athletes.per.event, Name, Games, Height, Weight)
# df.g

# all.athletes <- unique(df.g, by="Name")
# all.athletes
# all.athletes <- df.g[, list(Name=unique(Name), type=TRUE)]
# all.athletes
# df.g
# all.athletes.merged <- dplyr::left_join(all.athletes, df.g, by="Name", copy = FALSE) %>% unique(by="Name")
# all.athletes.merged

# all.events <- athletes.per.event[, list(Name=unique(Games), type=FALSE)]
# all.events
# View(df.g)
# all.vertices
# all.athletes
# all.vertices <- rbind(all.athletes.merged, all.events, fill=TRUE)
# all.vertices
# df.g.incl.games
# df.g.incl.games
# g <- graph_from_data_frame(df.g.incl.games, directed=FALSE, vertices = all.vertices)

# # Graph with each node either a game or athlete
# V(g)
# V(g)$Height
# V(g)$color <- V(g)$type + 1
# plot.igraph(g, vertex.size = 12, vertex.label=NA, vertex.color.cex=c("red", "green"))

# # Bipartite plot, showing athletes to which olympic games they participated 
# g %>% add_layout_(as_bipartite()) %>% plot(vertex.size = 2)

# # Graph connection athletes based on event participation
# g <- graph_from_data_frame(df.g.incl.games, directed=FALSE, vertices = all.vertices)
# g.athletes <- bipartite.projection(g)$proj2
# g <- induced.subgraph(g.athletes, degree(g.athletes) > 0)
# V(g)$color <- V(g)
# plot.igraph(g, vertex.size = 14, vertex.label=NA)
# transitivity(g.athletes, type="average")
# sort(betweenness(g, normalized=TRUE))
# sort(degree(g))
# ggplot() + geom_histogram(aes(x=degree(g)),fill="orange", color="orange")

# rsconnect::setAccountInfo(name='ndagroup15-bk-jk',
#                           token='8B32285D71BDA3FEEBF8D01B42CBB1CE',
#                           secret='HIb/PSluLUUcvgwrXxZaohTFpGdo8WT/1zsGgRZX')
# library(rsconnect)
# rsconnect::deployApp('./')
# V(g.athletes)$Height
# assortativity(g.athletes, as.numeric(as.factor(V(g.athletes)$Height)))
# # plot.igraph(as_bipartite(g).athletes)
# athletes.per.event.with.medal


# dt.unique.name.games <- unique(athlete.events, by = c("Name", "Games", "Event", "Medal"))
# dt.unique.name.games
# dt.unique.name.w.medals.games <- dt.unique.name.games[!is.na(dt.unique.name.games$Medal),]
# dt.unique.name.w.medals.games <- dt.unique.name.w.medals.games[dt.unique.name.w.medals.games$Medal == "Gold"]
# dt.unique.name.w.medals.games <- dt.unique.name.w.medals.games[dt.unique.name.w.medals.games]
# dt.unique.name.w.medals.games
# df.only.medals <- athlete.events[!is.na(athlete.events$Medal), ]
# df.only.medals

# df.medals.per.year.team <- df.only.medals %>% count(Year, Season)

# df.medals.per.year.team

# ggplot(df.medals.per.year.team, aes(x=Year, y=n, fill=Season)) + geom_col()

# table(athlete.events[athlete.events$Name == "Sven Kramer"]$Medal)[["Gold"]]
                                                                

# all.athletes <- dt.unique.name.w.medals.games[, list(name=unique(Name), type=TRUE)]
# all.athletes
# all.games <- dt.unique.name.w.medals.games[, list(name=unique(Games), type=FALSE)]
# all.games
# all.athletes
# all.vertices <- rbind(all.athletes, all.games)

# dt.unique.name.w.medals.games

# g <- graph_from_data_frame(dt.unique.name.w.medals.games[, c("Games", "Name")], directed = TRUE, vertices=all.vertices)

# g <- induced.subgraph(g, degree(g) > 0)

# g

# g.bi <- bipartite.projection(g)$proj2
# g.bi
# plot.igraph(g, vertex.label.cex = 0.7)

