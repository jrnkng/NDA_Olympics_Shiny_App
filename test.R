library("shiny")
library(igraph)
library(dplyr)
library(ggplot2)
library(data.table)

setwd("/Users/jeroenkoning/Desktop/own_projects/nda/NDA_Olympics_Shiny_App")
noc.regions <- read.csv('./data/noc_regions.csv')
athlete.events <- as.data.table(read.csv('./data/athlete_events.csv'))

dt.unique.name.games <- unique(athlete.events, by = c("Name", "Games", "Event", "Medal"))
dt.unique.name.games
dt.unique.name.w.medals.games <- dt.unique.name.games[!is.na(dt.unique.name.games$Medal),]
dt.unique.name.w.medals.games <- dt.unique.name.w.medals.games[dt.unique.name.w.medals.games$Medal == "Gold"]
dt.unique.name.w.medals.games <- dt.unique.name.w.medals.games[dt.unique.name.w.medals.games]
dt.unique.name.w.medals.games
df.only.medals <- athlete.events[!is.na(athlete.events$Medal), ]
df.only.medals

df.medals.per.year.team <- df.only.medals %>% count(Year, Season)

df.medals.per.year.team

ggplot(df.medals.per.year.team, aes(x=Year, y=n, fill=Season)) + geom_col()

table(athlete.events[athlete.events$Name == "Sven Kramer"]$Medal)[["Gold"]]
                                                                

all.athletes <- dt.unique.name.w.medals.games[, list(name=unique(Name), type=TRUE)]
all.athletes
all.games <- dt.unique.name.w.medals.games[, list(name=unique(Games), type=FALSE)]
all.games
all.athletes
all.vertices <- rbind(all.athletes, all.games)

dt.unique.name.w.medals.games

g <- graph_from_data_frame(dt.unique.name.w.medals.games[, c("Games", "Name")], directed = TRUE, vertices=all.vertices)
g
g <- induced.subgraph(g, degree(g) > 0)

g

g.bi <- bipartite.projection(g)$proj2
g.bi
plot.igraph(g)

