#install.packages("igraph")
library(igraph)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("reshape2")
library(reshape2)
#install.packages("DT")
library(DT)
#install.packages("network")
library(network)
#install.packages("plotly")
library(plotly)
#install.packages("intergraph")
library(intergraph)


setwd("/Users/annapenzo/Desktop/Advanced Social Network Analysis project")
df <- read.csv("eurovision_song_contest_1975_2019.csv", sep=";")
attributes <- read.csv("Attributes.csv", sep=";")
df_19 <- subset(df, Year > "2018")

f <- df_19[df_19[, "X.semi...final"] == "f",]

# EXPLORATORY ANALYSIS
#Bar plot

votes_by_country <- function(event, country) {
  data = event[event[,"From.country"] == country,]
  ggplot(data, aes(fill=Jury.or.Televoting, y=Points, x=To.country)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_y_continuous(breaks = seq(0, 12, 1), limits = c(0, 12)) +
    ggtitle("Votes by country and event") +
    scale_fill_discrete(name = "Jury or televoting") +
    xlab("Voted country") +
    theme(axis.text.x = element_text(angle = 90, size = 10))
}
votes_by_country(f, "Italy")


#CREATION OF THE MATRIX

agg <- aggregate(list(f$Points), by = list(f$From.country, f$To.country), sum) #aggregated J and T of the same country
names(agg)[names(agg) == 'c.12L..0L..0L..0L..3L..4L..8L..7L..6L..2L..5L..0L..0L..0L..0L..'] <- 'Points'
agg <- agg[agg$Group.1 != agg$Group.2,]

agg <- agg[agg$Points > 0,] #remove all rows where points were 0
matrix19 <- get.adjacency(
  graph_from_data_frame(agg, directed = TRUE),
  attr = "Points",
  sparse = FALSE
)

#Graph the network
Net <- graph.edgelist(as.matrix(agg[,1:2]), directed = T)

out_degrees = table(agg$Group.1)
in_degrees = table(agg$Group.2)
out_degrees 
in_degrees

#Directed network of relationships among countries
plot(Net, 
     vertex.size = 8, 
     vertex.label.cex = 0.4, 
     vertex.label.color = "black", 
     vertex.color = "tomato", 
     edge.arrow.size = 0.4)

#Undirected weighted network of relationships among countries
weighted_net <- graph.adjacency(matrix19, mode = "undirected", weighted = T)
plot(weighted_net,
     vertex.label.cex = .4, 
     vertex.label.color = "black", 
     vertex.size = 8,
     edge.width = E(weighted_net)$weight*0.1)

#CENTRALITY MEASURES
#degree centrality
degree(Net, mode = "in", normalized = T)
V(Net)$in_degree <- degree(Net, mode = "in", normalized = T) # assignment
plot(Net, 
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(Net)$in_degree*30)

#closeness centrality
closeness(Net)
V(Net)$closeness <- closeness(Net)
plot(Net,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.color = "lightgreen",
     vertex.size = V(Net)$closeness/max(V(Net)$closeness) * 20)

#betweenness centrality
betweenness(Net, directed = T, normalized = T)
V(Net)$betweenness <- betweenness(Net, directed = T, normalized = T) # assignment
plot(Net,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.color = "lightblue",
     vertex.size = V(Net)$betweenness/max(V(Net)$betweenness)*30)

#matrix of the centrality measures

all_atts <- lapply(vertex_attr_names(Net),function(x) vertex_attr(Net,x))
all_atts <- do.call("cbind", all_atts)
colnames(all_atts) <- vertex_attr_names(Net)
all_atts <- data.frame(all_atts[,2:ncol(all_atts)])
all_atts <- sapply(all_atts, as.numeric)
cormat <- cor(all_atts)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", direction=-2) +
  xlab("") +
  ylab("")

#centrality measures by country

all_atts <- as.data.frame(all_atts) 
names <- as.data.frame(row.names(as.data.frame(closeness(Net))))
rownames(all_atts) <- names$`row.names(as.data.frame(closeness(Net)))`
all_atts <- all_atts[order(all_atts$in_degree, decreasing = TRUE), ]
all_atts

datatable(all_atts) %>% formatStyle(names(all_atts),
                                    background = styleColorBar(range(all_atts), 'lightblue'),
                                    backgroundSize = '98% 88%',
                                    backgroundRepeat = 'no-repeat',
                                    backgroundPosition = 'center')



#GROUP DETECTION
#Weighted-networks per groups of countries

c_net <- function(start, end) {
  gr <- graph.adjacency(matrix19[start:end,start:end], mode = "directed", weighted = T)
  plot(gr,
       vertex.label.cex = .6, 
       vertex.label.color = "black", 
       vertex.size = 15,
       edge.width = E(gr)$weight*0.2)
}
c_net(30,41) #change numbers to select different groups of countries

#Finding groups in the network

highest_m <- agg[agg$Points > 16,]
Net2 <- graph.edgelist(as.matrix(highest_m[,1:2]), directed = T)
V(Net2)$Points <- highest_m$Points[match(V(Net2)$country, attributes$Country)]
plot(Net2, vertex.label.cex = 0.4, vertex.label.color = "black", vertex.color = "lightblue", edge.arrow.size = 0.4, vertex.size = 8)


#Creating the map with geolocation
dataf <- left_join(x = agg, y = attributes, by = c("Group.1" = "Country"))
dataf <- left_join(x = dataf, y = attributes, by = c("Group.2" = "Country"))
new_ev <- dataf[,c(1:4,10)]

points_to_country <- function(country) 
{if (!(country %in% new_ev$Acronym.y)) {print ("The country was not admited to the final")}
else {aggregate(new_ev$Points[new_ev$Acronym.y == country], 
                           by = list(from_code = as.character((new_ev$Acronym.x[new_ev$Acronym.y == country]))), FUN = median)}}
c = "ITA" #change the acronym to visualize different countries
points_to_country(c)

#Plot map only for countries admitted to the final
g <- list(
  scope = 'world',
  visible = T,
  showcountries = T,
  resolution = 50,
  lataxis = list(range = c(30, 70)), 
  lonaxis = list(range = c(-30, 60)),
  showland = FALSE
)

fig <- plot_ly()
fig <- add_trace(fig, type = "choropleth", z = points_to_country(c)$x, locationmode = "country codes", 
                 locations = points_to_country(c)$from_code, colorscale = "Blues", reversescale = TRUE)
fig <- fig %>% layout(geo = g, title = "Median votes")
fig

#### ERGM for 5 attributes 

#install.packages("statnet") 
library(statnet)

dt <- highest_m[order(highest_m$Group.1),]
Net3 <- graph.edgelist(as.matrix(dt[,1:2]), directed = T)
statnet_g <- asNetwork(Net3)

lst_val <- statnet_g$val
lst <- list()

for (x in lst_val){
 print(x[[2]][1]) 
 lst[[length(lst)+1]] <- x[[2]][1]
}


attr <- attributes[match(lst, attributes$Country),]

statnet_g %v% "Zone"<-attr$Zone
statnet_g %v% "Language"<-attr$Language
statnet_g %v% "EX.USSR"<-attr$EX.USSR
statnet_g %v% "EU"<-attr$EU
statnet_g %v% "Government"<-attr$Government

plot(statnet_g, 
     vertex.col=statnet_g %v% "Zone"*1, 
     vertex.cex = 1,
     displaylabels = TRUE,
     label.cex = .5,
     main = "Geographical areas network")
plot(statnet_g, 
     vertex.col=statnet_g %v% "Language"*3, 
     vertex.cex = 1,
     displaylabels = TRUE,
     label.cex = .5,
     main = "Language network")
plot(statnet_g, 
     vertex.col=statnet_g %v% "EX.USSR"*3, 
     vertex.cex = 1,
     displaylabels = TRUE,
     label.cex = .5,
     main = "Former USSR countries network")
plot(statnet_g, 
     vertex.col=statnet_g %v% "EU"*4, 
     vertex.cex = 1,
     displaylabels = TRUE,
     label.cex = .5,
     main = "European Union countries network")
plot(statnet_g, 
     vertex.col=statnet_g %v% "Government"*5, 
     vertex.cex = 1,
     displaylabels = TRUE,
     label.cex = .5,
     main = "Government type network")

set.seed(25)
model4 <- ergm(statnet_g ~ edges + 
                 nodematch("Zone") + #nodematch because discrete
                 nodematch("Language") + 
                 nodematch("EX.USSR") + 
                 nodematch("EU") + 
                 nodematch("Government") + 
                 mutual + #this stands for reciprocity
                 gwesp(0.25, fixed = T) + #triangles
                 gwidegree(decay=.5, fixed=T) +
                 gwodegree(decay=.5, fixed=T), 
               control=control.ergm(MCMLE.maxit= 40))
summary(model4)
