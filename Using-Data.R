######### Visualizing and using data ########
## Written by: Aislyn Keyes
## Last edited: 3 May 2021
## Version 1

## Load R packages
library(igraph)
library(dplyr)
library(NetIndices)
library(ggplot2)
library(plyr)


## Load data
# This will load the species list for all three salt marshes, using the final filtered Hechinger et al. (2011)
# data that was used in Keyes et al. (2021). These exclude the ecosystem service nodes
species <- read.csv("Final-Full-Node-List.csv")

# This will load the edge list for all thress salt marshes, using the final filtered Hechinger et al, (2021)
# data that was used in Keyes et al. (2021), These exclude the ecosystem service edges
edges <- read.csv("Final-Full-Edge-List.csv")

# These files will load the ecosystem service node and interaction lists for each salt marsh individually
# Carpinteria Salt Marsh (CSM)
csm.nodes <- read.csv("CSM_Nodes_Final.csv") #Nodes
csm.edges <- read.csv("CSM_Links_Final.csv") #Interactions
csm.edges <- csm.edges[,-1] #Remove column 1

# Bahia Falsa de San Quintin (BSQ)
bsq.nodes <- read.csv("BSQ_Nodes_Final.csv") #Nodes
bsq.edges <- read.csv("BSQ_Links_Final.csv") #Interactions
bsq.edges <- bsq.edges[,-1] #Remove column 1

# Estero de Punta Banda (EPB)
epb.nodes <- read.csv("EPB_Nodes_Final.csv") #Nodes
epb.edges <- read.csv("EPB_Links_Final.csv") #Interactions
epb.edges <- epb.edges[,-1] #Remove column 1

## Process data so that you have an edge list and node list for each salt marsh with both species and 
## services

colnames(edges)[colnames(edges)=="ï..ResourceSpeciesID"] <- "ResourceSpeciesID" #fix column name
colnames(species)[colnames(species)=="ï..SpeciesID"] <- "SpeciesID" #fix column name

# CSM
# interactions
csm.feed <- subset(edges, System=="CSM") # pull feeding edges from full edge list
csm.feed <- csm.feed[,-4] # get rid of System column
csm.edges <- rbind(csm.feed,csm.edges) # combine feeding and ecosystem service links

# nodes
csm.spp <- subset(species, System=="CSM") # pull the full, filtered species list
csm.node <- rbind.fill(csm.nodes,csm.spp) # combine the two node lists
csm.nodes <- distinct(csm.node,SpeciesID,.keep_all=TRUE) # remove rows with duplicate speciesID
attach(csm.nodes)
csm.nodes <- csm.nodes[order(SpeciesID),] # reorder nodes by SpeciesID so ecosystem services are last
detach(csm.nodes)
csm.nodes$NodeType <- ifelse(csm.nodes$SpeciesID < 350, 'Species', 'Service') #Fix node type to be species or service


# BSQ
# interactions
bsq.feed <- subset(edges, System=="BSQ") # pull service edges from full edge list
bsq.feed <- bsq.feed[,-4] # get rid of System column
bsq.edges <- rbind(bsq.feed,bsq.edges) # combine feeding and ecosystem service links

# nodes
bsq.spp <- subset(species, System=="BSQ") # pull the full, filtered species list
bsq.node <- rbind.fill(bsq.nodes,bsq.spp) # combine the two node lists
bsq.nodes <- distinct(bsq.node,SpeciesID,.keep_all=TRUE) # remove rows with duplicate speciesID
attach(bsq.nodes)
bsq.nodes <- bsq.nodes[order(SpeciesID),] # reorder nodes by SpeciesID so ecosystem services are last
detach(bsq.nodes)
bsq.nodes$NodeType <- ifelse(bsq.nodes$SpeciesID < 350, 'Species', 'Service') #Fix node type to be species or service


# EPB
# interactions
epb.feed <- subset(edges, System=="EPB") # pull service edges from full edge list
epb.feed <- epb.feed[,-4] # get rid of System column
epb.edges <- rbind(epb.feed,epb.edges) # combine feeding and ecosystem service links

# nodes
epb.spp <- subset(species, System=="EPB") # pull the full, filtered species list
epb.node <- rbind.fill(epb.nodes,epb.spp) # combine the two node lists
epb.nodes <- distinct(epb.node,SpeciesID,.keep_all=TRUE) # remove rows with duplicate speciesID
attach(epb.nodes)
epb.nodes <- epb.nodes[order(SpeciesID),] # reorder nodes by SpeciesID so ecosystem services are last
detach(epb.nodes)
epb.nodes$NodeType <- ifelse(epb.nodes$SpeciesID < 350, 'Species', 'Service') #Fix node type to be species or service

## Using the network data
# In order to use the network data, you need to put it into an igraph graph object format or an 
# adjacency matrix, depending on what you plan to do with it. 

# To create an igraph object for each salt marsh:
# CSM
csm.g <- graph_from_data_frame(csm.edges, #edge list
                               directed=TRUE, # directed graph, arrows point from resource to consumer
                               vertices=csm.nodes) # nodes, can be used as attributes later on
csm.g <- simplify(csm.g, remove.loops=TRUE) # to get rid of cannibalism

# BSQ
bsq.g <- graph_from_data_frame(bsq.edges, #edge list
                               directed=TRUE, # directed graph, arrows point from resource to consumer
                               vertices=bsq.nodes) # nodes, can be used as attributes later on
bsq.g <- simplify(bsq.g, remove.loops=TRUE) # to get rid of cannibalism

# EPB
epb.g <- graph_from_data_frame(epb.edges, #edge list
                               directed=TRUE, # directed graph, arrows point from resource to consumer
                               vertices=epb.nodes) # nodes, can be used as attributes later on
epb.g <- simplify(epb.g, remove.loops=TRUE) # to get rid of cannibalism

# With your networks in igraph format, you can now use any of the built in igraph functions 
# For example:
degree(csm.g) # degree provides you with the total number of interactions each node has in a network
plot.igraph(csm.g) # plot.igraph gives you a basic network visualization of the network

# To create an adjacency matrix for each salt marsh after you've created an igraph object:
csm.mat <- get.adjacency(csm.g, sparse=FALSE, attr = NULL) # CSM
bsq.mat <- get.adjacency(bsq.g, sparse=FALSE, attr = NULL) # BSQ
epb.mat <- get.adjacency(epb.g, sparse=FALSE, attr = NULL) # EPB

## Plotting the networks
# In order to plot the networks organized vertically based on trophic level,
# you start with the adjacency matrices. Here, I work through an example with one of the salt
# marshes. You can do this for all of them by replacing 'csm' with bsq or epb throughout.

# add column to specify interaction color
csm.edges$col <- ifelse(csm.edges$Type=='Feeding', "gray75", "black") 

# add column to specify node shape and color 
# you can choose whichever colors and shapes you'd like
csm.nodes$col = ifelse(csm.nodes$NodeType=="Species", "#C71585", "deep sky blue")
csm.nodes$shape = ifelse(csm.nodes$NodeType=="Species","circle","square")

csm.g <- graph_from_data_frame(csm.edges, directed=TRUE, vertices=csm.nodes) # make a graph object with new attribute data


layt <- layout.fruchterman.reingold(csm.g) # set a graph layout

csm.mat <- get.adjacency(csm.g, sparse = FALSE, attr = NULL) # convert to adj mat

t1 <- TrophInd(csm.mat) # estimate the trophic level of all nodes
layt[,2] <- t1$TL # ensures that nodes will be arranged by trophic level from top to bottom


# Plot network
plot(csm.g, # igraph object
     layout=layt, # specify trophic layout
     vertex.size = 5, # adjust the size of the nodes
     vertex.label = NA, # can add labels if you want but tends to look busy
     edge.arrow.size=0.5, # can adjust the size of interactions
     vertex.color=csm.nodes$col, # change the color of nodes based on attributes set earlier
     vertex.shape=csm.nodes$shape, # change shape of nodes based on attributes set earlier
     edge.color=csm.edges$col, # change interaction color based on attributes set earlier
     main="Carpinteria Salt Marsh") # add plot title

# add legend, once you run this you can click anywhere on the plot to place the legend
legend(locator(n=1),
       legend=c("Species","Ecosystem Service","Species interaction","Service provisioning"),
       col=c("#C71585","deep sky blue","grey","black"),
       pch=c(19,15,NA,NA),lty=c(NA,NA,1,1),cex=1, lwd=3,bty="n",
       ncol=2)



