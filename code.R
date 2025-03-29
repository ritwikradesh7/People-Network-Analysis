#Refer Problem Statement from readme
#Import the library igraph, install it if not present
library("igraph")

#Read csv file
#In this dataset, the first column is directly connected to the second column and given random letters
data <- read.csv(file.choose(), header = T)
#Creating a dataframe
df <- data.frame(data$first,data$second)

#Create Network from the data
netw <- graph.data.frame(df, directed = T)
V(netw) #Vertices
E(netw) #Edges
#To store the degree of each node
V(netw)$degree <- degree(netw)
V(netw)$degree

#Create Histogram distribution based on degree
hist(V(netw)$degree,
     xlab = 'Degree of vertex',
     ylab = 'Frequency',
     col = 'red',
     main = 'Histogram Plot')
#We can observe that majority of the people/nodes have connections bw 0 and 10

#Plot Network Diagram
#Set seed to maintain consistency
set.seed(200)
plot(netw)
#Since the above is not readable we format the nodes
plot(netw,
     vertext.size = 2,
     vertex.color = 'lightblue',
     edge.arrow.size = 0.8,
     vertex.label.size = 0.8)

#Configure the network layouts according to degree and pre existing layouts
#We assign diff colors to each unique node using rainbow and increase the size of the nodes according to its degree
set.seed(200)
plot(netw,
     vertex.color = rainbow(52),
     vertex.size = V(netw)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
#We can observe CA has most connections

#Hightlight Hubs and Authorities
#In a social network Hub is a node that has high outgoing traffic
#Authority is a node that has most incoming traffic
hs<- hub_score(netw)$vector
as<- authority_score(netw)$vector
plot(netw,
     vertex.color = rainbow(52),
     vertex.size = hs*40,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
#We can observe CC is the main Hub
plot(netw,
     vertex.color = rainbow(52),
     vertex.size = as*40,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
#We can observe CA is the main authority

#Community Outlining
#We can outline the most interconnected groups in the network
#To convert the network into undirected graph
comm <- graph.data.frame(df, directed = F)
plot(comm)
cluster_network = cluster_edge_betweenness(comm)
set.seed(200)
plot(cluster_network,
     comm,
     vertex.size = 10,
     vertex.label.size = 0.8)
