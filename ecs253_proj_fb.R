g<-read.graph("~/Downloads/celegansneural/celegansneural.gml",format=c("gml"))
is.directed(g)
is.connected(g,mode="strong")
clusters(g,mode= "strong")
mean(degree(g,mode ="in"))
mean(degree(g,mode ="out"))
hist(degree (g,mode ="in"), breaks=50,prob = T,main= "IN",color=2)
lines(density(degree (g,mode ="in")))
hist(degree (g,mode ="out"),breaks=50,prob= T, main ="OUT")
lines(density(degree (g,mode ="out")))

rglplot(g)
graph_1=get.data.frame(g, what=c("edges", "vertices", "both"))
write.graph(graph_1, "~/Downloads/celegansneural/graph_1.csv")
write.csv(graph_1, "~/Downloads/celegansneural/graph_1.csv", row.names = F,col.names=c(""))


V(g)$color <- ceiling( log(degree(g)+1))
range(degree(g)[which(V(g)$color==5)])
plot(g,vertex.label=NA,edge.arrow.size=0.1)
wc=walktrap.community(g)
modularity(wc)
plot(wc,g,vertex.label=NA,edge.arrow.size=0.1)


###################hw 3 prob1.1
library(igraph)
A = matrix(c(0,1,1,1,0,0,
             1,0,1,0,1,0,
             1,1,0,0,0,1,
             1,0,0,0,1,1,
             0,1,0,1,0,1,
             0,0,1,1,1,0),
           6,6)
A
g1=graph.adjacency(A,mode ='undirected')
B= matrix(0,6,6)
m =9
k= degree(g1)
for(i in 1:6)
{
  for(j in 1:6)
  {
    B[i,j] =  A[i,j] - k[i]*k[j]/m/2
  }
}
B
eigen(B)

###################hw 3 prob1.2
A2= matrix(c(0,5,0,1,
              5,0,1,0,
              0,1,0,5,
              1,0,5,0),
            4,4)
A2
g2= graph.adjacency(A_2,mode= 'undirected')
m2= sum(degree(g2))/2
m2
B2= matrix(0,4,4)
k2= degree(g2)
for(i in 1:4)
{
  for(j in 1:4)
  {
    B2[i,j] =  A2[i,j] - k2[i]*k2[j]/m2/2
  }
}
B2
eigen(B2)


###
sacbee_like <- read.csv("~/Downloads/sacbee_like.csv")
sacbee_comment <- read.csv("~/Downloads/sacbee_comment.csv")
ucdavis_like <- read.csv("~/Downloads/ucdavis_like.csv")
library(igraph)
sacbee_like = sacbee_like[
  duplicated(sacbee_like$fb_id, fromLast=TRUE) | duplicated(sacbee_like$fb_id)
,]
sacbee_comment= sacbee_comment[
  duplicated(sacbee_comment$fb_id, fromLast=TRUE) | duplicated(sacbee_comment$fb_id)
  ,]
ucdavis_like = ucdavis_like[
  duplicated(ucdavis_like$fb_id, fromLast=TRUE) | duplicated(ucdavis_like$fb_id)
  ,]
write.csv(sacbee_like, "~/Downloads/sacbee_like_new.csv",row.names = F)
write.csv(sacbee_comment, "~/Downloads/sacbee_comment_new.csv")

edgelist = sacbee_like[,c(2,4)]
g= graph.data.frame(edgelist)
V(g)$type <- V(g)$name %in% edgelist[,1]
bg=bipartite.projection(g)
print(bg[[1]], g=TRUE, e=TRUE)
print(bg[[2]], g=TRUE, e=TRUE)
graph.density(bg[[1]])
graph.density(bg[[2]])
jpeg('degree_user_253.jpg')
hist(degree(bg[[1]]),breaks = 50 ,
     main = 'nodes degree distribution of users for Sacramento Bee "like" data', 
     xlab='degree')
dev.off()
sacbee_like_1dg= degree(bg[[1]])
sacbee_like_2dg= degree(bg[[2]])

jpeg('degree_post_253.jpg')
hist(degree(bg[[2]]),breaks = 50 ,
     main = 'nodes degree distribution of posts for Sacramento Bee "like" data', 
     xlab='degree')
     )
dev.off()

plot(wc,bg[[1]],vertex.label=NA,edge.arrow.size=.1)
plot(bg[[1]],vertex.label=NA)

wc=walktrap.community(bg[[2]])
modularity(wc)
table(wc$membership)
# betweeness
btw =  betweenness(bg[[2]])
# fast greedy
fastg = fastgreedy.community(bg[[2]])

##
out_edge = E(bg[[2]])[crossing(fastg,bg[[2]])]
out_node = get.edges(bg[[2]],out_edge)
#crossing(fastg,bg[[2]])
sour_2 = fastg$membership[out_node[,1]]
targ_2 = fastg$membership[out_node[,2]]
cluster_community = cbind(sour_2,targ_2)
g_2_cluster = graph.data.frame(cluster_community)
# change paralell nows to weight of nodes
E(g_2_cluster)$weight <- 1
g_2_sim=simplify(g_2_cluster, edge.attr.comb=list(weight="sum"))
#
node_seq = V(g_2_sim)
V(g_2_sim)$weight = 
as.numeric(V(g_2_sim))
table(fastg$membership)[V(g_2_sim)$name]
plot(g_2_sim, edge.width=log2(E(g_2_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg$membership)[V(g_2_sim)$name])*3+2,
     main = 'aggregated posts network for Sacramento Bee "like" data')
# random walk
wc1=walktrap.community(bg[[1]])
modularity(wc1)
# betweeness
btw1=  betweenness(bg[[1]])
# fast greedy 
fastg1 = fastgreedy.community(bg[[1]])

out_edge1 = E(bg[[1]])[crossing(fastg1,bg[[1]])]
out_node1 = get.edges(bg[[1]],out_edge1)
#crossing(fastg,bg[[2]])
sour_1 = fastg1$membership[out_node1[,1]]
targ_1 = fastg1$membership[out_node1[,2]]
cluster_community1 = cbind(sour_1,targ_1)
g_1_cluster = graph.data.frame(cluster_community1)
# change paralell nows to weight of nodes
E(g_1_cluster)$weight <- 1
g_1_sim=simplify(g_1_cluster, edge.attr.comb=list(weight="sum"))
#
V(g_1_sim)$weight = 
  as.numeric(V(g_1_sim))
table(fastg1$membership)[V(g_1_sim)$name]
plot(g_1_sim, edge.width=log2(E(g_1_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg1$membership)[V(g_1_sim)$name])*2+2,
     main = 'aggregated users network for Sacramento Bee "like" data')
# random walk
wc1=walktrap.community(bg[[1]])
modularity(wc1)
# betweeness
btw1=  betweenness(bg[[1]])

#
is.connected(bg[[1]])
clusters(bg[[1]])
is.connected(bg[[2]])
clusters(bg[[2]])

#

##############################################
#############################sac-bee comment

edgelist = sacbee_comment[,c(2,4)]
g= graph.data.frame(edgelist)
V(g)$type <- V(g)$name %in% edgelist[,1]
bg=bipartite.projection(g)
print(bg[[1]], g=TRUE, e=TRUE)
print(bg[[2]], g=TRUE, e=TRUE)
graph.density(bg[[1]])
graph.density(bg[[2]])
sacbee_com_1dg= degree(bg[[1]])
sacbee_com_2dg= degree(bg[[2]])

jpeg('degree_user_253.jpg')
hist(sacbee_com_1dg,breaks = 50 , main = 'nodes degree distribution of user for Sacramento Bee "comment" data',
     xlab = 'degree')
dev.off()

jpeg('degree_post_253.jpg')
hist(sacbee_com_2dg,breaks = 50 , main = 'nodes degree distribution of post for Sacramento Bee "comment" data',
     xlab = 'degree')
     )
dev.off()

plot(wc,bg[[1]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1)
plot(bg[[1]],vertex.label=NA)

wc=walktrap.community(bg[[2]])
modularity(wc)
table(wc$membership)
# betweeness
btw =  betweenness(bg[[2]])
# fast greedy
fastg = fastgreedy.community(bg[[2]])
plot(fastg,bg[[2]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
       'community detection for posts network of Sacramento Bee "comment" data')
fastg1 = fastgreedy.community(bg[[1]])
plot(fastg1,bg[[1]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
       'community detection for users network of Sacramento Bee "comment" data')
##
out_edge = E(bg[[2]])[crossing(fastg,bg[[2]])]
out_node = get.edges(bg[[2]],out_edge)
#crossing(fastg,bg[[2]])
sour_2 = fastg$membership[out_node[,1]]
targ_2 = fastg$membership[out_node[,2]]
cluster_community = cbind(sour_2,targ_2)
g_2_cluster = graph.data.frame(cluster_community)
# change paralell nows to weight of nodes
E(g_2_cluster)$weight <- 1
g_2_sim=simplify(g_2_cluster, edge.attr.comb=list(weight="sum"))
#
node_seq = V(g_2_sim)
V(g_2_sim)$weight = 
  as.numeric(V(g_2_sim))
table(fastg$membership)[V(g_2_sim)$name]
plot(g_2_sim, edge.width=log(E(g_2_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =table(fastg$membership)[V(g_2_sim)$name]/10+1,
     main = 'aggregated posts network for Sacramento Bee "comment" data')
# random walk
wc1=walktrap.community(bg[[1]])
modularity(wc1)
# betweeness
btw1=  betweenness(bg[[1]])
# fast greedy 
fastg1 = fastgreedy.community(bg[[1]])
#
is.connected(bg[[1]])
is.connected(bg[[2]])
#

out_edge1 = E(bg[[1]])[crossing(fastg1,bg[[1]])]
out_node1 = get.edges(bg[[1]],out_edge1)
#crossing(fastg,bg[[2]])
sour_1 = fastg1$membership[out_node1[,1]]
targ_1 = fastg1$membership[out_node1[,2]]
cluster_community1 = cbind(sour_1,targ_1)
g_1_cluster = graph.data.frame(cluster_community1)
# change paralell nows to weight of nodes
E(g_1_cluster)$weight <- 1
g_1_sim=simplify(g_1_cluster, edge.attr.comb=list(weight="sum"))
#
V(g_1_sim)$weight = 
  as.numeric(V(g_1_sim))
table(fastg1$membership)[V(g_1_sim)$name]
plot(g_1_sim, edge.width=log2(E(g_1_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg1$membership)[V(g_1_sim)$name])*2+2,
     main = 'aggregated users network for Sacramento Bee "comment" data')
###########################
########################### UC D
ucd_comment <- read.csv("~/Downloads/ucdavis_comment.csv")
ucd_comment = ucd_comment[
  duplicated(ucd_comment$fb_id, fromLast=TRUE) | duplicated(ucd_comment$fb_id)
  ,]

edgelist = ucd_comment[,c(2,4)]
g= graph.data.frame(edgelist)
V(g)$type <- V(g)$name %in% edgelist[,1]
bg=bipartite.projection(g)
print(bg[[1]], g=TRUE, e=TRUE)
print(bg[[2]], g=TRUE, e=TRUE)
graph.density(bg[[1]]) #user
graph.density(bg[[2]]) #posts
ucd_com_1dg= degree(bg[[1]])
ucd_com_2dg= degree(bg[[2]])
hist(ucd_com_1dg,breaks = 100 , main = 'nodes degree distribution of user for UCD "comment" data',
     xlab = 'degree')
hist(ucd_com_2dg,breaks = 100 , main = 'nodes degree distribution of post for UCD "comment" data',
     xlab = 'degree')

# fast greedy
fastg = fastgreedy.community(bg[[2]])
plot(fastg,bg[[2]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
       'community detection for posts network of UCD "comment" data')
fastg1 = fastgreedy.community(bg[[1]])
plot(fastg1,bg[[1]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
       'community detection for users network of UCD "comment" data')
##
out_edge = E(bg[[2]])[crossing(fastg,bg[[2]])]
out_node = get.edges(bg[[2]],out_edge)
#crossing(fastg,bg[[2]])
sour_2 = fastg$membership[out_node[,1]]
targ_2 = fastg$membership[out_node[,2]]
cluster_community = cbind(sour_2,targ_2)
g_2_cluster = graph.data.frame(cluster_community)
# change paralell nows to weight of nodes
E(g_2_cluster)$weight <- 1
g_2_sim=simplify(g_2_cluster, edge.attr.comb=list(weight="sum"))
#
node_seq = V(g_2_sim)
V(g_2_sim)$weight = 
  as.numeric(V(g_2_sim))
table(fastg$membership)[V(g_2_sim)$name]
plot(g_2_sim, edge.width=log(E(g_2_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg$membership)[V(g_2_sim)$name])*3+1,
     main = 'aggregated posts network for UCD "comment" data')
#user
out_edge1 = E(bg[[1]])[crossing(fastg1,bg[[1]])]
out_node1 = get.edges(bg[[1]],out_edge1)
#crossing(fastg,bg[[2]])
sour_1 = fastg1$membership[out_node1[,1]]
targ_1 = fastg1$membership[out_node1[,2]]
cluster_community1 = cbind(sour_1,targ_1)
g_1_cluster = graph.data.frame(cluster_community1)
# change paralell nows to weight of nodes
E(g_1_cluster)$weight <- 1
g_1_sim=simplify(g_1_cluster, edge.attr.comb=list(weight="sum"))
#
V(g_1_sim)$weight = 
  as.numeric(V(g_1_sim))
table(fastg1$membership)[V(g_1_sim)$name]
plot(g_1_sim, edge.width=log2(E(g_1_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg1$membership)[V(g_1_sim)$name])*2+2,
     main = 'aggregated users network for UCD "comment" data')
###########################
########################### ucd like data


edgelist = ucdavis_like[,c(2,4)]
g= graph.data.frame(edgelist)
V(g)$type <- V(g)$name %in% edgelist[,1]
bg=bipartite.projection(g)
print(bg[[1]], g=TRUE, e=TRUE)
print(bg[[2]], g=TRUE, e=TRUE)
graph.density(bg[[1]]) #user
graph.density(bg[[2]]) #posts
ucd_like_1dg= degree(bg[[1]])
ucd_like_2dg= degree(bg[[2]])
hist(ucd_like_1dg,breaks = 100 , main = 'nodes degree distribution of user for UCD "like" data',
     xlab = 'degree')
hist(ucd_like_2dg,breaks = 100 , main = 'nodes degree distribution of post for UCD "like" data',
     xlab = 'degree')
# fast greedy
fastg = fastgreedy.community(bg[[2]])
#plot(fastg,bg[[2]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
#       'community detection for posts network of UCD "comment" data')
fastg1 = fastgreedy.community(bg[[1]])
#plot(fastg1,bg[[1]],vertex.label=NA,edge.arrow.size=.1, vertex.size =1, main = 
#       'community detection for users network of UCD "comment" data')
##
out_edge = E(bg[[2]])[crossing(fastg,bg[[2]])]
out_node = get.edges(bg[[2]],out_edge)
#crossing(fastg,bg[[2]])
sour_2 = fastg$membership[out_node[,1]]
targ_2 = fastg$membership[out_node[,2]]
cluster_community = cbind(sour_2,targ_2)
g_2_cluster = graph.data.frame(cluster_community)
# change paralell nows to weight of nodes
E(g_2_cluster)$weight <- 1
g_2_sim=simplify(g_2_cluster, edge.attr.comb=list(weight="sum"))
#
node_seq = V(g_2_sim)
V(g_2_sim)$weight = 
  as.numeric(V(g_2_sim))
table(fastg$membership)[V(g_2_sim)$name]
plot(g_2_sim, edge.width=log(E(g_2_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg$membership)[V(g_2_sim)$name])*3+1,
     main = 'aggregated posts network for UCD "like" data')
#user

out_edge1 = E(bg[[1]])[crossing(fastg1,bg[[1]])]
out_node1 = get.edges(bg[[1]],out_edge1)
sour_1 = fastg1$membership[out_node1[,1]]
targ_1 = fastg1$membership[out_node1[,2]]
cluster_community1 = cbind(sour_1,targ_1)
g_1_cluster = graph.data.frame(cluster_community1)
# change paralell nows to weight of nodes
E(g_1_cluster)$weight <- 1
g_1_sim=simplify(g_1_cluster, edge.attr.comb=list(weight="sum"))
#
V(g_1_sim)$weight = 
  as.numeric(V(g_1_sim))
table(fastg1$membership)[V(g_1_sim)$name]
plot(g_1_sim, edge.width=log2(E(g_1_sim)$weight)+1, edge.arrow.size=0.1,vertex.size =log2(table(fastg1$membership)[V(g_1_sim)$name])*2+2,
     main = 'aggregated users network for UCD "like" data')
###########################
