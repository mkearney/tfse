library(rtweet)
mookie <- get_timeline("ncamookie", n = 10000)
mookie$user_id

mookie.friends <- get_friends("ncamookie")
user_ids <- mookie.friends[[1]]
mookie.network <- lookup_users(user_ids)

n <- length(user_ids)
f <- vector("list", n)
tokens <- get_tokens()
j <- 1L

for (i in seq_len(n)) {
  f[[i]] <- tryCatch(get_friends(user_ids[i], token = tokens[j]),
    error = function(e) return(NULL))
  #if (is.null(r)) r <- data_frame_(ids = NA_real_)
  if (i %% 10 == 0) j <- j + 1
  if (i %% 25 == 0) message(paste0(round(i/2.44, 0), "% completed."))
}

keep <- user_ids %in% mookie.network$user_id
friend.networks <- f[keep]
names(friend.networks) <- mookie.network$screen_name

library(rtweet)
gf <- function(x, token, ...) {
  tryCatch(get_friends(x, token = token, ...),
    error = function(e) return(NULL))
}
ply_gf <- function(ids, tokens) {
  mapply(gf, ids, tokens)
}
flw <- get_followers("kearneymw")
flw <- unique(flw[[1]])[!is.na(unique(flw[[1]]))]
x <- lookup_users(flw)
kmw <- lookup_users("kearneymw")
x <- rbind(kmw, x)
tokens <- get_tokens()

adj_matrix <- function(x, tokens) {
  screen_names <- x[["screen_name"]]
  user_ids <- x[["user_id"]]
  n <- nrow(x)
  j <- cut(seq_len(n),
    breaks = seq(0, (ceiling(n / 15) * 15), 15),
    labels = FALSE)
  f <- ply_gf(user_ids, tokens[j])
  names(f)

  adj.mat <- unlist(lapply(f, function(x) {
    as.integer(user_ids %in% x[[1]])
  }), use.names = FALSE)

  adj.mat <- matrix(adj.mat, n, n)
  colnames(adj.mat) <- screen_names
  rownames(adj.mat) <- screen_names
  adj.mat
}

my_graph <- igraph::graph.adjacency(adj.mat)
clusters <- igraph::cluster_label_prop(my_graph)
igraph::membership(clusters)
library(igraph)

cols <- paste0(sample(tfse::gg_cols(10), n, replace = TRUE), "66")
cexs <- rep(4, n)
cols[screen_names == "BirdstheW0rd14"] <- "#dd3333cc"
cols[screen_names == "kearneymw"] <- "#3366ffcc"
cexs[screen_names == "BirdstheW0rd14"] <- 8
cexs[screen_names == "kearneymw"] <- 8


par(mar = c(0, 0, 0, 0))
plot(my_graph, igraph::membership(clusters),
  xlim = c(-.9, 0), ylim = c(-1, .1),
  vertex.size = cexs, vertex.label.cex = .5,
  vertex.frame.color = NA,
  #vertex.color = cols, vertex.label.color = "#000000cc",
  edge.width = .15, edge.color = NA,
  edge.arrow.size = .05)


png("mookie.png", 7000, 7000)
plot(my_graph,
  vertex.size = 5, vertex.label.cex = 2,
  vertex.color = cols, vertex.label.color = "black",
  edge.width = .15, edge.color = rgb(.3, .3, .3, .5),
  edge.arrow.size = .05)
dev.off()
my_graph
V(my_graph)
adj.mat2 <- adj.mat[screen_names %in% c(clusters[[1]], clusters[[2]]), screen_names %in% c(clusters[[1]], clusters[[2]])]
my_graph <- igraph::graph.adjacency(adj.mat2)
clusters <- igraph::cluster_label_prop(my_graph)
library(networkD3)
m3d <- igraph_to_networkD3(my_graph, group = membership(clusters))
forceNetwork(Links = m3d$links, Nodes = m3d$nodes,
  Source = 'source', Target = 'target', NodeID = 'name',
  Group = 'group', width = 500,
  height = 500, linkWidth = .1, bounded = TRUE,
  linkColour = "#33333399", zoom = TRUE,
  legend = FALSE, opacity = .5,
  opacityNoHover = TRUE)


plot(m3d)

library(igraph)
g <- graph.adjacency(adj.mat,mode="undirected")

plot(g)
b <- betweenness(g, v=V(g), directed = FALSE, weights = NULL,
  nobigint = TRUE, normalized = FALSE)
# Take the Square Root of the betweenness measure so nodes are not too large
V(g)$betweenness <- sqrt(b)
bad.vs <- V(g)[degree(g) == 0]

# remove isolated nodes
g <- delete.vertices(g, bad.vs)
c3 <- cluster_label_prop(g)
library(networkD3)

g_d3 <- igraph_to_networkD3(g)
plot(g_d3)

?forceNetwork

g[[3]]


forceNetwork(Links = g, Nodes = g_d3$nodes, Source = "source",
  Target = "target", Value = "value", NodeID = "name",
  Group = "group", opacity = 0.8, linkColour = ValjeanCols)

net <- forceNetwork(
  Links = g_d3$links,
  Nodes = g_d3$nodes,
  width = 700,
  height = 700,
  NodeID = 'headline')
 # radiusCalculation = JS("Math.sqrt(d.nodesize)+5"),
#  Nodesize = 'betweenness',
  #charge = -30,
  #linkWidth = .4,
  #colourScale = JS("d3.scale.category10()"),
  #linkColour = "goldenrod",
  #zoom = FALSE,
  #legend = TRUE,
  opacity = .85,
  #opacityNoHover = 0,
  #bounded = FALSE,
  #fontFamily = "San Francisco",
  fontSize = 14)



layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size = 2, label = NA,
  edge.arrow.size = .5, vertex.color="green")
?plot
?plot.graph
