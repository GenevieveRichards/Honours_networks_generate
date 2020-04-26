library(igraph)
library(purrr)
library(readr)


MHmakeRandomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

LETTERS <- MHmakeRandomString(26, lenght = 2)
#Bipartite Graph
g <- make_bipartite_graph(rep(0:1,length=20),c(1:20), directed = FALSE )
plot(g)


#ER Random Network 
g <- erdos.renyi.game(26, 26/100) %>% set_vertex_attr("label", value = LETTERS[1:26])
degree_value <- degree(g)
plot(g, layout=layout_with_fr)
hist(degree_value)

df <- as_long_data_frame(g)
write_csv(df, "random_network.csv")


#String
g_2 <- make_star(26) %>% set_vertex_attr("label", value = LETTERS[1:26])
ends(g_2, E(g_2))
plot(g_2)
hist(degree(g_2))

df <- as_long_data_frame(g_2)
write_csv(df, "ring_network.csv")

#Small World Network - Watt and Strogatz Model 
g_sm <- sample_smallworld(1, 30, 5, 0.4) %>% set_vertex_attr("label", value = LETTERS[1:30])
mean_distance(g_sm)
transitivity(g_sm, type="average")
degree(g_sm)
plot(g_sm, size = 0.3)
hist(degree(g_sm))
df <- as_long_data_frame(g_sm)
write_csv(df, "small_world_network.csv")


#Scale Free Network 
g_scale<- sample_pa(30, directed = FALSE) %>% set_vertex_attr("label", value = LETTERS[1:30])
degree(g_scale)
plot(g_scale, size = 0.2, layout = layout_with_fr)
ends(g_scale, E(g_scale))
hist(degree(g_scale))

df <- as_long_data_frame(g_scale)
write_csv(df, "scale_free_network.csv")

#Lattice 
l <- make_lattice(c(5, 10, 5))
plot(l)
