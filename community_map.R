library(DiagrammeR)

descriptors.2.name <- c("educational practices", "intention", "self efficacy", 
                         "teacher competencies", "teacher educators")

nodes.community <- create_node_df(3, nodes = c("Community 1", "Community 2", "Community 3"),
                                  type = "a", label = FALSE, color = "aqua", shape = "rectangle")
nodes.descriptors <- create_node_df(5, nodes = descriptors.2.name, type = "b")
nodes.1 <- combine_ndfs(nodes.community, nodes.descriptors)
edges.community <- create_edge_df(from = c(1, 1, 1, 1, 1), to = c(4, 5, 6, 7, 8))

g <- create_graph(nodes_df = nodes.community, edges_df = edges.community, directed = FALSE)
render_graph(g)
