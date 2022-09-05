library("igraph")
library("poweRlaw")
library("ggplot2")
# Just loading my data

#edge_list <- read.csv("edge-list-ocupacoes.csv")
#G <- graph.data.frame(edge_list)
# List of degrees
G = watts.strogatz.game(1,10000,4,0.1)
G = barabasi.game(100000)
#plot(G)
G.degrees <- degree(G)
# Let's count the frequencies of each degree
G.degree.histogram <- as.data.frame(table(G.degrees))
# Need to convert the first column to numbers, otherwise
# the log-log thing will not work (that's fair...)
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
# Now, plot it!
ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()