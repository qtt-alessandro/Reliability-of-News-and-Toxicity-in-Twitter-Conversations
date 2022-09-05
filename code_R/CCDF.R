library(ggplot2)
library(dplyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(distributional)
library(dplyr)


#it's better to plot the outcome on a y;log(x) scale sometimes
ccdf <- function(vector){
  #empirical cumulative distribution function
  tmp <- ecdf(vector)
  #complementary cumulative distribution function
  tmplot <- 1- tmp(sort(unique(vector)))
  #output:data_frame_for_plotting
  ccdf_for_plot <- cbind.data.frame(x=sort(unique(vector)), y=tmplot)
  return(ccdf_for_plot)
}

my_ecdf <- function(vector){
  #empirical cumulative distribution function
  tmp <- ecdf(vector)
  tmplot <- tmp(sort(unique(vector)))
  #output:data_frame_for_plotting
  ecdf_for_plot <- cbind.data.frame(x=sort(unique(vector)), y=tmplot)
  return(ecdf_for_plot)
}

df <- fread(file = '/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/data/metrics_data.csv',colClasses="character")


df[,outlet_score:=as.numeric(outlet_score)]
df[,toxicity_ratio:=as.numeric(toxicity_ratio)]
df[,unique_users:=as.numeric(unique_users)]
df[,size:=as.numeric(size)]
df[,depth:=as.numeric(depth)]
df[,wiener_index:=as.numeric(wiener_index)]
df[,assortativity_tox:=as.numeric(assortativity_tox)]


    
### Depth
fake_tt <- tibble(ccdf(fake_df$depth))
true_tt <- tibble(ccdf(true_df$depth))
colors <- c("True" = "black", "Fake" = "red")

png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/ccdf_depth.png", units="in", width=7, height=7, res=320)
ggplot() + 
  geom_line(data = fake_tt, aes(x,y,color = "Fake")) +
  geom_line(data = true_tt, aes(x,y,color='True')) +
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1)) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "CCDF Depth",
       color = "") +
  scale_color_manual(values = colors)
dev.off()

### unique users
fake_tt <- tibble(ccdf(fake_df$unique_users))
true_tt <- tibble(ccdf(true_df$unique_users))
colors <- c("True" = "black", "Fake" = "red")

png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/ccdf_unique_users.png", units="in", width=7, height=7, res=320)
ggplot() + 
  geom_line(data = fake_tt, aes(x,y,color = "Fake")) +
  geom_line(data = true_tt, aes(x,y,color='True')) +
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1)) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "CCDF Unique users",
       color = "") +
  scale_color_manual(values = colors)
dev.off()

### Tree size
fake_tt <- tibble(ccdf(fake_df$size))
true_tt <- tibble(ccdf(true_df$size))
colors <- c("True" = "black", "Fake" = "red")
png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/ccdf_tree_size.png", units="in", width=7, height=7, res=320)
ggplot() + 
  geom_line(data = fake_tt, aes(x,y,color = "Fake")) +
  geom_line(data = true_tt, aes(x,y,color='True')) +
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1)) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "CCDF Size",
       color = "") +
  scale_color_manual(values = colors)
dev.off()

### Wiener Index
fake_tt <- tibble(ccdf(fake_df$wiener_index))
true_tt <- tibble(ccdf(true_df$wiener_index))
colors <- c("True" = "black", "Fake" = "red")
png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/ccdf_wiener_index.png", units="in", width=7, height=7, res=320)
ggplot() + 
  geom_line(data = fake_tt, aes(x,y,color = "Fake")) +
  geom_line(data = true_tt, aes(x,y,color='True')) +
  scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1)) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "CCDF Wiener Index",
       color = "") +
  scale_color_manual(values = colors)
dev.off()



mean(df$size)
