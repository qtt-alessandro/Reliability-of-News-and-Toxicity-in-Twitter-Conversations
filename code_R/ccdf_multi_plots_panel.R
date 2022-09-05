library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)
#library(distributional)


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

df <- fread(file = '/Users/alessandroquattrociocchi/Git/Reliability-of-News-and-Toxicity-in-Twitter-Conversations/code_R/data/full_metrics_thesis.csv',colClasses="character")


df[,outlet_score:=as.numeric(outlet_score)]
df[,toxicity_ratio:=as.numeric(toxicity_ratio)]
df[,unique_users:=as.numeric(unique_users)]
df[,size:=as.numeric(size)]
df[,depth:=as.numeric(depth)]
df[,wiener_index:=as.numeric(wiener_index)]
df[,assortativity_tox:=as.numeric(assortativity_tox)]



ccdf_depth_T <- tibble(ccdf(df[df$outlet_flag == "T",]$depth))
ccdf_depth_F <- tibble(ccdf(df[df$outlet_flag == "N",]$depth))

ccdf_depth_vl <- tibble(ccdf(df[df$outlet_label == "very_low",]$depth))
ccdf_depth_l <- tibble(ccdf(df[df$outlet_label == "low",]$depth))
ccdf_depth_m <- tibble(ccdf(df[df$outlet_label == "mixed",]$depth))
ccdf_depth_h <- tibble(ccdf(df[df$outlet_label == "high",]$depth))
ccdf_depth_vh <- tibble(ccdf(df[df$outlet_label == "very_high",]$depth))

colors <- c("Very Low" = "black", "Low" = "red", "Mixed" = 'pink', 'High' = 'orange', 'Very High' = 'blue')

ccdfDepthLabels <- ggplot() + 
                          geom_line(data = ccdf_depth_vl, aes(x,y,color = "Very Low")) +
                          geom_line(data = ccdf_depth_l, aes(x,y,color='Low')) +
                          geom_line(data = ccdf_depth_m, aes(x,y,color='Mixed')) +
                          geom_line(data = ccdf_depth_h, aes(x,y,color='High')) +
                          geom_line(data = ccdf_depth_vh, aes(x,y,color='Very High')) +
                          scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
                          scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
                          labs(x = "",
                               y = "CCDF Depth",
                               color = "") +
                          scale_color_manual(values = colors) + 
                          theme_classic()+theme(text = element_text(size=18),
                          legend.position="none")


ccdfDepthTF <- ggplot() + 
  geom_line(data = ccdf_depth_T, aes(x,y), linetype='dashed') +
  geom_line(data = ccdf_depth_F, aes(x,y)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "",
       color = "") +
scale_linetype_manual("Variabler",values=c("Antal Kassor"=2,"Medelvärde"=1))+

  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")

plot1 <- 
  ggdraw(ccdfDepthLabels) +
  draw_plot(ccdfDepthTF, x = 0.28, y = 0.22, width = 0.3, height = 0.3)

library(ggpubr)
plots <- plot_grid(plot1,plot1,plot1,plot1,labels = c("AUTO"))
legend1 <- get_legend(ccdfDepthLabels + theme(legend.position = "bottom"))
legend2 <- get_legend(ccdfDepthTF + theme(legend.position = "bottom", legend.title = element_blank()))
final_plot <- plot_grid(plots,legend1,legend2, ncol = 1, rel_heights = c(1,0.1,0.1), label_size = 20)

final_plot





