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

#filtering function 
filtering_df <- function(dataset, LabelFilter, MetricFilter, selection){
  if (selection == 'multi'){
    
    x <- dataset %>%filter(outlet_label == as.name(LabelFilter))%>%select(as.name(MetricFilter))
  }
  else{
    
    x <- dataset %>%filter(outlet_flag == as.name(LabelFilter))%>%select(as.name(MetricFilter))
  }
  return (x)}

#function generating plot
generate_label_plot <- function(vl_df, l_df, m_df, h_df, vh_df, true_df, false_df,lab_selection){
  
colors <- c("Very Low" = "black", "Low" = "red", "Mixed" = 'pink', 'High' = 'orange', 'Very High' = 'blue')
if (lab_selection == 'depth'){lab = 'CCDF Depth'}
else if  (lab_selection == 'size'){lab = 'CCDF Size'}
else if  (lab_selection == 'wiener'){lab = 'CCDF Wiener Index'}
else if  (lab_selection == 'unique_user'){lab = 'CCDF Unique Users'}
outer_plot <- ggplot() + 
    geom_line(data = vl_df, aes(x,y,color = "Very Low")) +
    geom_line(data = l_df, aes(x,y,color='Low')) +
    geom_line(data = m_df, aes(x,y,color='Mixed')) +
    geom_line(data = h_df, aes(x,y,color='High')) +
    geom_line(data = vh_df, aes(x,y,color='Very High')) +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
    scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    labs(x = "",
         y = lab,
         color = "") +
    scale_color_manual(values = colors) + 
    theme_classic()+theme(text = element_text(size=18),
                          legend.position="none")


inner_plot <- ggplot() + 
  geom_line(data = true_df, aes(x,y), linetype='dashed') +
  geom_line(data = false_df, aes(x,y)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "",
       y = "",
       color = "") +
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")

plot1 <- 
  ggdraw(outer_plot) +
  draw_plot(inner_plot, x = 0.28, y = 0.22, width = 0.3, height = 0.3)

return(plot1)
}
  

df <- fread(file = '/Users/alessandroquattrociocchi/Git/Reliability-of-News-and-Toxicity-in-Twitter-Conversations/code_R/data/full_metrics_thesis.csv',colClasses="character")


df[,outlet_score:=as.numeric(outlet_score)]
df[,toxicity_ratio:=as.numeric(toxicity_ratio)]
df[,unique_users:=as.numeric(unique_users)]
df[,size:=as.numeric(size)]
df[,depth:=as.numeric(depth)]
df[,wiener_index:=as.numeric(wiener_index)]
df[,assortativity_tox:=as.numeric(assortativity_tox)]


### Depth 
ccdf_depth_vl <- ccdf(filtering_df(df, 'high', 'depth','multi')$depth)
ccdf_depth_l  <- ccdf(filtering_df(df, 'low', 'depth','multi')$depth)
ccdf_depth_m  <- ccdf(filtering_df(df, 'mixed', 'depth','multi')$depth)
ccdf_depth_h  <- ccdf(filtering_df(df, 'high', 'depth','multi')$depth)
ccdf_depth_vh <- ccdf(filtering_df(df, 'very_high', 'depth','multi')$depth)
ccdf_depth_T  <- ccdf(filtering_df(df, 'T', 'depth','binary')$depth)
ccdf_depth_F  <- ccdf(filtering_df(df, 'N', 'depth','binary')$depth)

### Size 
ccdf_size_vl <- ccdf(filtering_df(df, 'high', 'size','multi')$size)
ccdf_size_l  <- ccdf(filtering_df(df, 'low', 'size','multi')$size)
ccdf_size_m  <- ccdf(filtering_df(df, 'mixed', 'size','multi')$size)
ccdf_size_h  <- ccdf(filtering_df(df, 'high', 'size','multi')$size)
ccdf_size_vh <- ccdf(filtering_df(df, 'very_high', 'size','multi')$size)
ccdf_size_T <- ccdf(filtering_df(df, 'T', 'size','binary')$size)
ccdf_size_F <- ccdf(filtering_df(df, 'N', 'size','binary')$size)

### Unique Users 
ccdf_nunique_vl <- ccdf(filtering_df(df, 'high', 'unique_users','multi')$unique_users)
ccdf_nunique_l  <- ccdf(filtering_df(df, 'low', 'unique_users','multi')$unique_users)
ccdf_nunique_m  <- ccdf(filtering_df(df, 'mixed', 'unique_users','multi')$unique_users)
ccdf_nunique_h  <- ccdf(filtering_df(df, 'high', 'unique_users','multi')$unique_users)
ccdf_nunique_vh <- ccdf(filtering_df(df, 'very_high', 'unique_users','multi')$unique_users)
ccdf_nunique_T <- ccdf(filtering_df(df, 'T', 'unique_users','binary')$unique_users)
ccdf_nunique_F <- ccdf(filtering_df(df, 'N', 'unique_users','binary')$unique_users)

### Wiener Index
ccdf_wiener_vl <- ccdf(filtering_df(df, 'high', 'wiener_index','multi')$wiener_index)
ccdf_wiener_l  <- ccdf(filtering_df(df, 'low', 'wiener_index','multi')$wiener_index)
ccdf_wiener_m  <- ccdf(filtering_df(df, 'mixed', 'wiener_index','multi')$wiener_index)
ccdf_wiener_h  <- ccdf(filtering_df(df, 'high', 'wiener_index','multi')$wiener_index)
ccdf_wiener_vh <- ccdf(filtering_df(df, 'very_high', 'wiener_index','multi')$wiener_index)
ccdf_wiener_T <- ccdf(filtering_df(df, 'T', 'wiener_index','binary')$wiener_index)
ccdf_wiener_F <- ccdf(filtering_df(df, 'N', 'wiener_index','binary')$wiener_index)


ccdfSize_plot <- generate_label_plot(ccdf_size_vl, ccdf_size_l, ccdf_size_m, ccdf_size_h, ccdf_size_vh, ccdf_size_T, ccdf_size_F,'size')
ccdfDepth_plot <- generate_label_plot(ccdf_size_vl, ccdf_size_l, ccdf_size_m, ccdf_size_h, ccdf_size_vh, ccdf_size_T, ccdf_size_F,'depth')
ccdfnunique_plot <- generate_label_plot(ccdf_nunique_vl, ccdf_nunique_l, ccdf_nunique_m, ccdf_nunique_h, ccdf_nunique_vh, ccdf_nunique_T, ccdf_nunique_F,'unique_user')
ccdfwiener_plot <- generate_label_plot(ccdf_wiener_vl, ccdf_wiener_l, ccdf_wiener_m, ccdf_wiener_h, ccdf_wiener_vh, ccdf_wiener_T, ccdf_wiener_F,'wiener')

library(ggpubr)
plots <- plot_grid(ccdfSize_plot,ccdfDepth_plot,ccdfnunique_plot,ccdfwiener_plot,labels = c("AUTO"))
legend1 <- get_legend(ccdfSize_plot + theme(legend.position = "bottom"))
legend2 <- get_legend(ccdfSize_plot + theme(legend.position = "bottom", legend.title = element_blank()))


final_plot <- plot_grid(plots,legend1,legend2, ncol = 1, rel_heights = c(1,0.005,0.005), label_size = 20)

pdf(file = "/Users/alessandroquattrociocchi/Desktop/plot.pdf") 
final_plot
dev.off()



