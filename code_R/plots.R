
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

library(ggplot2)
library(dplyr)

df <- read.csv(file = '/Users/alessandroquattrociocchi/Desktop/test.csv')

ggplot(df, aes( x = wiener_index)) + geom_histogram(fill = "white", color = "black") +
  scale_x_log10(#breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(#breaks = scales::trans_breaks("log10", function(x) 10^x)
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Wiener Index", y = "Count") +
  theme_classic()
#sembra che le cascate siano molto "viral"
#possiamo confrontarle con delle cascate random? cioe con degli alberi randomici con stesso n?

tibble(ccdf(df$wiener_index)) %>%
  ggplot(aes(x = x, y = y)) + geom_line() + 
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Wiener Index", y = "CCDF") +
  theme_classic()


intervals <- 10^seq(log10(min(df$wiener_index)-0.1),log10(max(df$wiener_index)+0.1),length.out = 15)

df <- df %>% mutate(intervals_wiener = cut(wiener_index, breaks = intervals, dig.lab = 10)) %>%
  group_by(intervals_wiener) %>% mutate(toxicity_ratio_interval = mean(toxicity_ratio)) %>%
  mutate(toxicity_ratio_median_interval = median(toxicity_ratio)) %>% ungroup()
 
ggplot(df, aes( x = intervals_wiener, y = toxicity_ratio_interval)) + 
  geom_point() + 
  labs(title = "Sorcona 80s")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))

regression_table <- 
df %>% select(intervals_wiener,toxicity_ratio_median_interval) %>% arrange(intervals_wiener) %>%
  distinct() %>% ungroup() %>% mutate(rn = row_number())
model1 <- lm(regression_table$toxicity_ratio_median_interval[-1]~regression_table$rn[-1])
summary(model1)

ggplot(df, aes( x = intervals_wiener, y = toxicity_ratio)) + 
  geom_boxplot() + 
  geom_point(aes(x = intervals_wiener, y = toxicity_ratio_median_interval), color = "red") + 
  geom_abline(slope = 0.002794, intercept = 0.084672, color = "blue")+
  labs(title = "Sorcona 80s")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))
#l'idea sarebbe poi aggiungere una regressione su queste mediane

#proviamo a fare un questionability index sintetico
df <- df %>% mutate(synthetic_quest = runif(nrow(df), min = 0, max = 0.6))

ggplot(df, aes( x = intervals_wiener, y = synthetic_quest)) + 
  geom_boxplot() + 
  labs(title = "Sorcona 80s")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))

library(data.table)
library(dplyr)
library(urltools)
library(ggplot2)
library(ggridges)
library(viridis)
library(igraph)
library(Matrix)
library(ggExtra)
library(viridis)
library(qlcMatrix)
library(ggraph)
ggplot(df, aes(x = toxicity_ratio, y = synthetic_quest))+ 
         #geom_point()+
  stat_density_2d(geom = "polygon", contour = TRUE)+
  scale_fill_viridis(option = "magma", discrete=F,na.value="black")+
  theme_classic()+              
  labs(x="Average Audience Ideology",y="Average Source Score")+
  theme(legend.position = "none")+
  coord_fixed(ratio=0.2)


  
  
  geom_rect(data=data.frame(xmin = -1, xmax = 1, ymin = 0, ymax = 100),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#222021")+
  geom_point(aes(x=toxicity_ratio, y=reliability_score),size=-1)
  