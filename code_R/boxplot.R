library(ggplot2)
library(dplyr)
library(data.table)
library(ggridges)
library(viridis)
library(ggExtra)
library(viridis)

df <- fread(file = '/Users/alessandroquattrociocchi/Git/Reliability-of-News-and-Toxicity-in-Twitter-Conversations/code_R/data/full_metrics_thesis.csv',colClasses="character")

df[,outlet_score:=as.numeric(outlet_score)]
df[,toxicity_ratio:=as.numeric(toxicity_ratio)]
df[,unique_users:=as.numeric(unique_users)]
df[,size:=as.numeric(size)]
df[,depth:=as.numeric(depth)]
df[,wiener_index:=as.numeric(wiener_index)]
df[,assortativity_tox:=as.numeric(assortativity_tox)]

c1  <- df$outlet_score
c2 <- rep(NA, length(c1))
c3 <- df$toxicity_ratio
c4 <- df$size
dd <- data.frame(c1,c2,c3,c4)


dd <- dd %>% mutate(c2 = replace(c2, c1<=20,'Very Low'), 
                    c2 = replace(c2, c1>20&c1<=40,'Low'), 
                    c2 = replace(c2, c1>40&c1<=60,'Mixed'), 
                    c2 = replace(c2, c1>60&c1<=80,'High'), 
                    c2 = replace(c2, c1>80&c1<=100,'Very High'))



toxicity_score_plot <-  ggplot((dd %>% filter(c4 > 10)), aes(y = c3, x = factor(c2, level = c('Very Low', 'Low', 'Mixed', 'High', 'Very High'))))+ 
  geom_boxplot(outlier.alpha = 0.2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=1.4, color="red", fill="red")+
  labs(
    y = "Toxicity Ratio",
    x = "",
    title = '', 
    subtitle = expression(atop(textstyle(""),atop(textstyle("")))))+
  #ylim(0,1)+
   theme_classic()+theme(text = element_text(size=18))
  

pdf("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/toxicity_score_plot.pdf")
toxicity_score_plot
dev.off()
