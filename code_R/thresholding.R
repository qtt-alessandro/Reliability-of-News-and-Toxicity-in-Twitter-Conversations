library(ggplot2)
library(dplyr)
library(data.table)
library(ggridges)
library(viridis)
library(ggExtra)
library(viridis)

df <- fread(file = '/Users/alessandroquattrociocchi/Git/free-speech-analysis/thresholds_plot.csv')
df

png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/thresholding.png", units="in", width=7, height=7, res=320)
ggplot(df, aes(x, y, color=label)) +
  geom_point(size=1) + 
  geom_line(size= .5) + 
  scale_y_continuous(breaks = round(seq(0, 8000, by = 1000),1))+
  scale_x_continuous(breaks = round(seq(0, 1, by = 0.05),1))+
  geom_vline(xintercept = 0.60, size = 0.3 , linetype = "dashed") +
  annotate("text", x=0.75, y=6000, label= "Toxic \n Zone") + 
  labs(
    y = "number of toxic comments",
    x = "toxicity threshold",
    title = '')
dev.off()