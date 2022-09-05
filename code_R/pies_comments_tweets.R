library(dplyr)
library(data.table)
library(ggridges)
library(viridis)
library(ggExtra)
library(viridis)
library(scales)
#very low/low/mixed/high/very high

df <- fread(file = '/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/data/metrics_data.csv',colClasses="character")

df[,outlet_score:=as.numeric(outlet_score)]
df[,toxicity_ratio:=as.numeric(toxicity_ratio)]
df[,unique_users:=as.numeric(unique_users)]
df[,size:=as.numeric(size)]
df[,depth:=as.numeric(depth)]
df[,wiener_index:=as.numeric(wiener_index)]
df[,assortativity_tox:=as.numeric(assortativity_tox)]

c <- table(df$outlet_flag)



#### Tweets
val <- c(15910, 9428)
labels <- c("non reliable", "reliable")

df <- data.frame(val, labels)

png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/pie_tweets.png", units="in", width=7, height=7, res=320)
ggplot(df, aes(x="", y=val, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_col(color = "black") +
  geom_text(aes(label = val),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+guides(fill = guide_legend(title = "Labels"))+
  ggtitle("Tweets") +
  theme_void()
dev.off()


#Number of Comments
val <- c(1126346, 2469944)
labels <- c("non reliable", "reliable")

df <- data.frame(val, labels)

png("/Users/alessandroquattrociocchi/Git/free-speech-analysis/plots/figures/pie_comments.png", units="in", width=7, height=7, res=320)
ggplot(df, aes(x="", y=val, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_col(color = "black") +
  geom_text(aes(label = val),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+guides(fill = guide_legend(title = "Labels"))+
  ggtitle("Comments") +
  theme_void()
dev.off()




