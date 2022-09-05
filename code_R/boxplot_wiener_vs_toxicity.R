library(ggplot2)
library(dplyr)

df <- read.csv(file = '/Users/alessandroquattrociocchi/Desktop/test.csv')

intervals <- 10^seq(log10(min(df$wiener_index)-0.1),log10(max(df$wiener_index)+0.1),length.out = 15)

df <- df %>% mutate(intervals_wiener = cut(wiener_index, breaks = intervals, dig.lab = 10)) %>%
  group_by(intervals_wiener) %>% mutate(toxicity_ratio_interval = mean(toxicity_ratio)) %>%
  mutate(toxicity_ratio_median_interval = median(toxicity_ratio)) %>% ungroup()

regression_table <- 
  df %>% select(intervals_wiener,toxicity_ratio_median_interval) %>% arrange(intervals_wiener) %>%
  distinct() %>% ungroup() %>% mutate(rn = row_number())
model1 <- lm(regression_table$toxicity_ratio_median_interval[-1]~regression_table$rn[-1])
summary(model1)

png("/Users/alessandroquattrociocchi/Desktop/boxplot_wiener.png", units="in", width=7, height=7, res=320)
ggplot(df, aes( x = intervals_wiener, y = toxicity_ratio)) + 
  geom_boxplot() + 
  geom_point(aes(x = intervals_wiener, y = toxicity_ratio_median_interval), color = "red") + 
  geom_abline(slope = 0.002794, intercept = 0.084672, color = "blue")+
  labs(title = " ")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))
dev.off



 


