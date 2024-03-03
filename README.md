# Soil-data-code-2-factor-categorical-variables
Bikas<-read.csv("E:/Surya sir folder/SS1.csv", header = TRUE)
Bikas
colnames(Bikas)
require(gvlma)
Bikas$DepthS <- factor(Bikas$Depths)
Bikas$Ecosystem <- factor(Bikas$Ecosystem)
model <- lm(Fungal.popn.10.3.~Ecosystem * Depths, data = Bikas)
model
gvlma(model)
result <- aov(Fungal.popn.10.3. ~ Ecosystem * Depths, data = Bikas)
summary(result)
# TukeyHSD(result)
# pairwise.t.test(Bikas$EC, Bikas$Ecosystem)
# pairwise.t.test(Bikas$EC, Bikas$Depths)
kruskal.test(Fungal.popn.10.3. ~ Ecosystem, data = Bikas)
kruskal.test(Fungal.popn.10.3. ~ Depths, data =  Bikas)

# # Calculate the mean of Bulk Density for each level of Ecosystem
# ecosystem_means <- tapply(Bikas$EC, Bikas$Ecosystem, mean)
# ecosystem_means
# # Calculate the mean of Bulk Density for each level of Depth
# depth_means <- tapply(Bikas$EC, Bikas$Depths, mean)
# depth_means
# Calculate the mean of Bulk Density for each combination of Ecosystem and Depth
mean_density <- aggregate(Fungal.popn.10.3. ~ Ecosystem + Depths, data = Bikas, FUN = mean)
mean_density
library(ggplot2)

# Create a faceted plot
ggplot(mean_density, aes(x = Depths, y = Fungal.popn.10.3.)) +
  geom_bar(stat = "identity", fill = "orange", color = "green") +
  facet_wrap(~ Ecosystem, scales = "free_y") +
  labs(x = "Depth", y = "Fungal.popn.10.3.", title = "Fungal.popn.10.3. by Ecosystem and Depth")+
  scale_y_continuous(limits = c(0, 10))
