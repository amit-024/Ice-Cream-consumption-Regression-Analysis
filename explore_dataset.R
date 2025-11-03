library(ggplot2) # data visualization
library(GGally) # pairwise plots
library(lmtest) # heteroscedasticity tests
library(car) # Durbin-Watson & VIF
library(olsrr) # model selection & diagnostics
library(dplyr) # data handling
library(MASS)
library(lattice)
library(AICcmodavg)
library(plotly)
library(corrplot)   # for correlation heatmap
library(reshape2)


x <- read.table("icecream.txt", header = TRUE, skip = 14)
write.csv(x, file = "icecream_dataset.csv")  
ice <- read.csv("icecream_dataset.csv")[-1]
head(ice)
#par(bg = "white")


# =========================
# 1. Quick Summary
# =========================
summary(ice)        # numerical summary
str(ice)            # structure of dataset

# =========================
# 2. Pair Plot
# =========================
#theme_set(theme_gray(base_size = 16))
#png("pair_pair.png", width=2000, height=2000, bg="white", type="cairo", antialias="none")
 ggpairs(
  ice,
  switch = "both",
  lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)),
  cardinality_threshold=50
)

p + theme(
  text = element_text(size = 15),        # all text bigger
  axis.text = element_text(size = 11),   # axis tick labels
  strip.text = element_text(size = 12)   # facet labels (diagonal variable names)
)
ggsave("pair_pair.png", p, bg = "white",width = 20, height = 20, units = "in", dpi = 500)

dev.off()
# =========================
# 3. Correlation Heatmap
# =========================
ice$Period <- as.numeric(as.character(ice$Period))
cor_mat <- cor(ice[, -1])   # now works
#png("corr_heatmap.png", width=200, height=2000, bg="white", type="cairo", antialias="none")
corrplot(cor_mat,
         method = "color",
         type = "upper",
         addCoef.col = "black",   # correlation numbers in black
         #number.cex = 5,        # scale correlation numbers (default = 1)
         tl.col = "black",        # text labels in black
         tl.srt = 45,             # rotate labels
         #tl.cex = 5,            # scale variable labels
         title = "Correlation Heatmap")
dev.off()
# =========================
# 4. Scatter Plots: X vs Y
# =========================
# Consumption vs Price
ggplot(ice, aes(x = Price, y = Consumption)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  labs(title = "Consumption vs Price")
ggsave("y_price.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

# Consumption vs Income
ggplot(ice, aes(x = Income, y = Consumption)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  labs(title = "Consumption vs Income")
ggsave("y_income.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

# Consumption vs Temp
ggplot(ice, aes(x = Temp, y = Consumption)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  labs(title = "Consumption vs Temp")
ggsave("y_temp.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

# =========================
# 5. Boxplots
# =========================
# If Period is categorical, convert to factor:
ice$Period <- factor(ice$Period)

ggplot(ice, aes(x = Period, y = Consumption)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Consumption across Periods")
ggsave("y_period.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

ggplot(ice, aes(x = Period, y = Temp)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Temperature across Periods")
p=ggsave("temp_period.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

# =========================
# 6. Density Plots
# =========================
ggplot(melt(ice[, -1]), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Variables")
ggsave("density.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

################################
# histogram of response variable
###############################


ggplot(ice, aes(x = Consumption)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Consumption")
ggsave("y_hist.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)


