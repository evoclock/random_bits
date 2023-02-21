# Heatmaps in base R
library(ggplot2)
library(viridis)
library(tidyr)
library(tibble)
library(dplyr)
library(tidylog)

dat = as.matrix(mtcars)

# Normalise the data
heatmap(dat, scale="column")


# Bypass hierarchical clustering and just generate a heatmap based on the
# matrix. Colv and Rowv set to NA maintains the matrix structure

heatmap(dat, Colv = NA, Rowv = NA, scale="column")


# Using a different colour palette.

heatmap(dat, scale="column", col = viridis(50))


# Heatmaps with ggplot

x = LETTERS[1:20]
y = paste0("var", seq(1,20))
data = expand.grid(X=x, Y=y)
data$Z = runif(400, 0, 5)

plot2 = ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal()

plot2


# Heatmap from wide formats need to be put in long format to be used in
# ggplot

dat2 = volcano %>%
  as_tibble() %>%
  rowid_to_column(var="X") %>% # convert row names or IDs to column as variable X
  gather(key="Y", value="Z", -1) %>% # gather
  mutate(Y=as.numeric(gsub("V","",Y))) # change Y to numeric

plot3 = ggplot(dat2, aes(X, Y, fill= Z)) + 
  geom_tile() +
  theme_minimal() +
  theme(legend.position="none")
plot3
