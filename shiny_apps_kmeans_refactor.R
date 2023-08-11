# This is a hard-coded refactor of the shiny apps k-means example, mainly to 
# understand the code and as a template should it come in handy

# Load required libraries
library(ggplot2)
library(viridis)

# Select columns 1 and 3 from the iris dataset
selectedData = iris[, c(1, 3)]

# Set the number of clusters
k = 3

# Perform k-means clustering
clusters = kmeans(selectedData, k)

# Create a ggplot object for the initial scatter plot
plot_initial = ggplot(selectedData, aes(x = Sepal.Length, 
                                        y = Petal.Length)) +
  geom_point(aes(color = factor(clusters$cluster)), 
             shape = 20, 
             size = 3, 
             alpha = 0.6) +  # Set alpha value to whatever you prefer
  scale_color_viridis(discrete = TRUE, name = "Cluster") +  # change label as needed
  labs(title = "K-Means Clustering of Iris Data", 
       x = "Sepal Length", 
       y = "Petal Length")

# Add cluster centers to the plot
plot_final = plot_initial +
  geom_point(data = data.frame(Sepal.Length = clusters$centers[, 1], 
                               Petal.Length = clusters$centers[, 2]),
             aes(x = Sepal.Length, 
                 y = Petal.Length), 
             color = "black", shape = 4, size = 3) +
  theme_minimal()

# Display the plot
print(plot_final)
