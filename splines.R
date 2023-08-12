# Using splines
library(splines)
library(ggplot2)
library(patchwork)

n = 100
set.seed(101)
x = sort(rnorm(n, sd = 2))
# linear predictor
mu = 2 + 0.09*x - 0.6*x^2 + 0.17*x^3 
y = rpois(n, exp(mu))

# Plot
plot(x, y)
lines(x, exp(mu))

# Evidently fitting a polynomial is the answer here but we wouldn't know how
# the distribution was generated (if it was generated as a polynomial), therefore
# cubic splines would be useful in this context

# The thing to bear in mind is that the degrees of freedom we choose alows more
# knots to be fitted with the splines. Since we generated our data with a cubic
# polynomial, a model with 3 or more df should do best

# Let's compare a log linear model, a model with two df and one with 3 df
# log linear model
m1 = glm(y ~ x, family = "poisson")
m1pred = predict(m1, type = "response")

# non-linear models
m2 = glm(y ~ ns(x,2), family = "poisson")
m2pred = predict(m2, type = "response")

m3 = glm(y ~ ns(x,3), family = "poisson")
m3pred = predict(m3, type = "response")

# Create ggplot2 plots for each model with adjusted line thickness and transparency
p1 = ggplot(data.frame(x, y), 
            aes(x = x, 
                y = y)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_line(aes(y = m1pred), 
            color = "#440154", linewidth = 0.7) +
  theme_minimal() +
  ggtitle("Log Linear Model")

p2 = ggplot(data.frame(x, y), 
            aes(x = x, 
                y = y)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_line(aes(y = m2pred), 
            color = "#21918c", linewidth = 0.7) +
  theme_minimal() +
  ggtitle("Cubic Spline (2 df)")

p3 = ggplot(data.frame(x, y), 
            aes(x = x, 
                y = y)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_line(aes(y = m3pred), 
            color = "#fde725", linewidth = 0.7) +
  theme_minimal() +
  ggtitle("Cubic Spline (3 df)")

# Arrange the plots side by side using patchwork
(p1 | p2 | p3) + plot_layout(ncol = 3)

ggsave("./outputs/cubic_splines.jpg", 
       plot = last_plot(),
       dpi = 600)

# Which model is best?
AIC(m1)
#[1] 553.1264
AIC(m2)
#[1] 431.332
AIC(m3)    
#[1] 378.2775   

# The lowest AIC score corresponds to the model with 3 df/cubic splines