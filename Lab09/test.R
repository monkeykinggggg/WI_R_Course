library(plotly)
data <- data.frame(x = rnorm(100), y = rnorm(100))

plot_ly(
  data = data,
  x = ~x,
  y = ~y,
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, color = ~x, colorscale = "Viridis")
)