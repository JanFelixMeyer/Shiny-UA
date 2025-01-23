#-----------------------------------------------------------------------------
# Define ggplot2-theme
#-----------------------------------------------------------------------------
my_theme = theme_minimal()
my_theme = my_theme + theme(
  
  # Title and axis labels
  plot.title = element_text(size = 16),    # Increase plot title size
  axis.title = element_text(size = 14),    # Increase axis titles size
  axis.text = element_text(size = 12),     # Increase axis tick labels size
  
  # Legend text
  legend.title = element_blank(),
  legend.text = element_text(size = 12),   # Increase legend text size
  
  # Other settings
  legend.position = "bottom",              # Place the legend below the chart
  legend.direction = "horizontal",         # Arrange legend items horizontally
  strip.text = element_text(size = 14)     # Increase facet label size (if using facets)
)