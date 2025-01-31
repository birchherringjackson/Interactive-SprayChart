library(ggplot2)
library(dplyr)
library(plotly)

# Read the batted ball data from the CSV
ReynaHittingData <- read.csv("Reyna Hitting Data.csv", header = TRUE)

# Convert the Direction column (in degrees) from degrees to radians
ReynaHittingData <- ReynaHittingData %>%
  mutate(direction_radians = Direction * pi / 180)

# Convert polar coordinates (distance, direction) to Cartesian coordinates (x, y)
ReynaHittingData <- ReynaHittingData %>%
  mutate(
    x = Distance..Feet. * sin(direction_radians),
    y = Distance..Feet. * cos(direction_radians),
    color = case_when(
      # Check if the point meets all criteria for gold
      ExitSpeed..MPH. > 85 & 
        between(as.numeric(substring(Spin.Direction, 1, 2)), 10, 14) & # Spin Direction in "clock" format
        SpinRate >= 1500 & SpinRate <= 2500 ~ "gold",
      ExitSpeed..MPH. > 85 ~ "green", # Green if ExitSpeed > 85 but not all gold criteria
      TRUE ~ "red" # Default to red
    )
)
# Create the baseball field plot (reusing previous code)
baseball_field <- ggplot() +
  # Draw the infield diamond
  geom_polygon(data = diamond_coords, aes(x, y), fill = "lightblue", color = "black", size = 1.2) +
  # Draw the outfield arc
  geom_path(data = outfield_arc, aes(x, y), color = "black", size = 1.2) +
  # Draw the left foul line
  geom_line(data = foul_lines_left, aes(x, y), color = "black", size = 1.2) +
  # Draw the right foul line
  geom_line(data = foul_lines_right, aes(x, y), color = "black", size = 1.2) +
  # Fix the aspect ratio
  coord_fixed() +
  # Add title and clean up the background
  theme_minimal() +
  labs(
    title = "Reyna Hitting Session 1/20",
    x = "Feet",
    y = "Feet"
  )

# Add batted ball points with hover information
baseball_field <- baseball_field +
  geom_point(
    data = ReynaHittingData,
    aes(
      x = x, y = y,
      color = color,
      text = paste(
        "EV: ", ExitSpeed..MPH., " MPH",
        "<br>LA: ", Angle, "°",
        "<br>Distance: ", Distance..Feet., " ft",
        "<br>Spin Direction: ", Spin.Direction,
        "<br>Spin Rate: ", SpinRate, " RPM"
      )
    ),
    size = 2, alpha = 0.7
  ) +
  scale_color_manual(
    values = c("gold" = "gold", "green" = "green", "red" = "red"),
    breaks = c("gold", "green", "red"),
    labels = c("Perfect Perfect", "Hard-Hit", "Non-Hard Hit")  # This is where the label change happens
  ) +
  labs(color = "Batted Ball Quality") +
  theme_minimal()


# Convert ggplot to a plotly object for interactivity
interactive_plot <- ggplotly(baseball_field, tooltip = "text")

# Display the interactive plot
interactive_plot

library(htmlwidgets)
saveWidget(interactive_plot, "Reyna Hitting Session.html")
htmlwidgets::saveWidget(interactive_plot, "Interactive SprayChart.html", selfcontained = TRUE)









