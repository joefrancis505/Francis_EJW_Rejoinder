# Get the directory of this script
sourceDir <- getSrcDirectory(function(dummy) {dummy})

# Set the working directory to the script's location
setwd(sourceDir)

# Load necessary libraries
library(strucchange)

# Read the CSV file
data <- read.csv("all_authors_with_citations_and_indicators.csv")

# Ensure the Year column is numeric
data$Year <- as.numeric(as.character(data$Year))

# Filter data for Karl Marx from 1867 onwards
data_filtered <- subset(data, Name == "Karl Marx" & Year >= 1867)

# Handle potential missing data
data_filtered <- na.omit(data_filtered)

# Convert to time series object and apply log transformation
ts_data <- ts(log(data_filtered$cite_English), 
              start = min(data_filtered$Year), 
              end = max(data_filtered$Year), 
              frequency = 1)

# Perform Bai-Perron test on log-transformed data
bp_test <- breakpoints(ts_data ~ 1)

# Summary of the test
summary(bp_test)

# Plot the results
plot(bp_test)

# Print the optimal number of breakpoints
print(bp_test)

# Extract the years where breaks occurred
breakpoints <- breakdates(bp_test)
print(breakpoints)

# Plot the original log-transformed series with breakpoints
plot(ts_data)
lines(bp_test)

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, 
                       left_margin = 1.2, right_margin = 1.2) {
  # Set the plot area size
  par(pin = c(width - left_margin - right_margin, 
              height - top_margin - bottom_margin))
  
  # Set margins in inches
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  
  # Set other plot parameters
  par(family = "sans",
      cex = 1.2,
      cex.axis = 1.2,
      cex.lab = 1.2,
      tck = 0.01,
      lwd = 0.8,
      las = 1,
      mgp = c(3, 0.8, 0))
}

# Function to format labels
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to create the structural break plot
create_structural_break_plot <- function() {
  # Set plot dimensions
  plot_width <- 9.2  # inches
  plot_height <- 5   # inches
  
  # Set up plot parameters
  setup_plot(plot_width, plot_height)
  
  # Read the CSV file
  data <- read.csv("all_authors_with_citations_and_indicators.csv")
  
  # Ensure the Year column is numeric
  data$Year <- as.numeric(as.character(data$Year))
  
  # Filter data for Karl Marx from 1867 onwards
  data_filtered <- subset(data, Name == "Karl Marx" & Year >= 1867 & Year <= 2000)
  
  # Handle potential missing data
  data_filtered <- na.omit(data_filtered)
  
  # Convert to time series object and apply log transformation
  ts_data <- ts(log(data_filtered$cite_English), 
                start = min(data_filtered$Year), 
                end = max(data_filtered$Year), 
                frequency = 1)
  
  # Perform Bai-Perron test on log-transformed data
  bp_test <- breakpoints(ts_data ~ 1)
  
  # Create the plot
  plot(ts_data, 
       type = "n",
       xlab = " ",
       ylab = "Log n\uadgram share",
       xlim = c(1860, 2000),
       ylim = c(-19, -13),
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  # Draw custom axes
  axis(1, at = seq(1860, 2000, by = 20), 
       labels = format_labels(seq(1860, 2000, by = 20)), 
       lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  axis(2, at = pretty(range(ts_data)), 
       labels = format_labels(pretty(range(ts_data))), 
       lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  # Add box
  box(lwd = 0.8)
  
  # Plot data line
  lines(ts_data, col = "black", lwd = 1)
  
  # Add breakpoints
  breaks <- breakdates(bp_test)
  abline(v = breaks, lty = 2, col = "black")

  # Save the plot as PDF
  dev.copy(pdf, "Structural_breaks.pdf", width = plot_width, height = plot_height, encoding = "ISOLatin1")
  dev.off()
}

# Call the function to create the plot
create_structural_break_plot()