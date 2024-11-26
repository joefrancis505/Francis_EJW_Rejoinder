# ====================================================================================
# Structural Break Analysis of Marx's N-Gram Share (1867-2000)
# This script analyzes structural breaks in the time series of English n-gram share 
# for "Karl Marx" using the Bai-Perron test
# ====================================================================================

# Set up environment ----------------------------------------------------------------
# Get the directory of this script and set working directory
sourceDir <- getSrcDirectory(function(dummy) {dummy})
setwd(sourceDir)

# Load required libraries
library(strucchange)

# Helper Functions -----------------------------------------------------------------
# Function to set up plot parameters for publication-quality figures
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, 
                       left_margin = 1.2, right_margin = 1.2) {
  par(pin = c(width - left_margin - right_margin, 
              height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans",
      cex = 1.2,
      cex.axis = 1.2,
      cex.lab = 1.2,
      tck = 0.01,
      lwd = 0.8,
      las = 1,
      mgp = c(3, 0.8, 0))
}

# Function to format axis labels
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to create the actual plot
create_plot <- function(ts_data, breaks) {
  # Initialize plot
  plot(ts_data, 
       type = "n",
       xlab = " ",
       ylab = "Log n\uadgram share",
       xlim = c(1860, 2000),
       ylim = c(-19, -13),
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  # Add custom axes
  axis(1, at = seq(1860, 2000, by = 20), 
       labels = format_labels(seq(1860, 2000, by = 20)), 
       lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  axis(2, at = pretty(range(ts_data)), 
       labels = format_labels(pretty(range(ts_data))), 
       lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  # Add plot elements
  box(lwd = 0.8)
  lines(ts_data, col = "black", lwd = 1)
  
  # Add breakpoint lines
  abline(v = breaks, lty = 2, col = "black")
}

# Main Analysis Function -----------------------------------------------------------
create_structural_break_analysis <- function(save_pdf = TRUE, save_png = TRUE) {
  # Set plot dimensions
  plot_width <- 9.2  # inches
  plot_height <- 5   # inches
  
  # Read and prepare data
  data <- read.csv("all_authors_with_citations_and_indicators.csv")
  data$Year <- as.numeric(as.character(data$Year))
  
  # Filter data for Karl Marx (1867-2000)
  data_filtered <- subset(data, Name == "Karl Marx" & Year >= 1867 & Year <= 2000)
  data_filtered <- na.omit(data_filtered)
  
  # Create time series
  ts_data <- ts(log(data_filtered$cite_English), 
                start = min(data_filtered$Year), 
                end = max(data_filtered$Year), 
                frequency = 1)
  
  # Perform Bai-Perron test
  bp_test <- breakpoints(ts_data ~ 1)
  breaks <- breakdates(bp_test)
  
  # Save as PDF
  if(save_pdf) {
    pdf("Structural_breaks.pdf", width = plot_width, height = plot_height)
    setup_plot(plot_width, plot_height)
    create_plot(ts_data, breaks)
    dev.off()
  }
  
  # Save as PNG
  if(save_png) {
    png("Structural_breaks.png", 
        width = plot_width, height = plot_height,
        units = "in", res = 300)
    setup_plot(plot_width, plot_height)
    create_plot(ts_data, breaks)
    dev.off()
  }
  
  # Create plot in the current device (usually screen)
  setup_plot(plot_width, plot_height)
  create_plot(ts_data, breaks)
  
  # Return analysis results
  return(list(
    breakpoints = breaks,
    test_summary = summary(bp_test),
    time_series = ts_data
  ))
}

# Execute Analysis ----------------------------------------------------------------
results <- create_structural_break_analysis(save_pdf = TRUE, save_png = TRUE)

# Print Results ------------------------------------------------------------------
cat("\nStructural Break Analysis Results:\n")
cat("=================================\n")
print(results$test_summary)
cat("\nBreak Points Occurred in Years:\n")
print(results$breakpoints)