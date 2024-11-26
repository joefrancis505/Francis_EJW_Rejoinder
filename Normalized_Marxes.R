# ====================================================================================
# Normalized Marx Analysis (1878 and 1916)
# This script creates plots for the components of two normalized Synthetic Marxes 
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

# Function to create a single plot
create_single_plot <- function(data, numeric_cols, line_styles, ylim, ylab, label_positions) {
  plot(NULL, 
       xlim = c(1870, 1940),
       ylim = ylim,
       xlab = " ",
       ylab = ylab,
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  # Draw custom axes
  axis(1, at = seq(1870, 1940, by = 10), 
       labels = format_labels(seq(1870, 1940, by = 10)), 
       lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  axis(2, at = seq(ylim[1], ylim[2], by = ylim[2]/5), 
       labels = format_labels(seq(ylim[1], ylim[2], by = ylim[2]/5)), 
       lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  # Add box and vertical line
  box(lwd = 0.8)
  abline(v = 1917, lty = 2, lwd = 0.8)
  
  # Plot lines
  for (col in numeric_cols) {
    lines(data$year, data[[col]], 
          lty = line_styles[[col]]$lty,
          lwd = line_styles[[col]]$lwd)
  }
  
  # Add labels
  for (label in names(label_positions)) {
    text(label_positions[[label]]$x, 
         label_positions[[label]]$y, 
         label, 
         cex = 1.2)
  }
}

# Main Function to Create All Plots ------------------------------------------------
create_marx_normalized_plots <- function() {
  # Set plot dimensions
  plot_width <- 9.2  # inches
  plot_height <- 5   # inches
  
  # Load required library
  library(readODS)
  
  # Read data
  data_1878 <- read_ods("Normalized_Marxes.ods", sheet = "1878")
  data_1916 <- read_ods("Normalized_Marxes.ods", sheet = "1916")
  
  # Define numeric columns
  numeric_cols_1878 <- setdiff(names(data_1878), "year")
  numeric_cols_1916 <- setdiff(names(data_1916), "year")
  
  # Define line styles
  line_styles_1878 <- list(
    FerdinandLassalle = list(lty = 1, lwd = 0.5),
    MosesHess = list(lty = 2, lwd = 1),
    FriedrichList = list(lty = 1, lwd = 1),
    Ebbinghaus = list(lty = 6, lwd = 2.5)
  )
  
  line_styles_1916 <- list(
    FerdinandLassalle = list(lty = 1, lwd = 0.5),
    EduardBernstein = list(lty = 6, lwd = 2.5),
    AugustBebel = list(lty = 3, lwd = 1),
    OttoPfleiderer = list(lty = 4, lwd = 1),
    CarlStumpf = list(lty = 5, lwd = 1),
    HermannLotze = list(lty = 6, lwd = 1),
    FranzBrentano = list(lty = 1, lwd = 1),
    JosephLister = list(lty = 2, lwd = 2)
  )
  
  # Define label positions
  labels_1878 <- list(
    "Ferdinand Lassalle" = list(x = 1891.5, y = 2.25),
    "Ebbinghaus" = list(x = 1929.5, y = 1.35)
  )
  
  labels_1916 <- list(
    "Ferdinand Lassalle" = list(x = 1892, y = 0.87),
    "Eduard Bernstein" = list(x = 1930, y = 0.95)
  )
  
  # Create 1878 plots
  # PDF version
  pdf("Normalized_Marx_1878.pdf", width = plot_width, height = plot_height)
  setup_plot(plot_width, plot_height)
  create_single_plot(data_1878, numeric_cols_1878, line_styles_1878, 
                     ylim = c(0, 2.5), 
                     ylab = "N\uadgram share (1878 = 1 x weight)",
                     labels_1878)
  dev.off()
  
  # PNG version
  png("Normalized_Marx_1878.png", width = plot_width, height = plot_height,
      units = "in", res = 300)
  setup_plot(plot_width, plot_height)
  create_single_plot(data_1878, numeric_cols_1878, line_styles_1878, 
                     ylim = c(0, 2.5), 
                     ylab = "N\uadgram share (1878 = 1 x weight)",
                     labels_1878)
  dev.off()
  
  # Create 1916 plots
  # PDF version
  pdf("Normalized_Marx_1916.pdf", width = plot_width, height = plot_height)
  setup_plot(plot_width, plot_height)
  create_single_plot(data_1916, numeric_cols_1916, line_styles_1916, 
                     ylim = c(0, 1), 
                     ylab = "N\uadgram share (1916 = 1 x weight)",
                     labels_1916)
  dev.off()
  
  # PNG version
  png("Normalized_Marx_1916.png", width = plot_width, height = plot_height,
      units = "in", res = 300)
  setup_plot(plot_width, plot_height)
  create_single_plot(data_1916, numeric_cols_1916, line_styles_1916, 
                     ylim = c(0, 1), 
                     ylab = "N\uadgram share (1916 = 1 x weight)",
                     labels_1916)
  dev.off()
}

# Execute the function
create_marx_normalized_plots()