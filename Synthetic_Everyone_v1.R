# The synthetic everyone maker

# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Required packages
packages <- c("tidyverse", "Synth")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear the console
cat("\014")  # This simulates Ctrl+L

# Create Everyone_results directory and subdirectories
dir.create("Everyone_results", showWarnings = FALSE)
dir.create("Everyone_results/pdf", showWarnings = FALSE, recursive = TRUE)
dir.create("Everyone_results/png", showWarnings = FALSE, recursive = TRUE)
dir.create("Everyone_results/txt", showWarnings = FALSE, recursive = TRUE)

# Function to save synthetic control details
save_synth_details <- function(synth_out, dataprep_out, author) {
  output_file <- file.path("Everyone_results/txt", paste0(author, ".txt"))
  sink(output_file)
  cat(paste0("Synthetic Control Analysis for ", author, "\n\n"))
  
  # Write weights
  cat("Synthetic Control Weights:\n")
  solution_w <- synth_out$solution.w
  all_names <- dataprep_out$names.and.numbers$unit.names
  all_numbers <- dataprep_out$names.and.numbers$unit.numbers
  name_map <- setNames(all_names, all_numbers)
  control_numbers <- as.numeric(rownames(solution_w))
  
  weights_df <- data.frame(
    Name = name_map[as.character(control_numbers)],
    Weight = as.numeric(solution_w)
  )
  weights_df <- weights_df[order(-weights_df$Weight), ]
  weights_df$Weight <- sprintf("%.6f", weights_df$Weight)
  significant_weights <- weights_df[as.numeric(weights_df$Weight) > 0.001, ]
  print(significant_weights, row.names = FALSE)
  
  # Write predictor balance
  cat("\nPredictor Balance:\n")
  synth.tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  print(synth.tables$tab.pred)
  
  # Write MSPE
  cat("\nMean Squared Prediction Error (MSPE):", synth_out$loss.w, "\n")
  
  sink()
}

# Function to create plot
create_synth_plot <- function(synth_out, dataprep_out) {
  # Extract data
  dataprep <- dataprep_out
  synth <- synth_out
  
  Y1plot <- dataprep$Y1plot
  Y0plot <- dataprep$Y0plot
  
  names.and.numbers <- dataprep$names.and.numbers
  tr.units <- names.and.numbers$tr.units
  
  synthetic <- Y0plot %*% synth$solution.w
  
  # Set up plot parameters with consistent font sizes
  par(mai = c(0.6, 1.2, 0.2, 1.2),
      family = "sans",
      cex = 1.2,  
      cex.axis = 1.2,  
      cex.lab = 1.2,  
      tck = 0.01,
      lwd = 0.8,
      las = 1,
      mgp = c(3.5, 0.8, 0)) 
  
  # Calculate y-axis range with larger gap at top
  y_range <- range(c(Y1plot, synthetic))
  y_gap <- diff(y_range) * 0.25
  y_limits <- c(y_range[1], y_range[2] + y_gap)
  
  # Create the plot
  plot(1878:2000, Y1plot[, 1],
       ylim = y_limits,
       xlim = c(1878, 2000),
       type = "n",
       xlab = "",
       ylab = "N\uadgram share (%)",
       xaxs = "i",
       yaxs = "i",
       xaxt = "n",
       yaxt = "n")
  
  # Add custom x-axis
  axis(1, at = seq(1880, 2000, by = 20), cex.axis = 1.2)
  
  # Add custom y-axis with scientific notation and replaced minus signs
  y_ticks <- pretty(y_limits)
  y_labels <- format(y_ticks, scientific = TRUE)
  y_labels <- gsub("-", "\uad", y_labels)
  axis(2, at = y_ticks, 
       labels = y_labels, 
       las = 0,
       cex.axis = 1.2)

  # Add box
  box(lwd = 0.8)
  
  # Add lines with increased thickness
  lines(1878:2000, Y1plot[, 1], lwd = 2)  
  lines(1878:2000, synthetic[, 1], lty = 2, lwd = 2)  
  
  # Add vertical line at 1917
  abline(v = 1917, lty = 2, lwd = 0.8)
  
  # Add legend
  legend("topright", 
         legend = c("Actual", "Synthetic"),
         lty = c(1, 2),
         lwd = 1,
         bty = "n",
         cex = 1.2)
}

# Function to save plots in both formats
save_synth_plots <- function(synth_out, dataprep_out, author) {
  # Save PDF
  pdf(file = file.path("Everyone_results/pdf", paste0(author, ".pdf")), 
      width = 9.2, height = 5)
  create_synth_plot(synth_out, dataprep_out)
  dev.off()
  
  # Save PNG
  png(file = file.path("Everyone_results/png", paste0(author, ".png")),
      width = 9.2, height = 5, units = "in", res = 300)
  create_synth_plot(synth_out, dataprep_out)
  dev.off()
}

# Function to perform synthetic control analysis
perform_synth_analysis <- function(data, treated_author) {
  tryCatch({
    authors_to_exclude <- if(treated_author == "Karl Marx") {
      c("Karl Marx", "Marx")
    } else if(treated_author == "Marx") {
      c("Marx", "Karl Marx")
    } else {
      c(treated_author, "Marx")
    }
    
    # Define predictors early
    predictors <- c("YearofPublication", "wrote_English", "wrote_German", "wrote_French", 
                    "wrote_Greek", "wrote_Latin", "wrote_Italian", "wrote_Spanish", 
                    "YearofTranslationtoEnglish", "Socialist", "Political")
    
    character_vars <- c("wrote_English", "wrote_German", "wrote_French", "wrote_Greek", 
                        "wrote_Latin", "wrote_Italian", "wrote_Spanish", "Socialist", "Political")
    
    # Add data validation for the treated author
    treated_data <- data %>% 
      filter(Name == treated_author) %>%
      arrange(Year)
    
    if (nrow(treated_data) == 0) {
      stop(paste("No data found for author:", treated_author))
    }
    
    donor_pool <- data %>% filter(!Name %in% authors_to_exclude)
    selected_authors <- unique(donor_pool$Name)
    
    selected_data <- bind_rows(
      data %>% filter(Name == treated_author),
      data %>% filter(Name %in% selected_authors)
    )
    
    # Handle missing values
    for (var in c(predictors, "cite_English")) {
      if (var %in% c("YearofPublication", "YearofTranslationtoEnglish")) {
        selected_data[[var]][is.na(selected_data[[var]])] <- mean(selected_data[[var]], na.rm = TRUE)
      } else {
        selected_data[[var]] <- ifelse(is.na(selected_data[[var]]), 0, selected_data[[var]])
      }
    }
    
    # Data preparation
    selected_data <- selected_data %>%
      group_by(Name) %>%
      mutate(Author_ID = cur_group_id()) %>%
      ungroup()
    
    for (var in character_vars) {
      selected_data[[var]] <- as.numeric(as.factor(selected_data[[var]])) - 1
    }
    
    special_predictors <- list(
      list("cite_English", 1914:1916, "mean"),
      list("cite_English", 1908:1910, "mean"),
      list("cite_English", 1902:1904, "mean"),
      list("cite_English", 1896:1898, "mean"),
      list("cite_English", 1890:1892, "mean"),
      list("cite_English", 1884:1886, "mean"),
      list("cite_English", 1878:1880, "mean")
    )
    
    treated_id <- selected_data %>%
      filter(Name == treated_author) %>%
      distinct(Author_ID) %>%
      pull(Author_ID)
    
    control_ids <- selected_data %>%
      filter(Name != treated_author) %>%
      distinct(Author_ID) %>%
      pull(Author_ID)
    
    dataprep_out <- dataprep(
      foo = as.data.frame(selected_data),
      predictors = predictors,
      predictors.op = "mean",
      special.predictors = special_predictors,
      dependent = "cite_English",
      unit.variable = "Author_ID",
      time.variable = "Year",
      treatment.identifier = treated_id,
      controls.identifier = control_ids,
      time.predictors.prior = 1878:1916,
      time.optimize.ssr = 1878:1916,
      unit.names.variable = "Name",
      time.plot = 1878:2000
    )
    
    synth_out <- synth(data.prep.obj = dataprep_out)
    
    # Save results
    save_synth_details(synth_out, dataprep_out, treated_author)
    save_synth_plots(synth_out, dataprep_out, treated_author)
    
  }, error = function(e) {
    cat("Error processing", treated_author, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Main analysis function
main <- function() {
  # Read data
  data <- read.csv("all_authors_with_citations_and_indicators.csv", 
                   header = TRUE, stringsAsFactors = FALSE)
  
  # Basic data cleaning
  required_columns <- c("Name", "Year", "cite_English", "YearofPublication", 
                        "wrote_English", "wrote_German", "wrote_French", "wrote_Greek", 
                        "wrote_Latin", "wrote_Italian", "wrote_Spanish", 
                        "YearofTranslationtoEnglish", "Socialist", "Political")
  
  data <- data %>%
    select(all_of(required_columns)) %>%
    mutate(
      Year = as.integer(Year),
      cite_English = as.numeric(cite_English),
      YearofPublication = as.integer(YearofPublication),
      YearofTranslationtoEnglish = as.integer(YearofTranslationtoEnglish)
    ) %>%
    filter(Year >= 1878, Year <= 2000)
  
  # Process all authors
  all_authors <- unique(data$Name)
  for (author in all_authors) {
    perform_synth_analysis(data, author)
  }
}

# Main analysis function
main <- function() {
  # Read data
  data <- read.csv("all_authors_with_citations_and_indicators.csv", 
                   header = TRUE, stringsAsFactors = FALSE)
  
  # Basic data cleaning
  required_columns <- c("Name", "Year", "cite_English", "YearofPublication", 
                        "wrote_English", "wrote_German", "wrote_French", "wrote_Greek", 
                        "wrote_Latin", "wrote_Italian", "wrote_Spanish", 
                        "YearofTranslationtoEnglish", "Socialist", "Political")
  
  data <- data %>%
    select(all_of(required_columns)) %>%
    mutate(
      Year = as.integer(Year),
      cite_English = as.numeric(cite_English),
      YearofPublication = as.integer(YearofPublication),
      YearofTranslationtoEnglish = as.integer(YearofTranslationtoEnglish)
    ) %>%
    filter(Year >= 1878, Year <= 2000)
  
  # Process all authors
  all_authors <- unique(data$Name)
  for (author in all_authors) {
    perform_synth_analysis(data, author)
  }
}

# Run the analysis
main()