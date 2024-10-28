# The Gaussian Marx maker

# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
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

# Create directory structure
dir.create("Gaussian_Marx", showWarnings = FALSE)
dir.create("Gaussian_Marx/pdf", showWarnings = FALSE, recursive = TRUE)
dir.create("Gaussian_Marx/png", showWarnings = FALSE, recursive = TRUE)
dir.create("Gaussian_Marx/txt", showWarnings = FALSE, recursive = TRUE)

# Function to calculate volatility
calculate_volatility <- function(data, citation_column = "cite_English") {
  data %>%
    arrange(Name, Year) %>%
    group_by(Name) %>%
    mutate(
      prev_cite = lag(!!sym(citation_column)),
      relative_change = (!!sym(citation_column) - prev_cite) / prev_cite,
      log_return = log(!!sym(citation_column) / prev_cite)
    ) %>%
    summarize(
      mean_cite = mean(!!sym(citation_column), na.rm = TRUE),
      sd_relative_change = sd(relative_change, na.rm = TRUE),
      sd_log_return = sd(log_return, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarize(
      median_sd_relative_change = median(sd_relative_change, na.rm = TRUE),
      median_sd_log_return = median(sd_log_return, na.rm = TRUE)
    )
}

# Function to get user input for donor pool size
get_donor_pool_size <- function(max_donors) {
  cat("Welcome to the Gaussian Marx Maker!\n\n")
  cat("You can have a maximum of", max_donors, "donors in the pool. How many would you like to include? ")
  repeat {
    size <- as.integer(readline(prompt = ""))
    if (!is.na(size) && size > 0 && size <= max_donors) {
      return(size)
    }
    cat("Please enter a valid number between 1 and", max_donors, ".\n")
  }
}

# Function to ask if user wants to use Gaussian random walks
use_gaussian_random_walks <- function() {
  repeat {
    response <- tolower(readline(prompt = "Would you like to use Gaussian random walks for the donors' n-gram shares? (y/n) "))
    if (response %in% c("y", "n")) {
      return(response == "y")
    }
    cat("Please answer 'y' or 'n'.\n")
  }
}

# Function to ask if user wants to set a seed
use_custom_seed <- function() {
  repeat {
    response <- tolower(readline(prompt = "Would you like to set a seed for reproducibility? (y/n) "))
    if (response %in% c("y", "n")) {
      if (response == "y") {
        seed <- as.integer(readline(prompt = "What would you like the seed to be? "))
        return(list(use_seed = TRUE, seed_value = seed))
      } else {
        return(list(use_seed = FALSE, seed_value = NULL))
      }
    }
    cat("Please answer 'y' or 'n'.\n")
  }
}

# Function to perform Gaussian random walk
perform_gaussian_random_walk <- function(initial_value, years, volatility) {
  values <- numeric(length(years))
  values[1] <- initial_value
  
  for (i in 2:length(years)) {
    change <- rnorm(1, mean = 0, sd = volatility * values[i-1])
    new_value <- max(values[i-1] + change, .Machine$double.eps)
    values[i] <- new_value
  }
  
  return(values)
}

# Function to create plot
create_synth_plot <- function(synth_out, dataprep_out, num_authors, seed = NULL) {
  # Extract data
  dataprep <- dataprep_out
  synth <- synth_out
  
  Y1plot <- dataprep$Y1plot
  Y0plot <- dataprep$Y0plot
  synthetic <- Y0plot %*% synth$solution.w
  
  # Set up plot parameters
  par(mai = c(1.6, 1.2, 0.2, 1.2),
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
  plot(1878:1932, Y1plot[, 1],
       ylim = y_limits,
       xlim = c(1878, 1932),
       type = "n",
       xlab = "",
       ylab = "N\uadgram share (%)",
       xaxs = "i",
       yaxs = "i",
       xaxt = "n",
       yaxt = "n")
  
  # Add custom x-axis
  axis(1, at = seq(1880, 1930, by = 10), cex.axis = 1.2)
  
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
  lines(1878:1932, Y1plot[, 1], lwd = 2)  
  lines(1878:1932, synthetic[, 1], lty = 2, lwd = 2)  
  
  # Add vertical line at 1917
  abline(v = 1917, lty = 2, lwd = 0.8)
  
  # Add legend
  legend("topright", 
         legend = c("Actual", "Synthetic"),
         lty = c(1, 2),
         lwd = 1,
         bty = "n",
         cex = 1.2)
  
  # Add number of authors and seed below x-axis
  if (!is.null(seed)) {
    mtext(paste0(num_authors, " donors (seed = ", seed, ")"),
          side = 1, line = 4, cex = 1.44)
  } else {
    mtext(paste0(num_authors, " donors"),
          side = 1, line = 4, cex = 1.44)
  }
}

# Function to display and save synthetic control details
display_and_save_synth_details <- function(synth_out, dataprep_out, num_authors, seed = NULL) {
  # Generate filename
  filename <- if (!is.null(seed)) {
    paste0(num_authors, "_", seed)
  } else {
    as.character(num_authors)
  }
  
  output_file <- file.path("Gaussian_Marx/txt", paste0(filename, ".txt"))
  
  # Create output string and display in console
  output_text <- ""
  
  # Title
  title <- paste0("\nSynthetic Control Analysis", 
                  if (!is.null(seed)) paste0(" (seed = ", seed, ")"), 
                  "\n\n")
  cat(title)
  output_text <- paste0(output_text, title)
  
  # Write weights
  weights_header <- "Synthetic Control Weights:\n"
  cat(weights_header)
  output_text <- paste0(output_text, weights_header)
  
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
  
  # Print weights
  weights_output <- capture.output(print(significant_weights, row.names = FALSE))
  cat(paste(weights_output, collapse = "\n"), "\n\n")
  output_text <- paste0(output_text, paste(weights_output, collapse = "\n"), "\n\n")
  
  # Write predictor balance
  balance_header <- "Predictor Balance:\n"
  cat(balance_header)
  output_text <- paste0(output_text, balance_header)
  
  synth.tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  balance_output <- capture.output(print(synth.tables$tab.pred))
  cat(paste(balance_output, collapse = "\n"), "\n\n")
  output_text <- paste0(output_text, paste(balance_output, collapse = "\n"), "\n\n")
  
  # Write MSPE
  mspe_text <- paste0("Mean Squared Prediction Error (MSPE): ", synth_out$loss.w, "\n")
  cat(mspe_text)
  output_text <- paste0(output_text, mspe_text)
  
  # Save to file
  writeLines(output_text, output_file)
}

# Modify save_synth_plots to display plot in console
save_synth_plots <- function(synth_out, dataprep_out, num_authors, seed = NULL) {
  filename <- if (!is.null(seed)) {
    paste0(num_authors, "_", seed)
  } else {
    as.character(num_authors)
  }
  
  # Display plot in console
  create_synth_plot(synth_out, dataprep_out, num_authors, seed)
  
  # Save plots
  plot_width <- 9.2
  plot_height <- 5.9
  
  pdf(file = file.path("Gaussian_Marx/pdf", paste0(filename, ".pdf")), 
      width = plot_width, height = plot_height)
  create_synth_plot(synth_out, dataprep_out, num_authors, seed)
  dev.off()
  
  png(file = file.path("Gaussian_Marx/png", paste0(filename, ".png")),
      width = plot_width, height = plot_height, units = "in", res = 300)
  create_synth_plot(synth_out, dataprep_out, num_authors, seed)
  dev.off()
}

# Function to perform synthetic control analysis
perform_synth_analysis <- function(data, donor_pool_size, use_random_walks, volatility, seed_info) {
  treated_author <- "Karl Marx"
  
  if (seed_info$use_seed) {
    set.seed(seed_info$seed_value)
  } else if (use_random_walks) {
    seed_value <- as.integer(abs(rnorm(1)) * 1e6)
    set.seed(seed_value)
  }
  
  authors_to_exclude <- c(treated_author, "Marx")
  
  # Define predictors
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
  
  # Check for required time periods
  required_years <- 1878:1916
  missing_years <- required_years[!required_years %in% treated_data$Year]
  if (length(missing_years) > 0) {
    warning(paste("Missing data for", treated_author, "in years:", 
                  paste(missing_years, collapse = ", ")))
  }
  
  donor_pool <- data %>% filter(!Name %in% authors_to_exclude)
  
  total_eligible_donors <- nrow(donor_pool %>% distinct(Name))
  
  if (total_eligible_donors == 0) {
    stop("No eligible donors found with complete data")
  }
  
  selected_authors <- sample(unique(donor_pool$Name), size = min(donor_pool_size, length(unique(donor_pool$Name))))
  
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
  
  if (use_random_walks) {
    cat("Applying Gaussian random walks\n")
    years <- unique(selected_data$Year)
    for (author in selected_authors) {
      author_data <- selected_data %>% filter(Name == author)
      initial_value <- author_data$cite_English[author_data$Year == min(years)]
      new_values <- perform_gaussian_random_walk(initial_value, years, volatility)
      selected_data$cite_English[selected_data$Name == author] <- new_values
    }
  }
  
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
    time.plot = 1878:1932
  )
  
  synth_out <- synth(data.prep.obj = dataprep_out)
  
  return(list(synth_out = synth_out, dataprep_out = dataprep_out, selected_data = selected_data, 
              treated_author = treated_author,
              seed_value = if(use_random_walks) ifelse(seed_info$use_seed, seed_info$seed_value, seed_value) else NULL,
              total_eligible_donors = total_eligible_donors,
              volatility = if(use_random_walks) volatility else NULL,
              donor_pool_size = donor_pool_size))
}

# Main function with improved error handling
main <- function() {
  tryCatch({
    data <- read.csv("all_authors_with_citations_and_indicators.csv", header=TRUE, stringsAsFactors=FALSE)
    
    required_columns <- c("Name", "Year", "cite_English", "YearofPublication", 
                          "wrote_English", "wrote_German", "wrote_French", "wrote_Greek", 
                          "wrote_Latin", "wrote_Italian", "wrote_Spanish", 
                          "YearofTranslationtoEnglish", "Socialist", "Political")
    
    # Initial data validation and cleaning
    data <- data %>%
    select(all_of(required_columns)) %>%
      mutate(
        Year = as.integer(Year),
        cite_English = as.numeric(cite_English),
        YearofPublication = as.integer(YearofPublication),
        YearofTranslationtoEnglish = as.integer(YearofTranslationtoEnglish)
      ) %>%
      filter(Year >= 1878, Year <= 1932)
    
    max_donors <- 227  # Set the maximum number of donors to 227
    donor_pool_size <- get_donor_pool_size(max_donors)
    
    treated_author <- "Karl Marx"
    
    # Adjust the donor pool based on the treated author
    data_for_donors <- data %>%
      filter(Name != treated_author) %>%
      filter(Name != "Marx")
    
    actual_max_donors <- nrow(data_for_donors %>% distinct(Name))
    if (donor_pool_size > actual_max_donors) {
      cat("\nNote: The requested donor pool size exceeds the available donors after excluding Marx.")
      cat("\nAdjusting the donor pool size to", actual_max_donors, ".\n")
      donor_pool_size <- actual_max_donors
    }
    
    use_random_walks <- use_gaussian_random_walks()
    
    # Only calculate volatility if random walks are used
    volatility <- NULL
    if (use_random_walks) {
      volatility <- calculate_volatility(data)$median_sd_relative_change
    }
    
    seed_info <- if (use_random_walks) use_custom_seed() else list(use_seed = FALSE, seed_value = NULL)
    
    results <- perform_synth_analysis(data, donor_pool_size, use_random_walks, volatility, seed_info)
    
    # Clear the console
    cat("\014")  # This simulates Ctrl+L
    
    # Display and save synthetic control details
    display_and_save_synth_details(results$synth_out, results$dataprep_out, 
                                   donor_pool_size, 
                                   if(use_random_walks) results$seed_value else NULL)
    
    # Save and display plots
    save_synth_plots(results$synth_out, results$dataprep_out, donor_pool_size, 
                     if(use_random_walks) results$seed_value else NULL)
    
    # Print basic information
    cat("\nTreated Author:", results$treated_author, "\n")
    cat("Number of control units:", length(results$synth_out$solution.w), "\n")
    if (!is.null(results$seed_value)) {
      cat("Seed value:", results$seed_value, "\n")
    }
    
    # Print MSPE (if available)
    if (!is.null(results$synth_out$loss.w)) {
      cat("\nMean Squared Prediction Error (MSPE):", results$synth_out$loss.w, "\n")
    }
    
    # Print the volatility setting only if random walks were used
    if (use_random_walks) {
      cat("\nVolatility setting:", results$volatility, "\n")
    }
    
    cat("\nCongratulations! You have made a Synthetic Marx!\n")
    
  }, error = function(e) {
    cat("\nError occurred:", conditionMessage(e), "\n")
    cat("Please check the data and try again.\n")
  })
}

# Run the analysis
main()