generate_income_bands <- function(N, mean_income, sd_income, lower_band, upper_band, band_width) {
  # Generate normally distributed incomes
  incomes <- rnorm(N, mean = mean_income, sd = sd_income)
  
  # Filter incomes to keep only those within the specified bands
  incomes <- incomes[incomes >= lower_band & incomes <= upper_band]
  
  # Create income bands
  bands <- seq(lower_band, upper_band, by = band_width)
  
  # Assign each income to a band
  income_bands <- cut(incomes, breaks = bands, include.lowest = TRUE, right = FALSE)
  
  # Create a data frame with the results
  result <- data.frame(Income = incomes, Band = income_bands)
  
  return(result)
}

# Example usage
N <- 1000
mean_income <- 50000
sd_income <- 10000
lower_band <- 20000
upper_band <- 80000
band_width <- 10000

income_data <- generate_income_bands(N, mean_income, sd_income, lower_band, upper_band, band_width)
head(income_data)




generate_income_bands <- function(N, mean_income, lower_band, upper_band) {
  # Calculate the standard deviation from the 90% confidence interval
  sd_income <- (upper_band - lower_band) / (3.29)
  # https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
  sd_income <- sqrt(N) * (upper_band - lower_band) / (3.29)
  
  # Generate normally distributed incomes
  incomes <- rnorm(N, mean = mean_income, sd = sd_income)
  
    # Create income bands
  bands <- seq(0, 1e6, by = 1e4)
  
  # Assign each income to a band
  income_bands <- cut(incomes, breaks = bands, include.lowest = TRUE, right = FALSE)
  
  # Create a data frame with the results
  result <- data.frame(Income = incomes, Band = income_bands)
  
  return(result)
}

# Example usage
N <- 1000
mean_income <- 50000
lower_band <- 20000
upper_band <- 80000
band_width <- 10000

income_data <- generate_income_bands(N, mean_income, lower_band, upper_band)
head(income_data)

hist(income_data$Income)
