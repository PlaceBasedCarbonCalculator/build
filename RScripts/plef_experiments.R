tar_load(lsoa_emissions_all)
library(dplyr)

emissions_2019 = lsoa_emissions_all[lsoa_emissions_all$year == 2019,]

emissions_2019 = emissions_2019[,grepl("percap",names(emissions_2019))]


decile_summary <- function(df, probs = c(0.1, 0.9), na.rm = TRUE) {
  # Keep only numeric-like columns
  num_df <- df[, sapply(df, is.numeric), drop = FALSE]

  if (ncol(num_df) == 0) {
    stop("No numeric columns found in the data frame.")
  }

  res <- lapply(num_df, function(x) {
    qs <- as.numeric(quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7))
    names(qs) <- c("bottom_decile", "top_decile")
    c(qs, diff = qs[2] - qs[1])
  })

  out <- do.call(rbind, res)
  # Put column names as a proper column
  out <- data.frame(column = rownames(out), out, row.names = NULL, check.names = FALSE)
  out
}

summ = decile_summary(emissions_2019)
summ$diff_factor = summ$top_decile / summ$bottom_decile

summ$column = gsub("_kgco2e_percap","",summ$column)
summ$column = gsub("_"," ",summ$column)

summ$bottom_decile = round(summ$bottom_decile,1)
summ$top_decile = round(summ$top_decile,1)
summ$diff.top_decile = round(summ$diff.top_decile,1)
summ$diff_factor = round(summ$diff_factor,1)

summ = summ[order(summ$diff_factor),]

write.csv(summ,"data/emission_varaions.csv", row.names = FALSE)

plot(emissions_2019$restaurant_kgco2e_percap, emissions_2019$clothing_kgco2e_percap,
     xlab = "Restaurants", ylab = "Clothes")

names(emissions_2019) = gsub("_kgco2e_percap","",names(emissions_2019))
pairs(emissions_2019)


