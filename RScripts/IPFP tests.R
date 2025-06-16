library(mipfp)

# Example inconsistent marginals
target_rows <- c(100, 200, 150)
target_cols <- c(120, 180, 150)

# Initial estimate (random values)
initial_matrix <- matrix(runif(9, 30, 70), nrow = 3, ncol = 3)

# Fit with iterative proportional fitting
res <- Ipfp(initial_matrix, list(1,2), list(target_rows, target_cols), tol = 0.01)

# Check consistency
rowSums(res$x.hat)
colSums(res$x.hat)

library(dplyr)
library(tidyr)

# Make Example Population of three variables
pop = expand.grid(list(c("male","female"), c("child","teenager","adult","elderly"),c("poor","middle","rich")), stringsAsFactors = FALSE)
pop$population = sample(1:100, nrow(pop), replace = TRUE)

# Now Extract 3 bivariate tables
var1by2 = group_by(pop, Var1, Var2) |> summarise(population = sum(population))
var2by3 = group_by(pop, Var2, Var3) |> summarise(population = sum(population))
var1by3 = group_by(pop, Var1, Var3) |> summarise(population = sum(population))

# Now to "protect privacy" slightly randomised the numbers
var1by2$population = var1by2$population + sample(-1:1,nrow(var1by2), replace = TRUE)
var2by3$population = var2by3$population + sample(-1:1,nrow(var2by3), replace = TRUE)
var1by3$population = var1by3$population + sample(-1:1,nrow(var1by3), replace = TRUE)

# Convert  to matrix
var1by2 = pivot_wider(var1by2, names_from = "Var2", values_from = "population")
var1by2mat = as.matrix(var1by2[,2:ncol(var1by2)])
rownames(var1by2mat) = var1by2$Var1

var2by3 = pivot_wider(var2by3, names_from = "Var3", values_from = "population")
var2by3mat = as.matrix(var2by3[,2:ncol(var2by3)])
rownames(var2by3mat) = var2by3$Var2

var1by3 = pivot_wider(var1by3, names_from = "Var3", values_from = "population")
var1by3mat = as.matrix(var1by3[,2:ncol(var1by3)])
rownames(var1by3mat) = var1by3$Var1


seed = array(1, dim = c(nrow(var1by2mat),ncol(var1by2mat),ncol(var1by3mat))) * sum(var1by2mat)
#seed = seed * sum(var1by2mat)

res <- mipfp::Ipfp(seed, list(c(1,2),c(2,3),c(1,3)), list(var1by2mat, var2by3mat, var1by3mat))

# Does not accept inconsisent values
# res = humanleague::ipf(seed,
#                   indices = list(c(1,2),c(2,3),c(1,3)),
#                   marginals = list(var1by2mat, var2by3mat, var1by3mat))


res2 = res$x.hat * sum(var1by2mat)

result_df = expand.grid(
  rownames(var1by2mat),
  colnames(var1by2mat),
  colnames(var2by3mat)
)
names(result_df) = c("Var1","Var2","Var3")
result_df$population2 = as.numeric(res2)


# From https://spatial-microsim-book.robinlovelace.net/smsimr#sintegerisation
int_trs <- function(x){
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}


result_df$population2_int = int_trs(result_df$population2)

combined = left_join(pop, result_df, by = c("Var1","Var2","Var3"))
combined$diff = combined$population2_int - combined$population

var1by2_after = group_by(combined, Var1, Var2) |> summarise(population2_int = sum(population2_int))
var1by2_after = pivot_wider(var1by2_after, names_from = "Var2", values_from = "population2_int")

var2by3_after = group_by(combined, Var2, Var3) |> summarise(population2_int = sum(population2_int))
var2by3_after = pivot_wider(var2by3_after, names_from = "Var3", values_from = "population2_int")

var1by3_after = group_by(combined, Var1, Var3) |> summarise(population2_int = sum(population2_int))
var1by3_after = pivot_wider(var1by3_after, names_from = "Var3", values_from = "population2_int")

var1by2
var1by2_after

var2by3
var2by3_after

var1by3
var1by3_after

# Convert  to matrix
var1by2_aftermat = as.matrix(var1by2_after[,2:ncol(var1by2_after)])
rownames(var1by2_aftermat) = var1by2_after$Var1

var2by3_aftermat = as.matrix(var2by3_after[,2:ncol(var2by3_after)])
rownames(var2by3mat) = var2by3_after$Var2

var1by3_aftermat = as.matrix(var1by3_after[,2:ncol(var1by3_after)])
rownames(var1by3_aftermat) = var1by3_after$Var1


# Now Run though humanleauge
res_human <- humanleague::qisi(seed,
                               indices = list(c(1,2), c(2,3),c(1,3)),
                               marginals = list(var1by2_aftermat,var2by3_aftermat, var1by3_aftermat))


result_human_df = expand.grid(
  rownames(var1by2_aftermat),
  colnames(var1by2_aftermat),
  colnames(var2by3_aftermat)
)
names(result_human_df) = c("Var1","Var2","Var3")
result_human_df$population_human = as.numeric(res_human$result)

combined_human = left_join(combined, result_human_df, by = c("Var1","Var2","Var3"))
combined_human$diff_human = combined_human$population - combined_human$population_human






# Now Comapre to humanleauge

var1by3matF = match_matrix_rsums(var1by2mat, var1by3mat)
var2by3matF = furness_balance(var2by3mat, rsum = colSums(var1by2mat), csum = colSums(var1by3matF), int_only = TRUE)


res_human <- humanleague::qisi(seed,
                               indices = list(c(1,2), c(2,3),c(1,3)),
                               marginals = list(var1by2mat,var2by3matF, var1by3matF))


result_human_df = expand.grid(
  rownames(var1by2mat),
  colnames(var1by2mat),
  colnames(var2by3mat)
)
names(result_human_df) = c("Var1","Var2","Var3")
result_human_df$population2 = as.numeric(res_human$result)

combined_human = left_join(pop, result_human_df, by = c("Var1","Var2","Var3"))
combined_human$diff = combined_human$population2 - combined_human$population

# qisi speed check

# bigger example
pop = expand.grid(list(c("a","b","c","d","e","f"), c("A","B","C","D","E","F"),c("a","b","c")), stringsAsFactors = FALSE)
pop$population = sample(1:100, nrow(pop), replace = TRUE)

# Now Extract 3 bivaraite tables
var1by2 = group_by(pop, Var1, Var2) |> summarise(population = sum(population))
var2by3 = group_by(pop, Var2, Var3) |> summarise(population = sum(population))
var1by3 = group_by(pop, Var1, Var3) |> summarise(population = sum(population))

# Now to "protect privacy" slightly randomised the numbes
var1by2$population = var1by2$population + sample(-1:1,nrow(var1by2), replace = TRUE)
var2by3$population = var2by3$population + sample(-1:1,nrow(var2by3), replace = TRUE)
var1by3$population = var1by3$population + sample(-1:1,nrow(var1by3), replace = TRUE)

# COnver to matrix
var1by2 = pivot_wider(var1by2, names_from = "Var2", values_from = "population")
var1by2mat = as.matrix(var1by2[,2:ncol(var1by2)])
rownames(var1by2mat) = var1by2$Var1

var2by3 = pivot_wider(var2by3, names_from = "Var3", values_from = "population")
var2by3mat = as.matrix(var2by3[,2:ncol(var2by3)])
rownames(var2by3mat) = var2by3$Var2

var1by3 = pivot_wider(var1by3, names_from = "Var3", values_from = "population")
var1by3mat = as.matrix(var1by3[,2:ncol(var1by3)])
rownames(var1by3mat) = var1by3$Var1



seed_simple = array(1, dim = c(6,6,3))
seed_pefect = array(pop$population, dim = c(6,6,3))
seed_norm = array(pop$population/sum(pop$population), dim = c(6,6,3))

seed_norm2 = array(pop$population/60, dim = c(6,6,3))
seed_norm2 = ifelse(seed_norm2 > 1, 1,  seed_norm2)
seed_norm2 = ifelse(seed_norm2 < 1 &    seed_norm2 > 0.03, 0.75 , seed_norm2)
seed_norm2 = ifelse(seed_norm2 < 0.03 & seed_norm2 > 0.01, 0.5  , seed_norm2)
seed_norm2 = ifelse(seed_norm2 < 0.01 & seed_norm2 > 0   , 0.001, seed_norm2)

seed_norm3 = array(round(pop$population/sum(pop$population),3)*sum(pop$population), dim = c(6,6,3))

# Match sums
var1by2mat = match_matrix_rsums(var1by3mat,var1by2mat)



bench::mark(r_simple = humanleague::qisi(seed_simple,
                                         indices = list(c(1,2), c(1,3),c(2,3)),
                                         marginals = list(var1by2mat,var1by3mat,var2by3mat)),
            r_perfect = humanleague::qisi(seed_pefect,
                                          indices = list(c(1,2), c(1,3),c(2,3)),
                                          marginals = list(var1by2mat,var1by3mat,var2by3mat)),
            r_norm = humanleague::qisi(seed_norm,
                                          indices = list(c(1,2), c(1,3),c(2,3)),
                                          marginals = list(var1by2mat,var1by3mat,var2by3mat)),
            r_norm2 = humanleague::qisi(seed_norm2,
                                       indices = list(c(1,2), c(1,3),c(2,3)),
                                       marginals = list(var1by2mat,var1by3mat,var2by3mat)),
            r_norm3 = humanleague::qisi(seed_norm3,
                                        indices = list(c(1,2), c(1,3),c(2,3)),
                                        marginals = list(var1by2mat,var1by3mat,var2by3mat)),
            check = FALSE
)


result_df = expand.grid(
  rownames(var1by2mat),
  colnames(var1by2mat),
  colnames(var2by3mat)
)
names(result_df) = c("Var1","Var2","Var3")
result_df$p_simple = as.numeric(r_simple$result)
result_df$p_perfect = as.numeric(r_perfect$result)
result_df$p_norm = as.numeric(r_norm$result)
result_df$p_norm2 = as.numeric(r_norm2$result)
result_df$p_norm3 = as.numeric(r_norm2$result)
combined = left_join(pop, result_df, by = c("Var1","Var2","Var3"))

combined$error_simple = abs(combined$p_simple - combined$population)
combined$error_perfect = abs(combined$p_perfect - combined$population)
combined$error_norm = abs(combined$p_norm - combined$population)
combined$error_norm2 = abs(combined$p_norm2 - combined$population)
combined$error_norm3 = abs(combined$p_norm3 - combined$population)

summary(combined$error_simple)
summary(combined$error_perfect)
summary(combined$error_norm)
summary(combined$error_norm2)
summary(combined$error_norm3)


match_matrix_rsums <- function(mat1, mat2) {
  # Calculate the row sums of both matrices
  row_sums1 <- rowSums(mat1)
  row_sums2 <- rowSums(mat2)

  if(all(row_sums1 == row_sums2)){
    return(mat2)
  }

  # Calculate the difference in row sums
  row_diff <- row_sums1 - row_sums2


  # Adjust mat2 to match the row sums of mat1
  for (i in 1:nrow(mat2)) {
    adjustment <- row_diff[i]
    while (adjustment != 0) {
      for (j in 1:ncol(mat2)) {
        if (adjustment == 0) break
        if (adjustment > 0) {
          mat2[i, j] <- mat2[i, j] + 1
          adjustment <- adjustment - 1
        } else if(mat2[i, j] > 0){
          mat2[i, j] <- mat2[i, j] - 1
          adjustment <- adjustment + 1
        }
      }
    }
  }
  return(mat2)
}

match_matrix_csums <- function(mat1, mat2) {
  # Calculate the col sums of both matrices
  col_sums1 <- colSums(mat1)
  col_sums2 <- colSums(mat2)

  if(all(col_sums1 == col_sums2)){
    return(mat2)
  }

  # Calculate the difference in col sums
  col_diff <- col_sums1 - col_sums2


  # Adjust mat2 to match the col sums of mat1
  for (i in 1:ncol(mat2)) {
    adjustment <- col_diff[i]
    while (adjustment != 0) {
      for (j in 1:nrow(mat2)) {
        if (adjustment == 0) break
        if (adjustment > 0) {
          mat2[j, i] <- mat2[j, i] + 1
          adjustment <- adjustment - 1
        } else if(mat2[j, i] > 0){
          mat2[j, i] <- mat2[j, i] - 1
          adjustment <- adjustment + 1
        }
      }
    }
  }

  return(mat2)
}



