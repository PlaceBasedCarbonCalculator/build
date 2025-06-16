consistent_matrices = function(marginals, indicies){

  #Check inbalances
  totals = lapply(marginals, sum)
  rsums = lapply(marginals, rowSums)
  csums = lapply(marginals, colSums)

  # Check if index in each matrix
  indexlist = list()
  for(i in seq(1, max(unlist(indices)))){

    icheck = list()
    for(j in seq_along(indices)){
      if(i %in% indices[[j]]){
        if(indices[[j]][1] == i){
          icheck[[j]] = rsums[[j]]
        } else {
          icheck[[j]] = csums[[j]]
        }
      } else {
        icheck[[j]] = NA
      }
    }
    indexlist[[i]] = icheck
  }

  # For Each Index Work out what is wrong
  indexdiff = list()
  for(i in seq_along(indexlist)){
    x = indexlist[[i]]
    indexdiff_x = list()
    for(j in seq_along(x)){
      y = x[[j]]
      if(length(y) == 1){
        indexdiff_x[[j]] = NA
        next
      }
      indexdiff_x_y = list()
      for(k in seq_along(x)){
        z = y - x[[k]]
        if(all(is.na(z))){
          z = NA
        }
        indexdiff_x_y[[k]] = z
      }
      indexdiff_x[[j]] = indexdiff_x_y
    }
    indexdiff[[i]] = indexdiff_x
  }



}

# bigger example
pop = expand.grid(list(c("male","b"), c("C","D"),c("e","f")), stringsAsFactors = FALSE)
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



indices = list(c(1,2), c(1,3),c(2,3))
marginals = list(var1by2mat,var1by3mat,var2by3mat)
