make_similarity_table = function(hh, oac_year = 2011){
  nms_Tenure <- c("Outright", "Mortgage", "Social_rented", "Private_rented")
  nms_hhComp <- c("OnePersonOther", "OnePersonOver66", "CoupleNoChildren", "CoupleChildren",
                  "CoupleNonDepChildren", "FamilyOver66", "LoneParent", "LoneParentNonDepChildren",
                  "OtherChildren", "OtherIncStudentOrOver66", "OtherNoChildren")
  nms_hhSize <- c("p1", "p2", "p3", "p4+")
  nms_CarVan <- c("car0", "car1", "car2", "car3+")
  #nms_NSSEC <- c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")

  similarity_Tenure = matrix(c(
    1, 0.7, 0.5, 0.3,
    0.7, 1, 0.7, 0.5,
    0.5, 0.7, 1, 0.7,
    0.3, 0.5, 0.7, 1
  ), nrow = 4, dimnames = list(nms_Tenure, nms_Tenure))

  similarity_hhComp = matrix(c(
    1  , 0.9, 0.7, 0.5, 0.3, 0.1, 0.7, 0.7, 0.5, 0.3, 0.1,
    0.9, 1  , 0.9, 0.7, 0.5, 0.3, 0.9, 0.9, 0.7, 0.5, 0.3,
    0.7, 0.9, 1  , 0.9, 0.7, 0.5, 0.9, 0.5, 0.9, 0.7, 0.5,
    0.5, 0.7, 0.9, 1  , 0.9, 0.7, 0.7, 0.9, 0.9, 0.9, 0.7,
    0.3, 0.5, 0.7, 0.9, 1  , 0.9, 0.5, 0.7, 0.9, 0.2, 0.9,
    0.1, 0.3, 0.5, 0.7, 0.9, 1  , 0.3, 0.5, 0.7, 0.9, 0.7,
    0.7, 0.9, 0.9, 0.7, 0.5, 0.3, 1  , 0.9, 0.7, 0.5, 0.3,
    0.7, 0.9, 0.5, 0.9, 0.7, 0.5, 0.9, 1  , 0.9, 0.7, 0.5,
    0.5, 0.7, 0.9, 0.9, 0.9, 0.7, 0.7, 0.9, 1  , 0.9, 0.7,
    0.3, 0.5, 0.7, 0.9, 0.2, 0.9, 0.5, 0.7, 0.9, 1  , 0.9,
    0.1, 0.3, 0.5, 0.7, 0.9, 0.7, 0.3, 0.5, 0.7, 0.9, 1
  ), nrow = 11, dimnames = list(nms_hhComp, nms_hhComp))

  similarity_hhSize = matrix(c(
    1, 0.85, 0.65, 0.45,
    0.85, 1, 0.85, 0.65,
    0.65, 0.85, 1, 0.85,
    0.45, 0.65, 0.85, 1
  ), nrow = 4, dimnames = list(nms_hhSize, nms_hhSize))

  similarity_CarVan = matrix(c(
    1, 0.8, 0.6, 0.4,
    0.8, 1, 0.8, 0.6,
    0.6, 0.8, 1, 0.8,
    0.4, 0.6, 0.8, 1
  ), nrow = 4, dimnames = list(nms_CarVan, nms_CarVan))


  # Define the OAC variables
  variables_21 <- c("1a1", "1a2", "1b1", "1b2", "1c1", "1c2", "2a1", "2a2", "2a3", "2b1", "2b2", "2c1", "2c2", "3a1", "3a2", "3a3", "3a4", "3b1", "3b2", "3c1", "3c2", "4a1", "4a2", "4a3", "4b1", "4b2", "4b3", "4b4", "4c1", "4c2", "5a1", "5a2", "5a3", "5b1", "5b2", "6a1", "6a2", "6a3", "6b1", "6b2", "6b3", "6c1", "6c2", "7a1", "7a2", "7b1", "7b2", "8a1", "8a2", "8b1", "8b2", "8b3")

  variables_11 <- c("1a1","1a2","1a3","1a4","1b1","1b2","1b3","1c1","1c2","1c3","2a1","2a2","2a3","2b1","2b2","2c1","2c2","2c3",
                    "2d1","2d2","2d3","3a1","3a2","3b1","3b2","3b3","3c1","3c2","3d1","3d2","3d3","4a1","4a2","4a3","4b1","4b2",
                    "4c1","4c2","4c3","5a1","5a2","5a3","5b1","5b2","5b3","6a1","6a2","6a3","6a4","6b1","6b2","6b3","6b4","7a1",
                    "7a2","7a3","7b1","7b2","7b3","7c1","7c2","7c3","7d1","7d2","7d3","7d4","8a1","8a2","8b1","8b2","8c1","8c2",
                    "8c3","8d1","8d2","8d3")

  variables_01 <- c("1a1","1a2","1a3","1b1","1b2","1c1","1c2","1c3","2a1","2a2","2b1","2b2","3a1","3a2","3b1","3b2","3c1","3c2",
                     "4a1","4a2","4b1","4b2","4b3","4b4","4c1","4c2","4c3","4d1","4d2","5a1","5a2","5b1","5b2","5b3","5b4","5c1",
                     "5c2","5c3","6a1","6a2","6b1","6b2","6b3","6c1","6c2","6d1","6d2","7a1","7a2","7a3","7b1","7b2")


  if(oac_year == 2021){
    variables = variables_21
  } else if(oac_year == 2011){
    variables = variables_11
  } else if(oac_year == 2001){
    variables = variables_01
  } else {
    stop("oac_year can only be 2001, 2011, 2021")
  }



  if(!all(unique(hh$OAC) %in% variables)){
    stop("OAC values in hh are not in known variables ",paste(unique(hh$OAC)[!unique(hh$OAC) %in% variables], collapse = " "))
  }

  # Initialize the similarity matrix
  similarity_OAC <- matrix(0, nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))

  # Calculate the similarity matrix
  for (i in seq_along(variables)) {
    for (j in seq_along(variables)) {
      if (variables[i] == variables[j]) {
        similarity_OAC[i, j] <- 1
      } else if (substr(variables[i], 1, 2) == substr(variables[j], 1, 2)) {
        similarity_OAC[i, j] <- 0.8
      } else if (substr(variables[i], 1, 1) == substr(variables[j], 1, 1)) {
        similarity_OAC[i, j] <- 0.5
      } else {
        similarity_OAC[i, j] <- 0
      }
    }
  }


  # Similarity matrices can be precomputed and stored outside the function if they remain constant
  similarity_matrices <- list(
    Tenure5 = similarity_Tenure,
    hhComp15 = similarity_hhComp,
    hhSize5 = similarity_hhSize,
    CarVan5 = similarity_CarVan,
    OAC = similarity_OAC
  )


  similarity_table = vector(mode = "list", length = 5)
  names(similarity_table) = names(similarity_matrices)

  for(i in 1:length(similarity_table)){
    similarity_table[[i]] = vector(mode = "list", length = nrow(similarity_matrices[[i]]))
    names(similarity_table[[i]]) = colnames(similarity_matrices[[i]])
    for(j in 1:nrow(similarity_matrices[[i]])){
      var = names(similarity_table)[i]
      sim_matrix <- similarity_matrices[[var]]
      input_value <- names(similarity_table[[i]])[j]
      hh_values <- hh[[var]]

      # if(var == "OAC"){
      #   #Special case LSOAs can have multiple OACs,
      #   input_index <- which(rownames(sim_matrix) %in% input_value)
      #   hh_indices <- match(hh_values, colnames(sim_matrix))
      #   scores <- sim_matrix[input_index, hh_indices]
      #   if(inherits(scores,"matrix")){
      #     scores <- apply(scores, 2, max, na.rm = TRUE)
      #   }
      # } else {
        input_index <- which(rownames(sim_matrix) == input_value)
        hh_indices <- match(hh_values, colnames(sim_matrix))
        scores <- sim_matrix[input_index, hh_indices]
      #}

      similarity_table[[i]][[j]] = unname(scores)
    }
  }

  similarity_table
}





match_hh_census3 <- function(Tenure5,hhComp15,hhSize5,CarVan5,OACs, hh, similarity_table) {

  OAC = unlist(strsplit(OACs," "))
  OAC_max_values <- do.call(pmax,  similarity_table$OAC[OAC])

  similarity_scores = similarity_table$Tenure5[[Tenure5]] +
    similarity_table$hhComp15[[hhComp15]] +
    similarity_table$hhSize5[[hhSize5]] +
    similarity_table$CarVan5[[CarVan5]] +
    OAC_max_values

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      Tenure5 = Tenure5,
      hhComp15 = hhComp15,
      hhSize5 = hhSize5,
      CarVan5 = CarVan5,
      OACs = OACs,
      n_match = nrow(hh_sub),
      match_score = max_score / 5,
      household_id = I(list(hh_sub$household_id))
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}
