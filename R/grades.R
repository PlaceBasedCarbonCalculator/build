value2grade <- function(x, high_good = FALSE, zeroNA = TRUE){

  x_cent <- percentile(x, zeroNA = zeroNA) # In flights.R
  grades <- c(rep("A+",1),
              rep("A",4),
              rep("A-",5),
              rep("B+",6),
              rep("B",6),
              rep("B-",7),
              rep("C+",7),
              rep("C",7),
              rep("C-",7),
              rep("D+",7),
              rep("D",7),
              rep("D-",7),
              rep("E+",7),
              rep("E",6),
              rep("E-",6),
              rep("F+",5),
              rep("F",4),
              rep("F-",1))

  if(high_good){
    grades <- c(grades, "F-")
    x_grade <- grades[match(x_cent,100:0)]
  } else {
    grades <- c("A+",grades)
    x_grade <- grades[match(x_cent,0:100)]
  }

  x_grade[is.na(x_grade)] = "NA"

  return(x_grade)
}
