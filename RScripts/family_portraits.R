source("R/class_ethnic.R")
source("R/furness_balancing.R")

library(dplyr)

households_nssec = read_household_nssec()
residents_ethnic = read_NSSEC_ethinic()

# Simplify Ethnicity to White, Black, Other
residents_ethnic$ethnic6[residents_ethnic$ethnic6 %in% c("Asian","Mixed","Other")] = "Other"

residents_ethnic = residents_ethnic %>%
  group_by(LSOA21CD, NSSEC10, ethnic6) %>%
  summarise(residents = sum(residents))

# Fill in Missing Data
# Assume all white as missing area are at least 97% white
residents_missing = unique(households_nssec$LSOA21CD[!households_nssec$LSOA21CD %in% unique(residents_ethnic$LSOA21CD)])

residents_missing = expand.grid(residents_missing, unique(residents_ethnic$NSSEC10), unique(residents_ethnic$ethnic6))
names(residents_missing) = c("LSOA21CD","NSSEC10","ethnic6")
residents_missing$residents = 0
residents_missing$residents[residents_missing$ethnic6 == "White"] = 1

residents_ethnic = rbind(residents_ethnic, residents_missing)
residents_ethnic = residents_ethnic %>%
  group_by(LSOA21CD, NSSEC10) %>%
  mutate(percent = residents/sum(residents)) %>%
  ungroup()

residents_ethnic = residents_ethnic[order(residents_ethnic$LSOA21CD),]
households_nssec = households_nssec[order(households_nssec$LSOA21CD),]

residents_ethnic = group_split(residents_ethnic, LSOA21CD)
households_nssec = group_split(households_nssec, LSOA21CD)

# Add Thenicity to NNSEC Houshodls
hh = households_nssec[[1]]
eth = residents_ethnic[[1]]

combine_nssec_enthinic = function(hh, eth){
  if(hh$LSOA21CD[1] != eth$LSOA21CD[1]){
    stop("LSOAs don't match")
  } else {
    lsoa = hh$LSOA21CD[1]
    hh$LSOA21CD = NULL
    eth$LSOA21CD = NULL
  }

  cats = c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")
  res = list()
  for(i in seq_along(cats)){
    h = hh[hh$NSSEC10 == cats[i],]
    e = eth[eth$NSSEC10 == cats[i],]
    h$NSSEC10 = NULL

    mat = matrix(1, nrow = nrow(e), ncol = ncol(h))
    csum = as.numeric(h)
    rownames(mat) = e$ethnic6
    colnames(mat) = names(h)

    if(sum(csum) == 0){
      res[[i]] = NULL
    } else {
      #names(csum) = names(h)
      rsum = round(e$percent * sum(csum))

      mat2 = furness_balance(mat, rsum, csum, check = TRUE, int_only = TRUE)
      mat2 = as.data.frame(mat2)
      mat2$ethnic = rownames(mat2)
      mat2 = tidyr::pivot_wider(mat2,
                                values_from = c("LoneParent","DNA","OnePersonOver66","OnePersonOther",
                                         "CoupleNoChildren","CoupleChildren","CoupleNoDepChild",
                                         "Other8"),
                                names_from = "ethnic")
      res[[i]] = mat2

    }
  }
  names(res) = cats
  res = dplyr::bind_rows(res, .id = "NSSEC10")

}



for(i in 1:length(lsoas)){
  id = lsoas[i]


  if(nrow(eth) == 0){
    stop()
  }





}
