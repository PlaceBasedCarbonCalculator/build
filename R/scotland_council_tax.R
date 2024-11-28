load_scotland_council_tax = function(path = "../inputdata/council_tax_scotland/"){

  tmp_dir = file.path(tempdir(),"scot_tax")
  dir.create(tmp_dir)
  unzip(file.path(path,"Dwellings by Council Tax Band Detailed (3).zip"), exdir = tmp_dir)

  ct = list()
  fls = list.files(tmp_dir, pattern = "dwellings-by-band-DZ")
  coltype = readr::cols(Geography_Code = readr::col_character(),
                        Geography_Name = readr::col_character(),
                        Geography_Type = readr::col_character(),
                        DateCode = readr::col_integer(),
                        Measurement = readr::col_character(),
                        Units = readr::col_character(),
                        Value = readr::col_integer(),
                        `Council Tax Band` = readr::col_character())

  for(i in seq_along(fls)){
    sub = readr::read_csv(file.path(tmp_dir, fls[i]), col_types = coltype)
    sub = sub[,c("Geography_Code","DateCode","Value","Council Tax Band")]
    ct[[i]] = sub
  }

  ct = dplyr::bind_rows(ct)
  ct$`Council Tax Band` = tolower(ct$`Council Tax Band`)
  ct$`Council Tax Band` = gsub(" ","_",ct$`Council Tax Band`)
  ct = ct[!grepl("bands_",ct$`Council Tax Band`),]

  ct = tidyr::pivot_wider(ct, id_cols = c("Geography_Code","DateCode"),
                               names_from = "Council Tax Band", values_from = "Value")

  names(ct)[names(ct) == "Geography_Code"] = "LSOA11CD"
  names(ct)[names(ct) == "DateCode"] = "year"
  names(ct)[names(ct) == "total_dwellings"] = "all_properties"
  ct

}
