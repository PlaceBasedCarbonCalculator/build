txt = tar_mermaid(T)

txt2 = txt[!grepl("parameters",txt)]
txt2 = txt2[!grepl("dl_boundaries",txt2)]
writeLines(txt2, "mermaid.txt")
