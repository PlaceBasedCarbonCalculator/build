# This creates a flowchart of the analysis pipeline that can be viewed using
# https://mermaid.live/

library(targets)
txt = tar_mermaid(T)
if(TRUE){ # If TRUE a clearer diagram is produced by removing some steps
  txt = txt[!grepl("parameters",txt)]
  txt = txt[!grepl("dl_boundaries",txt)]
}

writeLines(txt, "mermaid.txt")
