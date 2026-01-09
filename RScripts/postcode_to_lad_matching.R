library(targets)
library(sf)
library(tmap)


tar_load(bounds_postcode_area)
tar_load(bounds_la)


la_pts <- st_point_on_surface(bounds_la)         # for LAD25NM labels


adj_list <- st_touches(bounds_postcode_area)

four_color_dsatur <- function(adj, max_colors = 8) {
  n <- length(adj)
  colors <- rep(NA_integer_, n)
  deg <- lengths(adj)

  # Helper: saturation (count of distinct neighbor colors)
  sat_count <- function(i) {
    used <- colors[adj[[i]]]
    length(unique(used[!is.na(used)]))
  }

  while (anyNA(colors)) {
    candidates <- which(is.na(colors))
    # Compute saturation for candidates, tie-break by degree
    sat_vals <- sapply(candidates, sat_count)
    deg_vals <- deg[candidates]
    i <- candidates[order(sat_vals, deg_vals, decreasing = TRUE)][1]

    used <- colors[adj[[i]]]
    used <- used[!is.na(used)]
    # Choose the smallest available color
    avail <- setdiff(seq_len(max_colors), unique(used))
    if (length(avail) == 0) {
      stop("Needed more than 4 colors. Consider using rook adjacency or check for topology/sliver polygons.")
    }
    colors[i] <- avail[1]
  }
  colors
}

color_ids <- four_color_dsatur(adj_list, max_colors = 8)

# Define a good 4-color palette (distinct and colorblind-friendly)
palette4 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#F2F249")  # Red, Blue, Green, Purple
bounds_postcode_area$fill_col <- palette4[color_ids]






# --- 5) Build the tmap ---
tmap_mode("plot")  # or "view" if you prefer interactive

m2 <- tm_shape(bounds_postcode_area) +
  tm_fill(fill = "fill_col", fill.legend = tm_legend_hide()) +
  #tm_borders(col = "grey30", lwd = 0.6) +
  tm_text("PC_AREA", size = 0.7, col = "grey30") +

  tm_shape(bounds_la) +
  tm_borders(col = "grey70", lwd = 1.2) +

  tm_shape(la_pts) +
  tm_text("LAD25NM", size = 0.2) +

  tm_layout(frame = FALSE,
            main.title.size = 1.0)
tmap_save(m2,"plots/postcode_area_la_map.png", dpi = 600, width = 10, height = 20)
