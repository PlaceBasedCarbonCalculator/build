library(osmextract)
library(sf)
path = "D:/CondorSuttonBank/north-yorkshire-260405.osm.pbf"


# Polygon
# natural = wood
# landuse = forest
poly_gpkg = oe_vectortranslate(path, layer = "multipolygons",
                               extra_tags = c("natural","leaf_type","landuse"),)

# Point
# natural = tree
point_gpkg = oe_vectortranslate(path, layer = "points",
                               extra_tags = c("natural"),)

polys = read_sf(poly_gpkg)
polys = polys[,c("osm_id","natural","leaf_type","landuse")]
polys = polys[!is.na(polys$natural) | !is.na(polys$landuse),]
polys$natural2 = ifelse(is.na(polys$natural),polys$landuse,polys$natural)
polys = polys[polys$natural2 %in% c("wood","forest","forestry"),]
polys$leaf_type2 = ifelse(is.na(polys$leaf_type),"broadleaved",polys$leaf_type)
polys$leaf_type2 = ifelse(polys$leaf_type2 != "needleleaved","broadleaved",polys$leaf_type2)

st_write(polys,"D:/CondorSuttonBank/OSM_woods_poly.gpkg")

# points = read_sf(point_gpkg)
# points = points[,c("osm_id","natural","leaf_type")]
# points = points[!is.na(points$natural),]
# points = points[points$natural %in% c("wood","tree_row","forestry"),]
# points$leaf_type2 = ifelse(is.na(polys$leaf_type),"broadleaved",polys$leaf_type)
# points$leaf_type2 = ifelse(polys$leaf_type2 != "needleleaved","broadleaved",polys$leaf_type2)
#
# st_write(polys,"D:/CondorSuttonBank/OSM_woods_poly.gpkg")
