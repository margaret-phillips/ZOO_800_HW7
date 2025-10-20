# ==========================================================
# Wisconsin map — square frame, rivers clipped to state, bluntnose sites
# ==========================================================

# Install if needed
need <- c("ggplot2","ggspatial","sf","osmdata","dplyr","maps","tibble")
inst <- need[!need %in% rownames(installed.packages())]
if (length(inst)) install.packages(inst, dependencies = TRUE)

library(ggplot2)
library(ggspatial)
library(sf)
library(osmdata)
library(dplyr)
library(maps)
library(tibble)

# -------------------------
# Tunables
# -------------------------
MIN_LENGTH_KM   <- 20     # keep rivers >= this length after dissolve
SIMPLIFY_TOL_M  <- 300    # simplification tolerance (meters)
OVERPASS_TO     <- 90     # seconds for Overpass API
CRS_METERS      <- 5070   # NAD83 / Conus Albers (meters)
CRS_WGS84       <- 4326   # lon/lat
SQUARE_PAD_M    <- 20000  # padding around state inside square (meters)

# -------------------------
# Output path (Desktop or iCloud Desktop)
# -------------------------
desktop_paths <- c("~/Desktop",
                   "~/Library/Mobile Documents/com~apple~CloudDocs/Desktop")
desktop_paths <- path.expand(desktop_paths)
out_dir <- desktop_paths[file.exists(desktop_paths)][1]
if (is.na(out_dir) || !dir.exists(out_dir)) stop("Couldn't locate Desktop.")
out_png <- file.path(out_dir, "wisconsin_square_rivers_clipped.png")

# -------------------------
# Wisconsin outline (sf)
# -------------------------
wi_map  <- maps::map("state", region = "wisconsin", fill = TRUE, plot = FALSE)
stopifnot(!is.null(wi_map$x), !is.null(wi_map$y))
wi_poly <- st_as_sf(wi_map)
st_crs(wi_poly) <- CRS_WGS84

# -------------------------
# Build a true SQUARE extent in meters (robust) and convert to lon/lat
# -------------------------
wi_m  <- st_transform(wi_poly, CRS_METERS)
bb_m  <- st_bbox(wi_m)
width_m  <- as.numeric(bb_m["xmax"] - bb_m["xmin"])
height_m <- as.numeric(bb_m["ymax"] - bb_m["ymin"])
side_m   <- max(width_m, height_m) + 2 * SQUARE_PAD_M
cx_m     <- as.numeric((bb_m["xmin"] + bb_m["xmax"]) / 2)
cy_m     <- as.numeric((bb_m["ymin"] + bb_m["ymax"]) / 2)
half_m   <- side_m / 2

sq_coords_m <- matrix(
  c(cx_m - half_m, cy_m - half_m,
    cx_m + half_m, cy_m - half_m,
    cx_m + half_m, cy_m + half_m,
    cx_m - half_m, cy_m + half_m,
    cx_m - half_m, cy_m - half_m),
  ncol = 2, byrow = TRUE
)
square_m  <- st_sfc(st_polygon(list(sq_coords_m)), crs = st_crs(wi_m))
square_ll <- st_transform(square_m, CRS_WGS84)
bb_sq_ll  <- st_bbox(square_ll)
plot_xlim <- c(bb_sq_ll["xmin"], bb_sq_ll["xmax"])
plot_ylim <- c(bb_sq_ll["ymin"], bb_sq_ll["ymax"])

# Use same square bbox for OSM request
bbox_vec <- c(xmin = plot_xlim[1], ymin = plot_ylim[1],
              xmax = plot_xlim[2], ymax = plot_ylim[2])

# -------------------------
# OSM rivers (named) -> clean -> dissolve -> merge -> simplify
# -------------------------
q <- opq(bbox = bbox_vec, timeout = OVERPASS_TO) |>
  add_osm_feature(key = "waterway", value = "river") |>
  add_osm_feature(key = "name")

osm <- osmdata_sf(q, quiet = TRUE)
rivers_raw <- osm$osm_lines
if (is.null(rivers_raw) || nrow(rivers_raw) == 0)
  stop("No river lines returned from OSM (named rivers).")

# Exclude intermittent where tagged
if ("intermittent" %in% names(rivers_raw)) {
  rivers_raw <- subset(
    rivers_raw,
    is.na(intermittent) | !(tolower(as.character(intermittent)) %in% c("yes","1","true"))
  )
}

# Project to meters for robust topology ops
r_m <- st_transform(st_make_valid(rivers_raw), CRS_METERS)

# Keep named defensively
if (!("name" %in% names(r_m))) r_m$name <- NA_character_
r_named <- r_m |> filter(!is.na(name), nchar(name) > 0)

# Dissolve by name and line-merge
r_diss <- r_named |>
  group_by(name) |>
  summarise(geometry = st_union(geometry), .groups = "drop")
r_diss <- st_make_valid(r_diss)
r_diss <- st_collection_extract(r_diss, "LINESTRING")
r_diss <- st_cast(r_diss, "MULTILINESTRING", warn = FALSE)
r_diss$geometry <- st_line_merge(r_diss$geometry)
r_diss <- st_collection_extract(r_diss, "LINESTRING")

# Length filter and simplify (meters)
r_diss$len_km <- as.numeric(st_length(r_diss)) / 1000
r_keep  <- r_diss |> filter(len_km >= MIN_LENGTH_KM)
r_simpl <- st_simplify(r_keep, dTolerance = SIMPLIFY_TOL_M, preserveTopology = TRUE)

# -------------------------
# Clip rivers to Wisconsin (in meters, then back to lon/lat)
# -------------------------
wi_union_m    <- st_make_valid(st_union(wi_m))
rivers_clip_m <- st_intersection(r_simpl, wi_union_m)
rivers_simple <- st_transform(rivers_clip_m, CRS_WGS84)

# -------------------------
# 12 bluntnose minnow sites (south → north)
# -------------------------
sites <- tribble(
  ~name,                                              ~lat,    ~lon,
  "Sixmile Creek (Yahara headwaters), Dane Co.",      43.20,  -89.45,
  "Door Creek, Dane Co.",                             43.06,  -89.20,
  "Sylvester Creek (Lower Sugar R), Green Co.",       42.62,  -89.55,
  "Rock Lake, Jefferson Co.",                         43.08,  -88.92,
  "Lake Ripley, Jefferson Co.",                       43.00,  -89.00,
  "Kilbourn Flowage (Lake Wisconsin), Col/Sauk",      43.45,  -89.75,
  "Apple River, Polk/St. Croix Cos.",                 45.31,  -92.36,
  "Pelican Lake, Oneida Co.",                         45.57,  -89.17,
  "Tomahawk & Little Tomahawk Lakes, Oneida Co.",     45.83,  -89.58,
  "Jennie Webber Lake, Oneida Co.",                   45.63,  -89.52,
  "Upper Eau Claire Lake, Bayfield Co.",              46.38,  -91.08,
  "Lake Owen, Bayfield Co.",                          46.33,  -91.17
)
sites_sf <- st_as_sf(sites, coords = c("lon","lat"), crs = CRS_WGS84)

# -------------------------
# Plot (square frame; rivers clipped; sites)
# -------------------------
p <- ggplot() +
  geom_sf(data = wi_poly, fill = "grey97", color = "grey60", linewidth = 0.6) +
  geom_sf(data = rivers_simple, linewidth = 0.6, color = "steelblue") +
  geom_sf(data = sites_sf, shape = 21, size = 2.8, fill = "red", color = "black", stroke = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  coord_sf(xlim = plot_xlim, ylim = plot_ylim, expand = FALSE, crs = st_crs(CRS_WGS84)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_blank(),
    axis.title   = element_blank(),
    axis.ticks   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin  = margin(6, 6, 6, 6)
  )

print(p)
ggsave(out_png, p, width = 9, height = 9, dpi = 300)

message(
  "Saved: ", out_png,