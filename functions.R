# ---- Functions ----

# draw cleanup propensity from distribution
draw_cleanup_propensity <- function(n) {
  sample(cleanup_probs, n, replace = TRUE)
}

# fouling probability surface for a given entry point
# returns matrix of weights (not normalised)

fouling_weight_surface <- function(entry_x, entry_y, on_lead = TRUE) {
  # distance from entry and path already precomputed
  # entrance-based weight (exponential decay)
  w_ent <- exp(-entrance_lambda * dist_to_entrance)
  # path-based weight: Weibull for distance from path
  if (on_lead) {
    w_path <- pweibull(dist_to_path, shape = weibull_shape, scale = weibull_scale)
  } else {
    # flatten: increase scale to broaden distribution
    w_path <- pweibull(dist_to_path, shape = weibull_shape, scale = weibull_scale * flatten_factor_offlead)
  }
  # multiplicative combination
  w <- w_ent * w_path
  if(all(w == 0)) w <- matrix(1, GRID_SIZE, GRID_SIZE)
  return(w)
}

# sample fouling location matrix indices given weight surface
sample_location_from_weights <- function(weight_mat) {
  w <- as.numeric(weight_mat)
  w[w < 0] <- 0
  if (all(w == 0)) w <- rep(1, length(w))
  p <- w / sum(w)
  idx <- sample(seq_along(p), size = 1, prob = p)
  # convert index to x,y
  x <- ((idx - 1) %% GRID_SIZE) + 1
  y <- floor((idx - 1) / GRID_SIZE) + 1
  list(x = x, y = y)
}

# proximity to a bin (min Euclidean distance)
# dist_to_nearest_bin <- function(x,y) {
# min(sqrt((bins$x - x)^2 + (bins$y - y)^2))
# }

# check if within bin radius
# is_within_bin_radius <- function(x,y, radius = bin_radius) {
# dist_to_nearest_bin(x,y) <= radius
# }




