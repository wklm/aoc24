library(lpSolve)


input <- readLines("day8input.txt")
m <- do.call(rbind, strsplit(input, ""))


# part 1
find_mirrored_antinodes <- function(pair) {
  Filter(
    \(p) {
      p[1] >= 1 && p[1] <= nrow(m) &&
        p[2] >= 1 && p[2] <= nrow(m)
    },
    list(
      pair[[1]] * 2 - pair[[2]],
      pair[[2]] * 2 - pair[[1]]
    )
  )
}

apply_to_pairs_of_coordinates <- function(m, f) {
  as.vector(m) |>
    unique() |>
    Filter(\(x) !(x %in% c(".", "#")), x = _) |>
    lapply(\(x) {
      combn(asplit(which(m == x, arr.ind = TRUE), 1), 2, simplify = FALSE) |>
        lapply(f)
    })
}


apply_to_pairs_of_coordinates(m, find_mirrored_antinodes) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE) |>
  unique() |>
  length() |>
  print()




# part 2
find_inline_antinodes <- function(pair) {
  points_matrix <- do.call(rbind, pair)
  grid_size <- nrow(m)
  contained_in_grid <- function(coord) {
    return(coord >= 1 & coord <= grid_size)
  }
  is_int <- function(x, tol = 1e-6) {
    abs(x - round(x)) < tol
  }

  x1 <- points_matrix[1, 1]
  y1 <- points_matrix[1, 2]
  x2 <- points_matrix[2, 1]
  y2 <- points_matrix[2, 2]

  # vertical
  if (x1 == x2) {
    if (!contained_in_grid(x1)) {
      return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
    }
    y_start <- ceiling(min(y1, y2))
    y_end <- floor(max(y1, y2))
    y_start <- max(y_start, 1)
    y_end <- min(y_end, grid_size)
    if (y_start > y_end) {
      return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
    }
    return(cbind(x = rep(round(x1), y_end - y_start + 1), y = y_start:y_end))
  }

  # horizontal
  if (y1 == y2) {
    if (!contained_in_grid(y1)) {
      return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
    }
    x_start <- ceiling(min(x1, x2))
    x_end <- floor(max(x1, x2))
    x_start <- max(x_start, 1)
    x_end <- min(x_end, grid_size)
    if (x_start > x_end) {
      return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("x", "y"))))
    }
    return(cbind(x = x_start:x_end, y = rep(round(y1), x_end - x_start + 1)))
  }

  m <- (y2 - y1) / (x2 - x1)
  b <- y1 - m * x1

  solve_y_lp <- function(x_val) {
    y_exact <- m * x_val + b
    if (!is_int(y_exact)) {
      return(c(x = NA, y = NA))
    }
    y_int <- round(y_exact)
    if (!contained_in_grid(y_int)) {
      return(c(x = NA, y = NA))
    }
    return(c(x = x_val, y = y_int))
  }

  feasible_points <- vapply(1:grid_size, \(x) solve_y_lp(x),
    FUN.VALUE = numeric(2), USE.NAMES = FALSE
  )

  feasible_points <- feasible_points[
    ,
    apply(feasible_points, 2, \(pt) !any(is.na(pt)))
  ]

  return(t(feasible_points))
}


apply_to_pairs_of_coordinates(m, find_inline_antinodes) |>
  unlist(recursive = FALSE) |>
  do.call(rbind, args = _) |>
  unique() |>
  nrow() |>
  print()
