m <- do.call(rbind, strsplit(readLines("day6input.txt"), ""))

# part 1
traverse <- function(area) {
  p <- which(area == "^", arr.ind = TRUE)
  obstacles <- which(area[1:(p[1] - 1), p[2]] == "#")
  nearest_obstacle <- if (length(obstacles) > 0) max(obstacles) else 0

  if (nearest_obstacle == 0) {
    area[1:p[1], p[2]] <- "X"
  } else {
    rows_between <- seq(p[1], nearest_obstacle + 1, by = -1)
    area[
      do.call(rbind, lapply(rows_between, \(r) c(r, p[2])))
    ] <- "X"
    area[
      rows_between[length(rows_between)], p[2]
    ] <- "^"
    plot_area(area)
    area <- traverse(t(area)[nrow(area):1, ])
  }
  return(area)
}

(traverse(m) == "X") |> sum()


# Part 2
traverse_loop_positions <- function(area) {
  potential_positions <- which(area != "#" & area != "^", arr.ind = TRUE)

  loop_positions <- list()

  for (i in 1:nrow(potential_positions)) {
    test_area <- area
    pos <- potential_positions[i, ]
    test_area[pos[1], pos[2]] <- "#" # Place an obstruction at this position

    if (simulate_guard(test_area)) {
      loop_positions <- c(loop_positions, list(pos))
    }
  }

  num_loop_positions <- length(loop_positions)
  cat("Number of obstruction positions:", num_loop_positions, "\n")
}

simulate_guard <- function(area) {
  directions <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
  dir_right <- c(2, 3, 4, 1)
  p <- which(area == "^", arr.ind = TRUE)
  dir <- 1

  visited_states <- list()

  while (TRUE) {
    state_key <- paste(paste(p, collapse = ","), dir, sep = ";")
    if (state_key %in% visited_states) {
      return(TRUE)
    } else {
      visited_states <- c(visited_states, state_key)
    }

    delta <- directions[[dir]]
    front_p <- p + delta

    if (front_p[1] < 1 || front_p[1] > nrow(area) ||
      front_p[2] < 1 || front_p[2] > ncol(area)) {
      return(FALSE)
    }

    if (area[front_p[1], front_p[2]] == "#") {
      dir <- dir_right[dir]
    } else {
      p <- front_p
    }
  }
}

traverse_loop_positions(m)
