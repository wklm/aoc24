![Header Image](image.png)

# day 1 ⭐⭐

```R
input <- read.table(text = input, header = FALSE)

# part 1
c1 <- input$V1 |> sort()
c2 <- input$V2 |> sort()

abs(c1 - c2) |> sum() |> print()

# part 2
c1 |> unique() |> sapply(\(x) x * sum(c2 == x)) |> sum() |> print()
```

# day 2 ⭐⭐

```R
input <- read.table(text = input, fill = TRUE, header = FALSE) |>
  apply(MARGIN = 1, FUN = \(x) x[!is.na(x)])

# part 1
is_monotonic_sequence <- function(x) {
  all(diff(x) > 0) || all(diff(x) < 0)
}

has_valid_differences <- function(x) {
  all(abs(diff(x)) >= 1 & abs(diff(x)) <= 3)
}

input |>
  lapply(\(x) is_monotonic_sequence(x) && has_valid_differences(x)) |>
  unlist() |>
  sum() |>
  print()

# part 2
get_leave_one_out_subsets <- function(x) {
  lapply(seq_along(x), FUN = \(i) x[-i])
}

has_valid_subsequence <- function(x, predicate) {
  any(sapply(get_leave_one_out_subsets(x), FUN = predicate))
}

input |>
  lapply(FUN = \(x) {
    has_valid_subsequence(x, \(y) {
      is_monotonic_sequence(y) && has_valid_differences(y)
    })
  }) |>
  unlist() |>
  sum() |>
  print()
```

# day 3 ⭐⭐

```R
# part 1

matches <- gregexpr("mul\\(\\d{1,3},\\d{1,3}\\)", input)


multiplications <- regmatches(input, matches)[[1]] |>
  sapply(\(x) {
    regmatches(x, gregexpr("\\d{1,3}", x))[[1]] |>
      as.integer() |>
      prod()
  }
  )

multiplications |>
  sum() |>
  print()

# part 2

do_matches <- c(0, gregexpr("do\\(\\)", input)[[1]])
dont_matches <- gregexpr("don't\\(\\)", input)[[1]]

enabled <- matches[[1]] |>
  as.vector() |>
  sapply(\(m) {
    do <- max(do_matches[do_matches < m])
    dont <- max(dont_matches[dont_matches < m])

    do > dont
  })

multiplications[enabled] |>
  sum() |>
  print()
```

# day 4 ⭐⭐

```R
# part 1

get_adjacent_4tuples <- function(mat) {
  n <- nrow(mat)
  idx_mat <- matrix(1:(n * n), nrow = n, ncol = n, byrow = TRUE)


  # horizontal
  h_tuples <- cbind(
    c(idx_mat[, 1:(n - 3)]),
    c(idx_mat[, 2:(n - 2)]),
    c(idx_mat[, 3:(n - 1)]),
    c(idx_mat[, 4:n])
  )

  # vertical
  idx_mat_t <- t(idx_mat)
  v_tuples <- cbind(
    c(idx_mat_t[, 1:(n - 3)]),
    c(idx_mat_t[, 2:(n - 2)]),
    c(idx_mat_t[, 3:(n - 1)]),
    c(idx_mat_t[, 4:n])
  )

  # diagonal
  d1_tuples <- cbind(
    c(idx_mat[1:(n - 3), 1:(n - 3)]),
    c(idx_mat[2:(n - 2), 2:(n - 2)]),
    c(idx_mat[3:(n - 1), 3:(n - 1)]),
    c(idx_mat[4:n, 4:n])
  )

  # anti-diagonal
  idx_mat_rev <- idx_mat[, n:1]
  d2_tuples <- cbind(
    c(idx_mat_rev[1:(n - 3), 1:(n - 3)]),
    c(idx_mat_rev[2:(n - 2), 2:(n - 2)]),
    c(idx_mat_rev[3:(n - 1), 3:(n - 1)]),
    c(idx_mat_rev[4:n, 4:n])
  )

  return(rbind(h_tuples, v_tuples, d1_tuples, d2_tuples))
}

m <- do.call(rbind, strsplit(readLines(textConnection(input)), ""))

get_adjacent_4tuples(m) |>
  apply(1, \(r) {
    xmas <- c("X", "M", "A", "S")
    identical(m[r], xmas) || identical(m[r], rev(xmas))
  }) |>
  sum() |>
  print()


# part 2

generate_3x3_submatrices <- function(mat) {
  n <- nrow(mat)
  i <- expand.grid(row = 1:(n - 3 + 1), col = 1:(n - 3 + 1))

  return(
    seq_len(nrow(i)) |>
      lapply(\(k) {
        mat[
          i$row[k]:(i$row[k] + 3 - 1),
          i$col[k]:(i$col[k] + 3 - 1),
          drop = FALSE
        ]
      })
  )
}


is_X_MAS <- function(mat) {
  d <- diag(mat)
  ad <- mat[cbind(1:3, 3:1)]
  xmas <- c("M", "A", "S")

  return(
    (identical(d, xmas) || identical(d, rev(xmas))) &&
      (identical(ad, xmas) || identical(ad, rev(xmas)))
  )
}


m |>
  generate_3x3_submatrices() |>
  sapply(is_X_MAS) |>
  sum() |>
  print()
```

# day 5 ⭐⭐

```R
rules <- input[[1]] |>
  strsplit("\n") |>
  unlist() |>
  sapply(\(s) as.numeric(unlist(strsplit(s, "\\|"))))

pages <- input[[2]] |>
  strsplit("\n") |>
  unlist() |>
  sapply(\(s) as.numeric(unlist(strsplit(s, ","))))


# Part 1

corrected <- sapply(pages, \(p) {
  sub_sort <- function(cp) {
    np <- Reduce(
      \(a, b) {
        lo <- match(b[1], a)
        gr <- match(b[2], a)
        if (!is.na(lo) && !is.na(gr) && lo > gr) {
          a[c(lo, gr)] <- a[c(gr, lo)]
        }
        a
      },
      rules[sapply(rules, \(r) all(r %in% p))],
      init = cp
    )
    if (identical(np, cp)) {
      return(np)
    } else {
      return(sub_sort(np))
    }
  }
  sub_sort(p)
})

is_correct <- mapply(\(p_orig, p_corr) all(p_orig == p_corr), pages, corrected)

pages[is_correct] |>
  sapply(\(p) p[ceiling(length(p) / 2)]) |>
  sum() |>
  print()

# Part 2
corrected[!is_correct] |>
  sapply(\(p) p[ceiling(length(p) / 2)]) |>
  sum() |>
  print()
```

# day 6 ⭐⭐

![obstacles](output.gif)


```R
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
```

# day 7 ⭐⭐

```R
input <- strsplit(readLines("day7input.txt"), ": ")

calibration <- function(operators) {
  sapply(input, function(x) {
    operands <- as.double(unlist(strsplit(x[2], " ")))
    result <- as.double(x[1])

    candidates <- Reduce(\(a, b) sapply(operators, \(f) f(a, b)), operands)
    if (any(candidates == result)) {
      return(result)
    } else {
      return(0)
    }
  }) |>
    sum()
}

# part 1
print(calibration(c(`+`, `*`)), digits = 16)

# part 2
`conc` <- function(a, b) {
  as.character(a) |>
    paste0(as.character(b)) |>
    as.double()
}

print(calibration(c(`*`, `+`, `conc`)), digits = 16)
```
