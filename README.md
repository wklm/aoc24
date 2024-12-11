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

![obstacles]( animation_day6.gif)

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

# day 8 ⭐⭐
serious cleanup needed 😅
```R
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
```

# day 9 ⭐⭐

```java
package day9;

import java.io.File;
import java.util.List;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

record DiskMap() {

    static int lastId = 0;
    static HashMap<Integer, Integer> blockSizes = new HashMap<>();
    static HashMap<Integer, Integer> blockIndices = new HashMap<>();

    List<String> parse(String input) {
        List<String> result = new ArrayList<>();
        int id = 0;
        boolean odd_i = true;
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            int times = Character.getNumericValue(c);
            if (odd_i) {
                for (int j = 0; j < times; j++) {
                    result.add(String.valueOf(id));
                }
                blockSizes.put(id, times);
                blockIndices.put(id, result.size() - times);
                id++;
            } else {
                for (int j = 0; j < times; j++) {
                    result.add(".");
                }
            }
            odd_i = !odd_i;
        }
        lastId = id - 1;
        return result;
    }

    private List<String> swap(List<String> blocks, int i, int j) {
        String temp = blocks.get(i);
        blocks.set(i, blocks.get(j));
        blocks.set(j, temp);
        return blocks;
    }

    List<String> makeCompact(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        int left = 0;
        int right = blocks.size() - 1;
        while (left < right) {
            while (left < right && !blocks.get(left).equals(".")) {
                left++;
            }
            while (left < right && blocks.get(right).equals(".")) {
                right--;
            }
            if (left < right) {
                blocks = swap(blocks, left, right);
                left++;
                right--;
            }
        }
        return blocks;
    }

    Long checksum(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        Long checksum = 0L;

        for (int i = 0; i < blocks.size(); i++) {
            String block = blocks.get(i);
            if (!block.equals(".")) {
                checksum += (long) i * Integer.parseInt(block);
            }
        }

        return checksum;

    }

    List<String> defragment(List<String> b) {
        List<String> blocks = new ArrayList<>(b);
        for (int id = lastId; id >= 0; id--) {
            int blockSize = blockSizes.get(id);
            int blockIndex = blockIndices.get(id);

            for (int j = 0; j < blockIndex; j++) {
                if (blocks.get(j).equals(".")) {
                    int freeSpace = 0;
                    for (int k = j; k < blocks.size(); k++) {
                        if (blocks.get(k).equals(".")) {
                            freeSpace++;
                        } else {
                            break;
                        }
                    }
                    if (freeSpace >= blockSize) {
                        for (int k = 0; k < blockSize; k++) {
                            blocks.set(j + k, String.valueOf(id));
                        }
                        for (int k = blockIndex; k < blockIndex + blockSize; k++) {
                            blocks.set(k, ".");
                        }
                        break;
                    }
                }
            }
        }
        return blocks;
    }
}

record Puzzle(String input) {
    static String getInput() {
        File file = new File("input.txt");
        try {
            return new String(java.nio.file.Files.readAllBytes(file.toPath()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }

    public static void main(String[] args) {
        DiskMap diskMap = new DiskMap();
        var parsed = Collections.unmodifiableList(
                diskMap.parse(Puzzle.getInput()));

        // Part 1
        System.out.println(diskMap.checksum(diskMap.makeCompact(parsed)));

        // Part 2
        System.out.println(diskMap.checksum(diskMap.defragment(parsed)));
    }
}
```

# day 10 ⭐⭐

```java
package day10;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;


class HikingMap {
    static Grid grid;
    static record Point(int x, int y, int value) {
    }

    static class NaryTree {
        final protected Point point;
        final List<NaryTree> children;

        NaryTree(Point point, List<NaryTree> children) {
            this.point = point;
            this.children = Collections.unmodifiableList(new LinkedList<>(children));
        }

        List<NaryTree> getLeaves() {
            if (children.isEmpty()) {
                return List.of(this);
            } else {
                var leaves = new LinkedList<NaryTree>();
                for (var child : children) {
                    leaves.addAll(child.getLeaves());
                }
                return leaves;
            }
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof NaryTree)) {
                return false;
            }
            NaryTree other = (NaryTree) obj;
            return point.equals(other.point);
        }

        @Override
        public int hashCode() {
            return Objects.hash(point);
        }
    }

    static class Grid {
        private final Point[][] rows;
        private final Map<Integer, List<Point>> locations = new HashMap<>();

        Grid(String input) {
            String[] lines = input.split("\n");
            rows = new Point[lines.length][];

            for (int i = 0; i < lines.length; i++) {
                String line = lines[i];
                char[] numbers = line.toCharArray();
                Point[] row = new Point[numbers.length];
                for (int j = 0; j < numbers.length; j++) {
                    int val = Character.getNumericValue(numbers[j]);
                    Point p = new Point(i, j, val);
                    row[j] = p;
                    locations.computeIfAbsent(val, k -> new LinkedList<>()).add(p);
                }
                rows[i] = row;
            }
        }

        Point get(int x, int y) {
            if (x < 0 || x >= rows.length || y < 0 || y >= rows[0].length) {
                throw new IndexOutOfBoundsException();
            }
            return rows[x][y];
        }

        int[] dim() {
            return new int[] { rows.length, rows[0].length };
        }

        List<Point> find(int value) {
            return locations.getOrDefault(value, Collections.emptyList());
        }

        List<Point> getNeighbors(Point point) {
            List<Point> neighbors = new LinkedList<>();
            int x = point.x();
            int y = point.y();
            int[] dimensions = dim();
            int nRows = dimensions[0];
            int nCols = dimensions[1];

            if (x - 1 >= 0)
                neighbors.add(get(x - 1, y));
            if (x + 1 < nRows)
                neighbors.add(get(x + 1, y));
            if (y - 1 >= 0)
                neighbors.add(get(x, y - 1));
            if (y + 1 < nCols)
                neighbors.add(get(x, y + 1));

            return neighbors;
        }
    }

    public HikingMap(String input) {
        grid = new Grid(input);
    }

    List<NaryTree> getTrailheads() {
        var zeros = grid.find(0);
        return zeros
                .stream()
                .map(this::getPaths)
                .collect(
                        Collectors.collectingAndThen(
                                Collectors.toList(),
                                Collections::unmodifiableList));
    }

    NaryTree getPaths(Point point) {
        var nextSteps = grid
                .getNeighbors(point)
                .stream()
                .filter(
                        n -> grid.get(n.x, n.y).value() == point.value() + 1)
                .collect(Collectors.toList());

        List<NaryTree> paths = nextSteps.stream()
                .map(this::getPaths)
                .collect(Collectors.toList());

        return new NaryTree(grid.get(point.x, point.y), paths);
    }

    int getScore(boolean distinct) {
        return getTrailheads().stream()
                .mapToInt(trailhead -> {
                    var peaks = trailhead
                            .getLeaves()
                            .stream()
                            .filter(l -> l.point.value() == 9);
                    return distinct
                            ? (int) peaks.distinct().count()
                            : (int) peaks.count();
                })
                .sum();
    }
}

record Puzzle(String input) {
    static String getInput() throws IOException {
        Path path = Paths.get("input.txt");
        return Files.readString(path);
    }

    public static void main(String[] args) {
        try {
            var map = new HikingMap(Puzzle.getInput());
            // part 1
            System.out.println(map.getScore(true));
            // part 2
            System.out.println(map.getScore(false));
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}
```

# day 11 ⭐⭐

```java
package wklm.aoc;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

class PlutonianPebbles {
    static class Stone {
        private static int digitCount(long i) {
            int count = 0;
            do {
                count++;
                i /= 10;
            } while (i > 0);
            return count;
        }

        static long[] transform(long stone) {
            if (stone == 0) {
                return new long[]{1};
            }
            int digitCount = digitCount(stone);
            if (digitCount % 2 == 0) {
                long divisor = (long) Math.pow(10, digitCount / 2);
                return new long[]{stone / divisor, stone % divisor};
            }
            return new long[]{stone * 2024L};
        }
    }

    static Map<Long, Long> getStones(String input) {
        return Arrays.stream(input.split(" "))
                .filter(s -> !s.isBlank())
                .map(Long::parseLong)
                .collect(Collectors.groupingBy(
                        Function.identity(),
                        Collectors.counting()));
    }

    static Map<Long, Long> blink(Map<Long, Long> stones) {
        Map<Long, Long> newStones = new HashMap<>();
        for (var entry : stones.entrySet()) {
            for (long s : Stone.transform(entry.getKey())) {
                newStones.merge(s, entry.getValue(), Long::sum);
            }
        }
        return newStones;
    }
}

record Puzzle(String input) {
    static String getInput() throws IOException {
        Path path = Paths.get("input.txt");
        return Files.readString(path);
    }

    public static void main(String[] args) {
        try {
            var stones = PlutonianPebbles.getStones(getInput());

            // part 1
            int blink_times = 25;
            for (int i = 0; i < blink_times; i++) {
                stones = PlutonianPebbles.blink(stones);
            }
            long totalStones = stones
                    .values()
                    .stream()
                    .mapToLong(Long::longValue)
                    .sum();
            System.out.println(totalStones);
            
            // part 2
            blink_times = 50;
            for (int i = 0; i < blink_times; i++) {
                stones = PlutonianPebbles.blink(stones);
            }
            totalStones = stones
                    .values()
                    .stream()
                    .mapToLong(Long::longValue)
                    .sum();
            System.out.println(totalStones);

        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}
```