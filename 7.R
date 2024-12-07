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
