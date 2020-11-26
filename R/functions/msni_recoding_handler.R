round2 <- function(x, n = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10 ^ n
  z <- z * posneg
  return(z)
}

individual_to_HH_numeric <-
  function(loop, response, varname, indicator) {
    r <- loop[, c("X_uuid", varname)]
    r <- r[complete.cases(r),]
    r = aggregate(r[, c(2)],
                  by = list(r$X_uuid),
                  FUN = sum,
                  na.rm = T)
    names(r) <- c("X_uuid", indicator)
    response <- merge(response, r, by = "X_uuid", all = T)
    return(response)
  }
