# >=6 días con Tmax > climatología diaria + 5°C. Rachas entre meses/años.
# Acumulación incremental: no conserva décadas de temperaturas en RAM.
heat_state <- function(n, dates) {
  s <- new.env(parent = emptyenv())
  s$dates <- dates
  s$keys <- unique(as.integer(format(dates, '%Y%m')))
  s$month <- match(as.integer(format(dates, '%Y%m')), s$keys)
  s$days <- s$events <- s$longest <- matrix(0L, n, length(s$keys))
  s$valid <- matrix(0L, n, length(s$keys))
  s$run <- integer(n); s$start <- integer(n)
  s
}
heat_close <- function(s, ids, end) {
  ids <- ids[s$run[ids] >= 1L]
  if (!length(ids)) return(invisible(NULL))
  # Longitud completa del episodio en cada mes que toca.
  first_month <- s$month[s$start[ids]]
  last_month <- s$month[end]
  for (a in unique(first_month)) {
    ii <- ids[first_month == a]
    for (m in seq.int(a, last_month))
      s$longest[ii, m] <- pmax(s$longest[ii, m], s$run[ii])
  }
}
heat_update <- function(s, hot, valid, j, c) {
  hot[is.na(hot)] <- FALSE
  if (j > 1L && s$dates[j] - s$dates[j - 1L] != 1) {
    heat_close(s, which(s$run >= c$heat_min_days), j - 1L)
    s$run[] <- 0L
  }
  ended <- which(!hot & s$run >= c$heat_min_days)
  if (j > 1L) heat_close(s, ended, j - 1L)
  starts <- hot & s$run == 0L
  s$start[starts] <- j
  s$run <- ifelse(hot, s$run + 1L, 0L)
  m <- s$month[j]
  s$valid[, m] <- s$valid[, m] + as.integer(valid)
  reached <- which(s$run == c$heat_min_days)
  if (length(reached)) {
    # Cuando se confirma el episodio, recuperar sus primeros seis días.
    for (offset in 0:(c$heat_min_days - 1L)) {
      mm <- s$month[j - offset]
      s$days[reached, mm] <- s$days[reached, mm] + 1L
    }
    starts_m <- s$month[s$start[reached]]
    for (mm in unique(starts_m)) {
      ii <- reached[starts_m == mm]
      s$events[ii, mm] <- s$events[ii, mm] + 1L
    }
  }
  longer <- which(s$run > c$heat_min_days)
  s$days[longer, m] <- s$days[longer, m] + 1L
}
heat_finish <- function(s, c) {
  heat_close(s, which(s$run >= c$heat_min_days), length(s$dates))
  for (m in seq_along(s$keys)) {
    bad <- s$valid[, m] < ceiling(sum(s$month == m) * c$min_valid_fraction)
    s$days[bad, m] <- s$events[bad, m] <- s$longest[bad, m] <- NA_integer_
  }
  invisible(s)
}
index_hwd <- function(hot, dates, valid, c) {
  s <- heat_state(nrow(hot), dates)
  for (j in seq_along(dates)) heat_update(s, hot[, j], valid[, j], j, c)
  heat_finish(s, c)
  list(keys = s$keys, days = s$days, events = s$events, longest = s$longest)
}
