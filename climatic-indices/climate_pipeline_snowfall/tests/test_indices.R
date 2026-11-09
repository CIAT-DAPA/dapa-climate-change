# Pruebas de lógica, sin datos externos ni paquetes de tablas.
for (f in sort(list.files('R', pattern = '\\.R$', full.names = TRUE))) source(f)
source('config.R')
c <- cfg
x <- rbind(c(0, 0, 2, 0, 0, 0, 5), c(1, 2, 3, 4, 5, 6, 7), rep(NA_real_, 7))
stopifnot(identical(index_cdd(x, c), c(3, 0, NA_real_)),
          identical(as.numeric(index_drd(x, c)), c(5, 0, NA_real_)))
p <- index_p95(x, c(3, 5, NA), c)
stopifnot(p$p95_wet_days_pct[1] == 50, abs(p$p95_wet_days_pct[2] - 200/7) < 1e-10,
          is.na(p$p95_wet_days_pct[3]))
stopifnot(index_frd(matrix(c(-1, 0, 1), 1), c) == 1,
          length(month_dates(2000, 2)) == 29L,
          length(month_dates(1900, 2)) == 28L,
          calendar_day(as.Date('1981-03-01')) == calendar_day(as.Date('1984-03-01')))
stopifnot(all(is.na(base_precipitation(matrix(NA_real_, 1, 28), c)$prec_total_mm)))
# Episodio de siete días entre diciembre y enero; evento atribuido a diciembre.
d <- seq(as.Date('2000-12-28'), as.Date('2001-01-08'), by = 'day')
hot <- matrix(c(rep(TRUE, 7), rep(FALSE, 5)), nrow = 1)
z <- index_hwd(hot, d, matrix(TRUE, 1, length(d)), c)
stopifnot(all(z$days == c(4, 3)), all(z$events == c(1, 0)), all(z$longest == c(7, 7)))
# Cinco días no califican; NA corta racha y anula mes por cobertura estricta.
h <- matrix(c(rep(TRUE, 5), FALSE, rep(TRUE, 6)), 1)
z <- index_hwd(h, d, matrix(TRUE, 1, length(d)), c)
stopifnot(sum(z$days) == 6, sum(z$events) == 1)
h[1, 9] <- NA
z <- index_hwd(h, d, !is.na(h), c)
stopifnot(is.na(z$days[1, 2]), z$days[1, 1] == 0)
# Fechas discontinuas no forman una ola.
d2 <- c(seq(as.Date('2001-01-01'), by='day', length.out=3),
        seq(as.Date('2001-01-10'), by='day', length.out=3))
z <- index_hwd(matrix(TRUE, 1, 6), d2, matrix(TRUE, 1, 6), c)
stopifnot(sum(z$days) == 0)
q <- matrix(c(1, 2, 3, 4, 0, 0, 0, 0), nrow = 2, byrow = TRUE)
stopifnot(all(class_values(c(2, 0), q) == c(2, 1)))
st <- data.frame(clay_class=1, slope_class=2, wetness_class=3, landcover_class=4)
stopifnot(abs(index_fld(5, st) - 3.5) < 1e-10)
stopifnot(identical(dim(row_q(matrix(c(NA, NA, 1, 2), 2, byrow=TRUE), .95)), c(2L, 1L)),
          !is.na(row_q(matrix(c(NA, NA, 1, 2), 2, byrow=TRUE), .95)[2, 1]))
cat('OK: índices, cobertura, calendario, empates y rachas entre meses/años.\n')
# Deciles temporales distintos por celda: nunca se agrupan celdas.
years <- 1991:2020
local_cfg <- cfg; local_cfg$reference_years <- years
mat <- rbind(0:29, 100:129, rep(0, 30), rep(NA_real_, 30))
ref <- decile_thresholds(mat, years, local_cfg)
stopifnot(identical(colnames(ref$q), paste0('p', seq(10,90,10))),
          all(abs(ref$q[2, ] - ref$q[1, ] - 100) < 1e-10),
          all(class_values(c(10,10,0,1), ref$q)[1:3] == c(4,1,1)),
          is.na(class_values(c(10,10,0,1), ref$q)[4]))
# Nueve límites => diez niveles; valor igual al límite queda en el inferior.
stopifnot(all(class_values(c(-1, 9), rbind(1:9, 1:9)) == c(1,9)),
          class_values(10, matrix(1:9,1)) == 10)
cls <- annual_classes(mat, ref$q)
freq <- class_frequencies(cls)
stopifnot(all(abs(rowSums(freq[1:3,paste0('class_',1:10,'_pct')])-100) < 1e-8),
          freq$class_1_pct[3] == 100, freq$n_classified[4] == 0,
          all(is.na(freq[4,paste0('class_',1:10,'_pct')])) )
empty <- class_frequencies(matrix(NA_integer_,1,0))
stopifnot(empty$n_classified == 0, all(is.na(empty[paste0('class_',1:10,'_pct')])))
cat('OK: diez niveles por celda, límites exactos, ceros, NA y frecuencias.\n')
