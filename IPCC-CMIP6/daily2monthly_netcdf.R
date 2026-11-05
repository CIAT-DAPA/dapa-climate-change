library(terra)

in_dir  <- "F:/cmip6_raw_historical/daily"
out_dir <- "F:/cmip6_raw_historical"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  in_dir,
  pattern = "_day_.*\\.nc$",
  full.names = TRUE
)

for (f in files) {
  
  message("Processing: ", basename(f)
  # Read daily NetCDF
  r <- rast(f)
  # Get dates
  dates <- time(r)
  if (is.null(dates)) {
    stop("No time dimension found in: ", f)
  }
  
  # Monthly grouping
  ym <- format(as.Date(dates), "%Y-%m")
  
  # Monthly average
  r_mon <- tapp(r, index = ym, fun = mean, na.rm = TRUE)
  
  # Assign monthly dates
  time(r_mon) <- as.Date(paste0(unique(ym), "-15"))
  
  # Rename output file: day -> Amon
  out_name <- basename(f)
  out_name <- gsub("_day_", "_Amon_", out_name)
  out_file <- file.path(out_dir, out_name)
  
  # Write NetCDF
  
  writeCDF(
    r_mon,
    filename = out_file,
    overwrite = TRUE
  )
  
}