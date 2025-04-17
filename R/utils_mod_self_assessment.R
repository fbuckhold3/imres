parse_choices <- function(raw) {
  # raw is a string like "1, Option A | 2, Option B"
  items <- strsplit(raw, "\\|")[[1]]
  kv   <- lapply(items, function(x) {
    parts <- trimws(strsplit(x, ",")[[1]])
    setNames(parts[1], parts[-1])
  })
  unlist(kv)
}
