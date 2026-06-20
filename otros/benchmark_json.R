# benchmark

# install.packages("yyjsonr")

download.file("https://bastianolea.rbind.io/index.json", "cache/index.json")

resultados <- bench::mark(
  iterations = 100,
  check = FALSE,
  "yyjsonr" = yyjsonr::read_json_file("cache/index.json"),
  "jsonlite" = jsonlite::fromJSON("cache/index.json")
)

resultados
