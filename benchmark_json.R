# benchmark

install.packages("yyjsonr")

download.file("https://bastianolea.rbind.io/index.json",
              "data/index.json")

resultados <- bench::mark(iterations = 100,
            check = FALSE,
            "yyjsonr" = yyjsonr::read_json_file("data/index.json"),
            "jsonlite" = jsonlite::fromJSON("data/index.json")
            )

resultados
