library(dplyr)
library(jsonlite)
library(dplyr)
library(logger)
library(rvest)
library(opendap.catalog)


# Source
surl <- "https://irishmarineinstitute.github.io/awesome-erddap/erddaps.json"

# Find public servers
servers <- read_json(surl, simplifyVector = TRUE) |>
  filter(public) |>
  mutate(url = paste0(url, "griddap/index.html"))

# Base functions ...
.internal_get <- function(ds) {
  xxx <- tryCatch(
    {
      suppressMessages({
        read_dap_file(ds$link, ds$id) |>
          mutate(variable = varname, tiled = "")
      })
    },
    error = function(e) {
      NULL
    }
  )

  log_info("\t > ", ds$id, " (", ds$n, " of ", ds$mx, ")")
  xxx
}

get_griddap_server <- function(server, overwrite = F) {
  outfile <- paste0("data-raw/", server$short_name, ".rds")

  if (any(!file.exists(outfile), overwrite)) {
    log_info("Checking Server: ", server$short_name)
    serv <- tryCatch(
      {
        html_attr(html_nodes(read_html(server$url), "a"), "href")
      },
      error = function(e) {
        NULL
      }
    )


    if (!is.null(serv)) {
      log_info("Scrapping Server: ", server$short_name)
      ds <- data.frame(link = serv) |>
        filter(grepl("html$", link)) |>
        filter(grepl("griddap", link)) |>
        filter(!grepl("test", link)) |>
        mutate(link = gsub(".html", "", link), id = basename(link)) |>
        filter(id != "documentation")

      log_info(nrow(ds), " datasets found...")

      if (nrow(ds) > 0) {
        ds <- mutate(ds, n = 1:n(), mx = max(n))

        collection <- lapply(1:nrow(ds), function(x) {
          .internal_get(ds = ds[x, ])
        })

        saveRDS(bind_rows(collection), outfile)
      }
    } else {
      logger::log_info("No data found on: ", server$short_name)
    }
  }
}



out <- lapply(1:nrow(servers), function(x) {
  get_griddap_server(server = servers[x, ])
})
