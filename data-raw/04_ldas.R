#### LDAS ####
library(rvest)

das <- html_nodes(read_html("https://hydro1.gesdisc.eosdis.nasa.gov/dods/"), "a")
URL <- unique(gsub("\\.[a-z]*$", "", html_attr(das, "href")))
URL <- URL[grepl("NLDAS|GLDAS", URL)]

ldas <- list()

for (i in 1:length(URL)) {
  ldas[[i]] <- read_dap_file(paste0(URL[i], "/"), id = basename(URL[i]))
  message(i, " of ", length(URL))
}

ldas2 <- bind_rows(ldas) |>
  tidyr::separate(id, into = c("id", "model", "scenario"), sep = "_") |>
  group_by(by = nrows) |>
  mutate(
    grid.id = paste0("DAS_", cur_group_id()),
    variable = varname,
    tiled = ""
  ) |>
  ungroup()

saveRDS(ldas2, "data-raw/ldas.rds")
