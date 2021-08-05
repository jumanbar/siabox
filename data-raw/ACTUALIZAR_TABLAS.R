
source("data-raw/traer_tablas_sia.R", encoding = "UTF-8", local = TRUE)
source("data-raw/t_eti_base.R", encoding = "UTF-8", local = TRUE)
source("data-raw/datos_sia.R", encoding = "UTF-8", local = TRUE)
source("data-raw/datos_sia_sed.R", encoding = "UTF-8", local = TRUE)

borrar <- c(grep("^sia_", dir("man"), value = TRUE),
            "datos_sia.Rd",
            "t_eti_base.Rd",
            "usuarios.Rd")

unlink(file.path("man", borrar))

devtools::document()
