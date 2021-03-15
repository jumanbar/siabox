tdir <- tempdir()

demoname <- "informe-laguna-merin-doc.Rmd"

demofile <- system.file("examples", demoname, package = "manoSIAR")

file.copy(demofile, file.path(tdir, demoname))

file.edit(file.path(tdir, demoname))
