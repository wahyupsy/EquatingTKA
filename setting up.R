

library(roxygen2)
library(devtools)
library(dplyr)

require(equateIRT)

devtools::document()

usethis::create_package("EquatingTKA")
setwd("D:/OneDrive/Github/EquatingTKA")

setwd("D:/OneDrive/Github/EquatingTKA")
usethis::use_description()



devtools::document()
devtools::install()


install.packages("remotes")
remotes::install_github("wahyupsy/EquatingTKA")

devtools::document()
devtools::check()

devtools::install(force = TRUE)

fd <- "D:/OneDrive/Bahan Analisis/Tabel Parameter"

readLines("NAMESPACE")

packageVersion("EquatingTKA")
find.package("EquatingTKA")
getNamespaceExports("EquatingTKA")


fd <- "D:/OneDrive/Bahan Analisis/Tabel Parameter"
list.param <- list.files(path=fd, pattern="item", full.names=TRUE)
list.param

system("git status")
remotes::install_github("wahyupsy/EquatingTKA", force=TRUE)
library(EquatingTKA)
jenjang <- c("SD", "SMP")
mapel <- c("Lit", "Num")

res.irt <- equating.sirt(fd)
res.sns <- equating.sns(fd)
hasil.plink <- equating.plink(fd)
hasil <- equating.direc(fd)
