

library(roxygen2)
library(devtools)
library(dplyr)
library(statmod)
library(equateIRT)

devtools::document()

usethis::create_package("EquatingTKA")
setwd("D:/OneDrive/Github/EquatingTKA")

setwd("D:/OneDrive/Github/EquatingTKA")
usethis::use_description()



devtools::document()
devtools::install()
library(EquatingTKA)

install.packages("remotes")
remotes::install_github("wahyupsy/EquatingTKA")

devtools::document()
devtools::check()

devtools::install(force = TRUE)

fd <- "D:/OneDrive/Bahan Analisis/Tabel Parameter"

readLines("NAMESPACE")

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

res.equating.sirt <- equating.sirt(fd)
res.equating.sirt$rangkuman

res.equating.irt <- equating.irt(fd)
res.equating.irt

res.equating.sns <- equating.sns(fd)
res.equating.sns

res.equating.plink <- equating.plink(fd)
res.equating.plink$rangkuman

res.equating.direc <- equating.direc(fd, purification = TRUE)
res.equating.direc$rangkuman
