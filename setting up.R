

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


res.irtoys <- equating.irtoys(fd, purification = TRUE)

d  <- res.irtoys$detail$hasil.2426$SMP_Num
dt <- d$displace_tbl
B  <- d$B["MM"]

rng <- range(c(dt$param24, dt$param2, dt$param2 + B))

plot(rng, rng, type = "n", xlab = "b 2024 (skala dasar)", ylab = "b 2026",
     main = "SMP Num: Achieving common scale 2024-2026")
abline(0, 1, lty = 2, col = "grey50")

text(dt$param24, dt$param2, dt$item, col = 2, cex = 0.8)
text(dt$param24, dt$param2 + B, dt$item, col = ifelse(dt$flag, "grey60", 3), cex = 0.8)

legend("topleft", bty = "n", cex = 0.8,
       legend = c("2026 asli", paste0("2026 + B (MM = ", round(B, 4), ")"), "anchor dibuang (flag)"),
       text.col = c(2, 3, "grey60"))




