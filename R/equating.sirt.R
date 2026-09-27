#' Equating parameter SIRT untuk tiga tahun
#'
#' Melakukan equating parameter 2024-2025 dan 2024-2026
#' berdasarkan kombinasi jenjang dan mata pelajaran.
#'
#' @param fd Folder yang berisi file parameter.
#'
#' @return List dengan dua elemen:
#'   \describe{
#'     \item{hasil[[1]]}{Rangkuman hasil equating.}
#'     \item{hasil[[2]]}{Hasil lengkap equating.}
#'   }
#'
#' @importFrom sirt equating.rasch
#' @export
equating.sirt <- function(fd) {

  list.param.file <- list.files(
    path = fd,
    pattern = "item",
    full.names = TRUE
  )

  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  hasil.2425 <- list()
  hasil.2426 <- list()

  rangkuman <- data.frame(
    Equating = character(),
    Kombinasi = character(),
    Num_Anchor = numeric(),
    Mean.Mean = numeric(),
    Haebara = numeric(),
    Stocking.Lord = numeric(),
    SD = numeric(),
    Var = numeric(),
    linkerror = numeric(),
    stringsAsFactors = FALSE
  )

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {

      nm <- paste0(jenjang[j], "_", mapel[k])

      cat("\n===== Processing:", nm, "=====\n")

      #=================================================
      # FILE
      #=================================================

      f24 <- grep("_24", list.param.file, value = TRUE)
      f25 <- grep("_25", list.param.file, value = TRUE)
      f26 <- grep("_26", list.param.file, value = TRUE)

      f24K <- grep(
        mapel[k],
        grep(jenjang[j], f24, value = TRUE),
        value = TRUE
      )

      f25K <- grep(
        mapel[k],
        grep(jenjang[j], f25, value = TRUE),
        value = TRUE
      )

      f26K <- grep(
        mapel[k],
        grep(jenjang[j], f26, value = TRUE),
        value = TRUE
      )

      #=================================================
      # READ PARAMETER
      #=================================================

      par.24 <- read.csv(
        f24K[1],
        skip = 1
      )

      par.25 <- read.csv(
        f25K[1],
        skip = 1
      )

      par.26 <- read.csv(
        f26K[1],
        skip = 1
      )

      par.24X <- data.frame(
        item = par.24$NAME,
        param = par.24$MEASURE
      )

      par.25X <- data.frame(
        item = par.25$NAME,
        param = par.25$MEASURE
      )

      par.26X <- data.frame(
        item = par.26$NAME,
        param = par.26$MEASURE
      )

      #=================================================
      # COMMON ITEM
      #=================================================

      common_2425 <- intersect(
        par.24X$item,
        par.25X$item
      )

      common_2426 <- intersect(
        par.24X$item,
        par.26X$item
      )

      #=================================================
      # EQUATING 2024-2025
      #=================================================

      mod.2425 <- sirt::equating.rasch(
        x = par.24X,
        y = par.25X
      )

      mm_2425 <- mod.2425$B.est["Mean.Mean"]
      hb_2425 <- mod.2425$B.est["Haebara"]
      sl_2425 <- mod.2425$B.est["Stocking.Lord"]

      desc_2425 <- mod.2425$descriptives

      #=================================================
      # EQUATING 2024-2026
      #=================================================

      mod.2426 <- sirt::equating.rasch(
        x = par.24X,
        y = par.26X
      )

      mm_2426 <- mod.2426$B.est["Mean.Mean"]
      hb_2426 <- mod.2426$B.est["Haebara"]
      sl_2426 <- mod.2426$B.est["Stocking.Lord"]

      desc_2426 <- mod.2426$descriptives

      #=================================================
      # SIMPAN HASIL 2024-2025
      #=================================================

      hasil.2425[[nm]] <- list(
        par.24 = par.24X,
        par.25 = par.25X,
        common.item = common_2425,
        model = mod.2425,
        Mean.Mean = mm_2425,
        Haebara = hb_2425,
        Stocking.Lord = sl_2425
      )

      #=================================================
      # SIMPAN HASIL 2024-2026
      #=================================================

      hasil.2426[[nm]] <- list(
        par.24 = par.24X,
        par.26 = par.26X,
        common.item = common_2426,
        model = mod.2426,
        Mean.Mean = mm_2426,
        Haebara = hb_2426,
        Stocking.Lord = sl_2426
      )

      #=================================================
      # RANGKUMAN 2024-2025
      #=================================================

      rangkuman <- rbind(
        rangkuman,
        data.frame(
          Equating = "2024-2025",
          Kombinasi = nm,
          Num_Anchor = length(common_2425),
          Mean.Mean = round(mm_2425, 4),
          Haebara = round(hb_2425, 4),
          Stocking.Lord = round(sl_2425, 4),
          SD = round(desc_2425$SD, 4),
          Var = round(desc_2425$Var, 4),
          linkerror = round(desc_2425$linkerror, 4)
        )
      )

      #=================================================
      # RANGKUMAN 2024-2026
      #=================================================

      rangkuman <- rbind(
        rangkuman,
        data.frame(
          Equating = "2024-2026",
          Kombinasi = nm,
          Num_Anchor = length(common_2426),
          Mean.Mean = round(mm_2426, 4),
          Haebara = round(hb_2426, 4),
          Stocking.Lord = round(sl_2426, 4),
          SD = round(desc_2426$SD, 4),
          Var = round(desc_2426$Var, 4),
          linkerror = round(desc_2426$linkerror, 4)
        )
      )
    }
  }

  #=================================================
  # OUTPUT
  #=================================================

  print(rangkuman)

  hasil <- list(
    rangkuman,
    list(
      hasil.2425 = hasil.2425,
      hasil.2426 = hasil.2426
    )
  )

  return(hasil)
}
