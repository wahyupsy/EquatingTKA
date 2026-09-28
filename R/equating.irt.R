#' Equating IRT menggunakan paket equateIRT
#'
#' Menghitung konstanta equating (B) untuk pasangan tahun 2024-2025 dan 2024-2026
#' pada berbagai kombinasi jenjang dan mata pelajaran menggunakan metode
#' Mean-Mean, Haebara, dan Stocking-Lord.
#'
#' @param fd Karakter. Path folder yang berisi file parameter item (.csv).
#' @param threshold Numerik. Batas absolut displacement (logit) untuk purifikasi anchor. Default 0.5.
#' @param purification Logikal. Jika TRUE, item dengan displacement > 2*SE dan > threshold akan dibuang.
#'
#' @return List berisi:
#' \describe{
#'   \item{rangkuman}{Data frame ringkasan konstanta B untuk setiap metode dan kombinasi.}
#'   \item{list.anchor}{List data frame item anchor sebelum purifikasi.}
#'   \item{detail}{List berisi objek model \code{modIRT}, tabel displacement, dan konstanta per pasangan tahun.}
#' }
#'
#' @importFrom equateIRT modIRT direc
#' @export
equating.irt <- function(fd, threshold = 0.5, purification = TRUE) {
  # Memastikan paket tersedia
  if (!requireNamespace("equateIRT", quietly = TRUE)) {
    stop("Paket 'equateIRT' diperlukan. Silakan instal terlebih dahulu.")
  }

  list.param <- list.files(path = fd, pattern = "item", full.names = TRUE)
  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  hasil_equating <- data.frame(
    Equating = character(),
    Kombinasi = character(),
    Anchor_Awal = numeric(),
    Anchor_Bersih = numeric(),
    Mean.Mean = numeric(),
    Haebara = numeric(),
    Stocking.Lord = numeric(),
    stringsAsFactors = FALSE
  )

  hasil.2425 <- list()
  hasil.2426 <- list()
  list.anchor <- list()
  sel_cols <- function(df) df[, c("NAME", "ENTRY", "MEASURE", "COUNT", "OBSMATCH")]

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {

      nm <- paste0(jenjang[j], "_", mapel[k])
      cat("\n===== Processing:", nm, "=====\n")

      tryCatch({
        # Filtering file berdasarkan jenjang, mapel, dan tahun
        f24K <- grep(mapel[k], grep(jenjang[j], grep("_24", list.param, value = TRUE), value = TRUE), value = TRUE)
        f25K <- grep(mapel[k], grep(jenjang[j], grep("_25", list.param, value = TRUE), value = TRUE), value = TRUE)
        f26K <- grep(mapel[k], grep(jenjang[j], grep("_26", list.param, value = TRUE), value = TRUE), value = TRUE)

        if (length(f24K) == 0 || length(f25K) == 0 || length(f26K) == 0)
          stop("File tidak lengkap untuk ", nm)

        par.24 <- read.csv(f24K[1], skip = 1)
        par.list <- list("25" = read.csv(f25K[1], skip = 1),
                         "26" = read.csv(f26K[1], skip = 1))

        par.24X <- data.frame(item = par.24$NAME, param = par.24$MEASURE, se = par.24$ERROR)

        for (yr in c("25", "26")) {
          p2 <- par.list[[yr]]
          tahun <- paste0("2024-20", yr)

          # Simpan anchor awal
          list.anchor[[paste0(nm, "24", yr)]] <- merge(
            sel_cols(par.24), sel_cols(p2), by = "NAME", suffixes = c("24", yr)
          )

          d2X <- data.frame(item = p2$NAME, param = p2$MEASURE, se = p2$ERROR)

          common <- intersect(par.24X$item, d2X$item)
          n_awal <- length(common)

          d1 <- par.24X[par.24X$item %in% common, ]
          d2 <- d2X[d2X$item %in% common, ]
          d1 <- d1[order(d1$item), ]
          d2 <- d2[order(d2$item), ]

          # Proses Purifikasi
          sepooled <- sqrt(d1$se^2 + d2$se^2)
          displace <- d1$param - d2$param
          flag <- abs(displace) > 2 * sepooled & abs(displace) > threshold
          keep <- if (purification) !flag else rep(TRUE, length(flag))

          displace_tbl <- data.frame(
            item = d1$item, param24 = d1$param, param2 = d2$param,
            displace = displace, sepooled = sepooled, flag = flag
          )

          d1 <- d1[keep, ]
          d2 <- d2[keep, ]
          n_bersih <- nrow(d1)

          mod <- NULL
          mm <- hb <- sl <- NA_real_

          if (n_bersih >= 2) {
            # Format untuk equateIRT (Rasch/1PL: a=1)
            mat1 <- cbind(a = 1, d = d1$param)
            mat2 <- cbind(a = 1, d = d2$param)
            rownames(mat1) <- d1$item
            rownames(mat2) <- d2$item

            mod <- equateIRT::modIRT(coef = list(mat1, mat2), var = NULL, display = FALSE)

            sl <- summary(equateIRT::direc(mods = mod, which = c(1, 2), method = "Stocking-Lord"))$coef["B", "Estimate"]
            mm <- summary(equateIRT::direc(mods = mod, which = c(1, 2), method = "mean-mean"))$coef["B", "Estimate"]
            hb <- summary(equateIRT::direc(mods = mod, which = c(1, 2), method = "Haebara"))$coef["B", "Estimate"]
          }

          res <- list(
            par.24 = d1,
            par.2 = d2,
            common.item = d1$item,
            displace_tbl = displace_tbl,
            model = mod,
            Mean.Mean = mm,
            Haebara = hb,
            Stocking.Lord = sl
          )

          if (yr == "25") hasil.2425[[nm]] <- res else hasil.2426[[nm]] <- res

          hasil_equating <- rbind(
            hasil_equating,
            data.frame(
              Equating = tahun,
              Kombinasi = nm,
              Anchor_Awal = n_awal,
              Anchor_Bersih = n_bersih,
              Mean.Mean = round(mm, 4),
              Haebara = round(hb, 4),
              Stocking.Lord = round(sl, 4)
            )
          )
        }
        cat("✓ Berhasil memproses", nm, "\n")
      }, error = function(e) {
        cat("❌ Gagal pada", nm, ":", e$message, "\n")
      })
    }
  }

  rownames(hasil_equating) <- NULL
  cat("\n\n===== RANGKUMAN HASIL EQUATING =====\n\n")
  print(hasil_equating, row.names = FALSE)

  return(list(
    rangkuman = hasil_equating,
    list.anchor = list.anchor,
    detail = list(hasil.2425 = hasil.2425, hasil.2426 = hasil.2426)
  ))
}
