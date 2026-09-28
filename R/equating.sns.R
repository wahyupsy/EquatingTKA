#' Three-Year IRT Equating with Anchor Item Purification (SNSequate)
#' @param fd Folder path containing item parameter files.
#' @param threshold Maximum displacement threshold allowed for anchor items.
#' @param purification Logical. Perform anchor item purification if TRUE.
#' @return A list containing summary table, anchor details, and raw irt.link models.
#' @importFrom SNSequate irt.link
#' @export
equating.sns <- function(fd, threshold = 0.5, purification = TRUE) {

  list.param.file <- list.files(path = fd, pattern = "item", full.names = TRUE)
  jenjang <- c("SD", "SMP"); mapel <- c("Lit", "Num")

  hasil_equating <- data.frame()
  list.anchor <- list(); hasil.2425 <- list(); hasil.2426 <- list()

  sel_cols <- function(df) df[, c("NAME", "ENTRY", "MEASURE", "COUNT", "OBSMATCH")]
  clean_df <- function(df) na.omit(data.frame(item = as.character(df$NAME), param = as.numeric(as.character(df$MEASURE)), se = df$ERROR, stringsAsFactors = FALSE))

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {
      nm <- paste0(jenjang[j], "_", mapel[k])
      cat("\n===== Processing:", nm, "=====\n")

      f24K <- grep(mapel[k], grep(jenjang[j], grep("_24", list.param.file, value = TRUE), value = TRUE), value = TRUE)
      f25K <- grep(mapel[k], grep(jenjang[j], grep("_25", list.param.file, value = TRUE), value = TRUE), value = TRUE)
      f26K <- grep(mapel[k], grep(jenjang[j], grep("_26", list.param.file, value = TRUE), value = TRUE), value = TRUE)

      par24 <- read.csv(f24K[1], skip = 1)
      par.list <- list("25" = read.csv(f25K[1], skip = 1), "26" = read.csv(f26K[1], skip = 1))
      par.24X <- clean_df(par24)

      for (yr in c("25", "26")) {
        p2 <- par.list[[yr]]
        tahun <- paste0("2024-20", yr)
        list.anchor[[paste0(nm, "24", yr)]] <- merge(sel_cols(par24), sel_cols(p2), by = "NAME", suffixes = c("24", yr))

        d2X <- clean_df(p2)
        common <- intersect(par.24X$item, d2X$item)
        n_awal <- length(common)

        d1 <- par.24X[par.24X$item %in% common, ]; d1 <- d1[order(d1$item), ]
        d2 <- d2X[d2X$item %in% common, ];         d2 <- d2[order(d2$item), ]

        # Purifikasi Anchor Item (Drift/Outlier Filter)
        sepooled <- sqrt(d1$se^2 + d2$se^2)
        displace <- d1$param - d2$param
        flag <- abs(displace) > (2 * sepooled) & abs(displace) > threshold
        keep <- if (purification) !flag else rep(TRUE, length(flag))

        d1_clean <- d1[keep, ]; d2_clean <- d2[keep, ]
        n_clean <- nrow(d1_clean)

        res_link <- NULL; mm <- hb <- sl <- NA_real_

        if (n_clean >= 2) {
          parm_df <- data.frame(aJj = 1, bJj = d1_clean$param, cJj = 0, aIj = 1, bIj = d2_clean$param, cIj = 0)
          res_link <- SNSequate::irt.link(parm = parm_df, common = 1:n_clean, model = "1PL", icc = "logistic", D = 1.7)
          mm <- round(res_link$mm[2], 4)
          hb <- round(res_link$Haebara[2], 4)
          sl <- round(res_link$StockLord[2], 4)
        }

        # Simpan Objek Model Utuh
        if (yr == "25") hasil.2425[[nm]] <- res_link else hasil.2426[[nm]] <- res_link

        # Gabung Ringkasan Hasil Equating
        hasil_equating <- rbind(hasil_equating, data.frame(
          Equating = tahun, Kombinasi = nm, Anchor_Awal = n_awal, Anchor_Bersih = n_clean,
          Mean.Mean = mm, Haebara = hb, Stocking.Lord = sl, stringsAsFactors = FALSE
        ))
      }
    }
  }

  print(hasil_equating, row.names = FALSE)

  # Mengembalikan output list 3 elemen
  return(list(
    hasil_equating = hasil_equating,
    list.anchor = list.anchor,
    hasil.link = list(hasil.2425 = hasil.2425, hasil.2426 = hasil.2426)
  ))
}
