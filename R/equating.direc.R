#' Equating IRT menggunakan equateIRT::direc
#'
#' @param fd Folder yang berisi file parameter item.
#' @param threshold Batas praktis displacement (logit) untuk purification anchor.
#' @param purification TRUE = buang item anchor yang displace (2*SE & > threshold);
#'   FALSE = pakai semua common item tanpa purifikasi.
#' @return List: hasil[[1]] rangkuman, hasil[[2]] list.anchor (gabungan item per pasangan),
#'   hasil[[3]] hasil detail.
#' @importFrom equateIRT modIRT direc eqc
#' @export
equating.direc <- function(fd, threshold = 0.5, purification = TRUE) {

  require(equateIRT)

  list.param.file <- list.files(path = fd, pattern = "item", full.names = TRUE)
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

  process_pair <- function(par.1, par.2, tahun, nm) {
    common_items <- intersect(par.1$item, par.2$item)
    n_awal <- length(common_items)
    data1 <- par.1[par.1$item %in% common_items, , drop = FALSE]
    data2 <- par.2[par.2$item %in% common_items, , drop = FALSE]
    data1 <- data1[order(data1$item), , drop = FALSE]
    data2 <- data2[order(data2$item), , drop = FALSE]

    sepooled <- sqrt(data1$se^2 + data2$se^2)
    displace <- data1$param - data2$param
    flag <- abs(displace) > 2 * sepooled & abs(displace) > threshold

    keep <- if (purification) !flag else rep(TRUE, length(flag))

    displace_tbl <- data.frame(
      item = data1$item,
      param1 = data1$param,
      param2 = data2$param,
      displace = displace,
      sepooled = sepooled,
      flag = flag
    )

    data1 <- data1[keep, , drop = FALSE]
    data2 <- data2[keep, , drop = FALSE]
    n_bersih <- nrow(data1)

    if (n_bersih < 2) {
      summary_pair <- data.frame(
        Equating = tahun,
        Kombinasi = nm,
        Anchor_Awal = n_awal,
        Anchor_Bersih = n_bersih,
        Mean.Mean = NA_real_,
        Haebara = NA_real_,
        Stocking.Lord = NA_real_,
        stringsAsFactors = FALSE
      )
      return(list(
        summary = summary_pair,
        par.1 = data1,
        par.2 = data2,
        common.item = data1$item,
        displace_tbl = displace_tbl,
        model = NULL,
        Mean.Mean = NULL,
        Haebara = NULL,
        Stocking.Lord = NULL
      ))
    }

    mat1 <- cbind(value.a1 = 1, valued.d = data1$param)
    mat2 <- cbind(value.a1 = 1, valued.d = data2$param)
    rownames(mat1) <- data1$item
    rownames(mat2) <- data2$item

    mod_data <- equateIRT::modIRT(coef = list(mat1, mat2), var = NULL, display = FALSE)

    coef.MM <- equateIRT::direc(mods = mod_data, which = c(1, 2), method = "mean-mean")
    coef.HB <- equateIRT::direc(mods = mod_data, which = c(1, 2), method = "Haebara")
    coef.SL <- equateIRT::direc(mods = mod_data, which = c(1, 2), method = "Stocking-Lord")

    eq.MM <- equateIRT::eqc(coef.MM)
    eq.HB <- equateIRT::eqc(coef.HB)
    eq.SL <- equateIRT::eqc(coef.SL)

    mm <- as.numeric(eq.MM$B[1])
    hb <- as.numeric(eq.HB$B[1])
    sl <- as.numeric(eq.SL$B[1])

    summary_pair <- data.frame(
      Equating = tahun,
      Kombinasi = nm,
      Anchor_Awal = n_awal,
      Anchor_Bersih = n_bersih,
      Mean.Mean = round(mm, 4),
      Haebara = round(hb, 4),
      Stocking.Lord = round(sl, 4),
      stringsAsFactors = FALSE
    )

    list(
      summary = summary_pair,
      par.1 = data1,
      par.2 = data2,
      common.item = data1$item,
      displace_tbl = displace_tbl,
      model = mod_data,
      Mean.Mean = coef.MM,
      Haebara = coef.HB,
      Stocking.Lord = coef.SL
    )
  }

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {
      nm <- paste0(jenjang[j], "_", mapel[k])
      cat("\n===== Processing:", nm, "=====\n")

      tryCatch({
        f24 <- grep("_24", list.param.file, value = TRUE)
        f25 <- grep("_25", list.param.file, value = TRUE)
        f26 <- grep("_26", list.param.file, value = TRUE)

        f24K <- grep(mapel[k], grep(jenjang[j], f24, value = TRUE), value = TRUE)
        f25K <- grep(mapel[k], grep(jenjang[j], f25, value = TRUE), value = TRUE)
        f26K <- grep(mapel[k], grep(jenjang[j], f26, value = TRUE), value = TRUE)

        par24 <- read.csv(f24K[1], skip = 1, stringsAsFactors = FALSE)
        par25 <- read.csv(f25K[1], skip = 1, stringsAsFactors = FALSE)
        par26 <- read.csv(f26K[1], skip = 1, stringsAsFactors = FALSE)

        # daftar gabungan item anchor (versi lengkap kolom, sebelum purifikasi)
        list.anchor[[paste0(nm, "2425")]] <- merge(sel_cols(par24), sel_cols(par25), by = "NAME", suffixes = c("24", "25"))
        list.anchor[[paste0(nm, "2426")]] <- merge(sel_cols(par24), sel_cols(par26), by = "NAME", suffixes = c("24", "26"))

        par.24X <- data.frame(
          item = as.character(par24$NAME),
          param = as.numeric(as.character(par24$MEASURE)),
          se = as.numeric(as.character(par24$ERROR))
        )
        par.25X <- data.frame(
          item = as.character(par25$NAME),
          param = as.numeric(as.character(par25$MEASURE)),
          se = as.numeric(as.character(par25$ERROR))
        )
        par.26X <- data.frame(
          item = as.character(par26$NAME),
          param = as.numeric(as.character(par26$MEASURE)),
          se = as.numeric(as.character(par26$ERROR))
        )

        res2425 <- process_pair(par.24X, par.25X, "2024-2025", nm)
        res2426 <- process_pair(par.24X, par.26X, "2024-2026", nm)

        hasil.2425[[nm]] <- res2425
        hasil.2426[[nm]] <- res2426

        hasil_equating <- rbind(hasil_equating, res2425$summary, res2426$summary)

        cat("✓ Selesai\n")
      }, error = function(e) {
        cat("ERROR pada", nm, ":", e$message, "\n")
      })
    }
  }

  rownames(hasil_equating) <- NULL

  cat("\n\n===== RANGKUMAN HASIL EQUATING =====\n\n")
  print(hasil_equating, row.names = FALSE)

  hasil <- list(
    rangkuman = hasil_equating,
    list.anchor = list.anchor,
    detail = list(
      hasil.2425 = hasil.2425,
      hasil.2426 = hasil.2426
    )
  )

  return(hasil)
}
