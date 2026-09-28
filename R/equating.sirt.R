equating.sirt <- function(fd, threshold = 0.5, purification = TRUE) {

  list.param.file <- list.files(path = fd, pattern = "item", full.names = TRUE)
  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  hasil.2425 <- list()
  hasil.2426 <- list()
  list.anchor <- list()
  sel_cols <- function(df) df[, c("NAME", "ENTRY", "MEASURE", "COUNT", "OBSMATCH")]

  rangkuman <- data.frame(
    Equating = character(),
    Kombinasi = character(),
    Anchor_Awal = numeric(),
    Anchor_Bersih = numeric(),
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

      tryCatch({

        f24K <- grep(mapel[k], grep(jenjang[j], grep("_24", list.param.file, value = TRUE), value = TRUE), value = TRUE)
        f25K <- grep(mapel[k], grep(jenjang[j], grep("_25", list.param.file, value = TRUE), value = TRUE), value = TRUE)
        f26K <- grep(mapel[k], grep(jenjang[j], grep("_26", list.param.file, value = TRUE), value = TRUE), value = TRUE)

        par.24 <- read.csv(f24K[1], skip = 1)
        par.list <- list("25" = read.csv(f25K[1], skip = 1),
                         "26" = read.csv(f26K[1], skip = 1))

        par.24X <- data.frame(item = par.24$NAME, param = par.24$MEASURE, se = par.24$ERROR)

        for (yr in c("25", "26")) {

          p2 <- par.list[[yr]]
          tahun <- paste0("2024-20", yr)

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
          mm <- hb <- sl <- sdv <- vr <- le <- NA_real_

          if (n_bersih >= 2) {
            mod <- sirt::equating.rasch(x = d1[, c("item", "param")], y = d2[, c("item", "param")])
            mm <- mod$B.est["Mean.Mean"]
            hb <- mod$B.est["Haebara"]
            sl <- mod$B.est["Stocking.Lord"]
            sdv <- mod$descriptives$SD
            vr <- mod$descriptives$Var
            le <- mod$descriptives$linkerror
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

          rangkuman <- rbind(
            rangkuman,
            data.frame(
              Equating = tahun,
              Kombinasi = nm,
              Anchor_Awal = n_awal,
              Anchor_Bersih = n_bersih,
              Mean.Mean = round(mm, 4),
              Haebara = round(hb, 4),
              Stocking.Lord = round(sl, 4),
              SD = round(sdv, 4),
              Var = round(vr, 4),
              linkerror = round(le, 4)
            )
          )
        }

        cat("✓ Berhasil memproses", nm, "\n")

      }, error = function(e) {
        cat("❌ Gagal pada", nm, ":", e$message, "\n")
      })
    }
  }

  rownames(rangkuman) <- NULL
  print(rangkuman, row.names = FALSE)

  hasil <- list(
    rangkuman = rangkuman,
    list.anchor = list.anchor,
    detail = list(
      hasil.2425 = hasil.2425,
      hasil.2426 = hasil.2426
    )
  )

  return(hasil)
}
