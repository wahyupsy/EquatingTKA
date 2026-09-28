#' Equating IRT menggunakan paket plink
#'
#' Menghitung konstanta equating 2024-2025 dan 2024-2026 untuk setiap
#' kombinasi jenjang (SD, SMP) dan mapel (Lit, Num) dengan metode
#' Mean-Mean, Mean-Sigma, Haebara, dan Stocking-Lord.
#'
#' @param fd Folder yang berisi file parameter item.
#' @param threshold Batas praktis displacement (logit) untuk purification anchor.
#' @param purification TRUE = buang item anchor yang displace (2*SE & > threshold);
#'   FALSE = pakai semua common item tanpa purifikasi.
#' @return List dengan elemen:
#' \describe{
#'   \item{rangkuman}{data.frame konstanta B (Mean-Mean, Mean-Sigma, Haebara, Stocking-Lord) per pasangan tahun dan kombinasi.}
#'   \item{list.anchor}{list gabungan item anchor per pasangan tahun (sebelum purifikasi).}
#'   \item{detail}{list hasil detail \code{hasil.2425} dan \code{hasil.2426}, berisi parameter anchor, tabel displacement, model plink, serta konstanta A dan B.}
#' }
#' @importFrom plink as.poly.mod as.irt.pars plink link.pars
#' @export
equating.plink <- function(fd, threshold = 0.5, purification = TRUE) {

  list.param.file <- list.files(path = fd, pattern = "item", full.names = TRUE)
  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  rangkuman <- data.frame()
  hasil.2425 <- list()
  hasil.2426 <- list()
  list.anchor <- list()
  sel_cols <- function(df) df[, c("NAME", "ENTRY", "MEASURE", "COUNT", "OBSMATCH")]

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {

      nm <- paste0(jenjang[j], "_", mapel[k])
      cat("\n===== Processing:", nm, "=====\n")

      tryCatch({

        f24K <- grep(jenjang[j], grep(mapel[k], grep("_24", list.param.file, value = TRUE), value = TRUE), value = TRUE)
        f25K <- grep(jenjang[j], grep(mapel[k], grep("_25", list.param.file, value = TRUE), value = TRUE), value = TRUE)
        f26K <- grep(jenjang[j], grep(mapel[k], grep("_26", list.param.file, value = TRUE), value = TRUE), value = TRUE)

        if (length(f24K) == 0 || length(f25K) == 0 || length(f26K) == 0)
          stop("File tidak lengkap untuk ", nm)

        par.24 <- read.csv(f24K[1], skip = 1, stringsAsFactors = FALSE)
        par.list <- list("25" = read.csv(f25K[1], skip = 1, stringsAsFactors = FALSE),
                         "26" = read.csv(f26K[1], skip = 1, stringsAsFactors = FALSE))

        par.24X <- data.frame(item = as.character(par.24$NAME),
                              param = as.numeric(as.character(par.24$MEASURE)),
                              se = as.numeric(as.character(par.24$ERROR)),
                              stringsAsFactors = FALSE)

        for (yr in c("25", "26")) {

          p2 <- par.list[[yr]]
          tahun <- paste0("2024-20", yr)

          list.anchor[[paste0(nm, "24", yr)]] <- merge(
            sel_cols(par.24), sel_cols(p2), by = "NAME", suffixes = c("24", yr)
          )

          d2X <- data.frame(item = as.character(p2$NAME),
                            param = as.numeric(as.character(p2$MEASURE)),
                            se = as.numeric(as.character(p2$ERROR)),
                            stringsAsFactors = FALSE)

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

          out <- NULL
          lp <- NULL
          B <- A <- c(MM = NA_real_, MS = NA_real_, HB = NA_real_, SL = NA_real_)

          if (n_bersih >= 2) {
            I <- n_bersih
            pm <- plink::as.poly.mod(I)

            plink.pars1 <- list(
              study1 = data.frame(a = rep(1, I), b = d1$param, c = rep(0, I)),
              study2 = data.frame(a = rep(1, I), b = d2$param, c = rep(0, I))
            )

            x <- plink::as.irt.pars(
              plink.pars1,
              cbind(study1 = 1:I, study2 = 1:I),
              cat = list(study1 = rep(2, I), study2 = rep(2, I)),
              poly.mod = list(pm, pm)
            )

            out <- plink::plink(x, rescale = "MS", base.grp = 1, D = 1.7)
            lp <- plink::link.pars(out)
            cons <- out$link@constants

            B <- sapply(c("MM", "MS", "HB", "SL"), function(m) as.numeric(cons[[m]]["B"]))
            A <- sapply(c("MM", "MS", "HB", "SL"), function(m) as.numeric(cons[[m]]["A"]))
          }

          res <- list(
            par.24 = d1,
            par.2 = d2,
            common.item = d1$item,
            displace_tbl = displace_tbl,
            model = out,
            link.pars = lp,
            B = B,
            A = A
          )

          if (yr == "25") hasil.2425[[nm]] <- res else hasil.2426[[nm]] <- res

          rangkuman <- rbind(
            rangkuman,
            data.frame(
              Equating = tahun,
              Kombinasi = nm,
              Anchor_Awal = n_awal,
              Anchor_Bersih = n_bersih,
              Mean.Mean = round(B["MM"], 4),
              Mean.Sigma = round(B["MS"], 4),
              Haebara = round(B["HB"], 4),
              Stocking.Lord = round(B["SL"], 4),
              row.names = NULL
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

  cat("\n\n===== RANGKUMAN HASIL EQUATING =====\n\n")
  print(rangkuman, row.names = FALSE)

  list(
    rangkuman = rangkuman,
    list.anchor = list.anchor,
    detail = list(hasil.2425 = hasil.2425, hasil.2426 = hasil.2426)
  )
}