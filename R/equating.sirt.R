#' Equating IRT menggunakan paket sirt
#'
#' Menghitung konstanta equating (B) untuk pasangan tahun 2024-2025 dan 2024-2026
#' menggunakan metode Rasch Equating dari paket \code{sirt}. Fungsi ini juga menyediakan
#' statistik deskriptif seperti Standar Deviasi, Varians, dan Link Error.
#'
#' @param fd Karakter. Path folder yang berisi file parameter item (.csv).
#' @param threshold Numerik. Batas absolut displacement (logit) untuk purifikasi anchor. Default 0.5.
#' @param purification Logikal. Jika TRUE, item dengan displacement > 2*SE dan > threshold akan dibuang.
#'
#' @return List berisi:
#' \describe{
#'   \item{rangkuman}{Data frame ringkasan konstanta B (Mean-Mean, Haebara, Stocking-Lord) serta indikator kualitas penyetaraan (SD, Var, linkerror).}
#'   \item{list.anchor}{List data frame item anchor sebelum purifikasi untuk setiap kombinasi.}
#'   \item{detail}{List berisi detail hasil per pasangan tahun (24-25 dan 24-26), termasuk tabel displacement dan objek model \code{sirt}.}
#' }
#'
#' @importFrom sirt equating.rasch
#' @export
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

        if (length(f24K) == 0 || length(f25K) == 0 || length(f26K) == 0)
          stop("File tidak lengkap untuk ", nm)

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
            # Menggunakan sirt::equating.rasch
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
  cat("\n\n===== RANGKUMAN HASIL EQUATING (SIRT) =====\n\n")
  print(rangkuman, row.names = FALSE)

  return(list(
    rangkuman = rangkuman,
    list.anchor = list.anchor,
    detail = list(hasil.2425 = hasil.2425, hasil.2426 = hasil.2426)
  ))
}


#-------------- Ko

# Ambil data SMPNum 24 dan 26
par24_smpnum <- read.csv(par_files.24[4])  # SMPNum_24
par26_smpnum <- read.csv(par_files.26[4])  # SMPNum_26

# ENTRY anchor items dari tabel Anda
anchor_entries <- c(11, 169, 110, 5, 154, 67, 202, 52, 119, 40, 
                    148, 97, 48, 150, 207, 215, 91, 2, 112, 46, 
                    123, 134, 189, 149)

# Filter hanya anchor items
d24_anchor <- par24_smpnum[par24_smpnum$ENTRY %in% anchor_entries, ]
d26_anchor <- par26_smpnum[par26_smpnum$ENTRY %in% anchor_entries, ]

# Hitung percent correct
d24_anchor$pct_24 <- (d24_anchor$SCORE / d24_anchor$COUNT) * 100
d26_anchor$pct_26 <- (d26_anchor$SCORE / d26_anchor$COUNT) * 100

# Bandingkan
komparasi_pct <- data.frame(
  ENTRY = d24_anchor$ENTRY,
  NAME = d24_anchor$NAME,
  MEASURE_24 = round(d24_anchor$MEASURE, 2),
  MEASURE_26 = d26_anchor$MEASURE[match(d24_anchor$ENTRY, d26_anchor$ENTRY)],
  PCT_24 = round(d24_anchor$pct_24, 2),
  PCT_26 = round(d26_anchor$pct_26[match(d24_anchor$ENTRY, d26_anchor$ENTRY)], 2)
)

komparasi_pct







# ------------------------------- Membandingkan Percent Correct -------------------------------




# Buat percent correct per base
pct_correct <- data.frame()

bases <- c("SDLit", "SDNum", "SMPLit", "SMPNum")

par_files.24 <- list.files(fd_par, pattern = "*24", full.names = TRUE)
par_files.25 <- list.files(fd_par, pattern = "*25", full.names = TRUE)
par_files.26 <- list.files(fd_par, pattern = "*26.*unequated", full.names = TRUE)

pct_correct <- data.frame()

for (i in seq_along(par_files.24)) {
  base <- bases[i]
  
  par24 <- read.csv(par_files.24[i])
  par25 <- read.csv(grep(base, par_files.25, value = TRUE)[1])
  par26 <- read.csv(grep(base, par_files.26, value = TRUE)[1])
  
  # Hitung percent correct
  par24$pct_correct <- (par24$SCORE / par24$COUNT) * 100
  par25$pct_correct <- (par25$SCORE / par25$COUNT) * 100
  par26$pct_correct <- (par26$SCORE / par26$COUNT) * 100
  
  # Match items untuk setiap pasangan
  for (year in c(25, 26)) {
    if (year == 25) {
      par_target <- par25
    } else {
      par_target <- par26
    }
    
    idx24 <- match(par_target$NAME, par24$NAME)
    matched_idx <- !is.na(idx24)
    
    d1 <- par24[idx24[matched_idx], ]
    d2 <- par_target[matched_idx, ]
    
    # Tambah data 24
    pct_correct <- rbind(
      pct_correct,
      data.frame(
        base = base,
        pair = paste0("24-", year),
        form = "24",
        n = nrow(d1),
        mean_pct = round(mean(d1$pct_correct, na.rm = TRUE), 2),
        sd_pct = round(sd(d1$pct_correct, na.rm = TRUE), 2)
      )
    )
    
    # Tambah data target
    pct_correct <- rbind(
      pct_correct,
      data.frame(
        base = base,
        pair = paste0("24-", year),
        form = year,
        n = nrow(d2),
        mean_pct = round(mean(d2$pct_correct, na.rm = TRUE), 2),
        sd_pct = round(sd(d2$pct_correct, na.rm = TRUE), 2)
      )
    )
  }
}

pct_correct
