#' Edit file control Winsteps yang sudah ada
#'
#' Membaca file control Winsteps (.CON/.cf), menghapus baris perintah lama
#' untuk setiap opsi yang bernilai TRUE atau diisi, lalu menyisipkan baris baru
#' sebelum \code{&END}. Anchor butir ditulis inline dalam blok \code{IAFILE=*}.
#'
#' @param con_file Path file control yang diedit.
#' @param namates Prefix nama file output Winsteps.
#' @param out_con Path file control hasil. Default menimpa \code{con_file}.
#' @param UIMEAN Nilai UIMEAN. NULL = tidak diubah.
#' @param USCALE Nilai USCALE. NULL = tidak diubah.
#' @param IREFER Vektor kode IREFER. NULL = tidak diubah.
#' @param extra Character vector baris perintah tambahan.
#' @param SIFILE Logical. TRUE = tulis \code{SIFILE=<namates>_imputation.csv}.
#' @param ISFILE Logical. TRUE = tulis \code{ISFILE=<namates>_threshold.csv}.
#' @param SCFILE Logical. TRUE = tulis \code{SCOREFILE=<namates>_norma.csv}.
#' @param DISFILE Logical. TRUE = tulis \code{DISFILE=<namates>_category.csv}.
#' @param PFILE Logical. TRUE = tulis \code{PFILE=<namates>_person.csv}.
#' @param IFILE Logical. TRUE = tulis \code{IFILE=<namates>_item.csv}.
#' @param ISGROUP Nilai ISGROUPS. NULL = tidak diubah.
#' @param IAFILE Logical. TRUE = tulis anchor butir inline dari \code{paramX}.
#' @param paramX data.frame minimal 2 kolom (nomor butir, measure) untuk IAFILE.
#'
#' @return Invisible character vector isi file control baru.
#'
#' @examples
#' \dontrun{
#' parX <- data.frame(ENTRY = par$ENTRY, MEASURE = par$MEASURE + k)
#' edit_con(con_file = "26_matp_an.cf", namates = "26_matp_an_eq",
#'          out_con = "26_matp_an_eq.cf",
#'          PFILE = TRUE, IFILE = TRUE, SCFILE = TRUE,
#'          IAFILE = TRUE, paramX = parX)
#' }
#' @export
modif.winsteps <- function(con_file, namates, out_con = con_file,
                           UIMEAN = NULL, USCALE = NULL, IREFER = NULL, extra = NULL,
                           SIFILE = FALSE, ISFILE = FALSE, SCFILE = FALSE,
                           DISFILE = FALSE, PFILE = FALSE, IFILE = FALSE,
                           ISGROUP = NULL, IAFILE = FALSE, paramX = NULL) {

  cmd <- readLines(con_file, warn = FALSE)

  hapus <- function(cmd, key) {
    i <- grep(paste0("^\\s*(", key, ")\\s*="), cmd, ignore.case = TRUE)
    if (length(i) == 0) return(cmd)
    if (grepl("=\\s*\\*\\s*$", cmd[i[1]])) {
      j <- i[1] + which(trimws(cmd[(i[1] + 1):length(cmd)]) == "*")[1]
      return(cmd[-(i[1]:j)])
    }
    cmd[-i]
  }

  baru <- c()

  # Helper untuk menyisipkan suffix (seperti "norma", "person", "item") sebelum kata kunci
  sisip_suffix <- function(nama, suffix) {
    # Menyisipkan " <suffix> " sebelum kata fixedpar, unequated, atau equated
    if (grepl("([ _])(fixedpar|unequated|equated)", nama, ignore.case = TRUE)) {
      sub("([ _])(fixedpar|unequated|equated)", paste0(" ", suffix, "\\1\\2"), nama, ignore.case = TRUE)
    } else {
      paste0(nama, "_", suffix)
    }
  }

  # Daftar opsi output
  opsi <- list(
    list(on = isTRUE(SIFILE),  key = "SIFILE",           line = paste0("SIFILE=", sisip_suffix(namates, "imputation"), ".csv")),
    list(on = isTRUE(ISFILE),  key = "ISFILE",           line = paste0("ISFILE=", sisip_suffix(namates, "threshold"), ".csv")),
    list(on = isTRUE(SCFILE),  key = "SCFILE|SCOREFILE", line = paste0("SCOREFILE=", sisip_suffix(namates, "norma"), ".csv")),
    list(on = isTRUE(DISFILE), key = "DISFILE",          line = paste0("DISFILE=", sisip_suffix(namates, "category"), ".csv")),
    list(on = isTRUE(PFILE),   key = "PFILE",            line = paste0("PFILE=", sisip_suffix(namates, "person"), ".csv")),
    list(on = isTRUE(IFILE),   key = "IFILE",            line = paste0("IFILE=", sisip_suffix(namates, "item"), ".csv")),
    list(on = !is.null(UIMEAN),  key = "UIMEAN",    line = paste0("UIMEAN=", UIMEAN)),
    list(on = !is.null(USCALE),  key = "USCALE",    line = paste0("USCALE=", USCALE)),
    list(on = !is.null(IREFER),  key = "IREFER",    line = paste0("IREFER=", paste(IREFER, collapse = ""))),
    list(on = !is.null(ISGROUP), key = "ISGROUPS?", line = paste0("ISGROUPS=", ISGROUP))
  )

  for (o in opsi) {
    if (o$on) {
      cmd  <- hapus(cmd, o$key)
      baru <- c(baru, o$line)
    }
  }

  if (isTRUE(IAFILE)) {
    if (is.null(paramX) || ncol(paramX) < 2) stop("'paramX' minimal 2 kolom (entry, measure).")
    cmd  <- hapus(cmd, "IAFILE")
    baru <- c(baru, "IAFILE=*", paste(paramX[, 1], round(paramX[, 2], 4)), "*")
  }

  if (!is.null(extra)) baru <- c(baru, extra)

  end <- grep("^\\s*&END", cmd, ignore.case = TRUE)[1]
  cmd <- append(cmd, baru, after = end - 1)

  writeLines(cmd, out_con)
  invisible(cmd)
}