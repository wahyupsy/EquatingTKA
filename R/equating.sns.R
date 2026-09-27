#' Three-Year IRT Equating with Anchor Item Purification
#'
#' Performs 1PL IRT equating between 2024-2025 and 2024-2026
#' using Mean-Mean, Haebara, and Stocking-Lord methods.
#'
#' @param fd Folder containing item parameter files.
#' @param threshold Maximum absolute difference in item difficulty
#'   allowed during anchor purification.
#'
#' @return A data frame containing the equating results for
#'   2024-2025 and 2024-2026.
#'
#' @importFrom SNSequate irt.link
#' @export
equating.sns <- function(
    fd,
    threshold = 0.5
) {

  list.param.file <- list.files(
    path = fd,
    pattern = "item",
    full.names = TRUE
  )

  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  hasil <- list()
  no <- 1

  clean_param_df <- function(df) {

    df$MEASURE <- as.numeric(
      as.character(df$MEASURE)
    )

    na.omit(
      data.frame(
        item = as.character(df$NAME),
        param = df$MEASURE,
        stringsAsFactors = FALSE
      )
    )
  }

  process_pair <- function(
      df1,
      df2,
      nama_pasang,
      label_kombinasi
  ) {

    common_items <- intersect(
      df1$item,
      df2$item
    )

    n_awal <- length(common_items)

    if (n_awal < 2) {

      return(
        data.frame(
          Equating = nama_pasang,
          Kombinasi = label_kombinasi,
          Anchor_Awal = n_awal,
          Anchor_Bersih = n_awal,
          Mean.Mean = NA,
          Haebara = NA,
          Stocking.Lord = NA,
          stringsAsFactors = FALSE
        )
      )
    }

    d1 <- df1[
      df1$item %in% common_items,
    ]

    d2 <- df2[
      df2$item %in% common_items,
    ]

    d1 <- d1[
      order(d1$item),
    ]

    d2 <- d2[
      order(d2$item),
    ]

    diff_b <- abs(
      d1$param - d2$param
    )

    keep <- diff_b <= threshold

    d1_clean <- d1[keep, ]
    d2_clean <- d2[keep, ]

    n_clean <- nrow(d1_clean)

    if (n_clean < 2) {

      return(
        data.frame(
          Equating = nama_pasang,
          Kombinasi = label_kombinasi,
          Anchor_Awal = n_awal,
          Anchor_Bersih = n_clean,
          Mean.Mean = NA,
          Haebara = NA,
          Stocking.Lord = NA,
          stringsAsFactors = FALSE
        )
      )
    }

    parm_df <- data.frame(
      aJj = rep(1, n_clean),
      bJj = d1_clean$param,
      cJj = rep(0, n_clean),
      aIj = rep(1, n_clean),
      bIj = d2_clean$param,
      cIj = rep(0, n_clean)
    )

    res_link <- SNSequate::irt.link(
      parm = parm_df,
      common = 1:n_clean,
      model = "1PL",
      icc = "logistic",
      D = 1.7
    )

    data.frame(
      Equating = nama_pasang,
      Kombinasi = label_kombinasi,
      Anchor_Awal = n_awal,
      Anchor_Bersih = n_clean,
      Mean.Mean = round(
        res_link$mm[2],
        4
      ),
      Haebara = round(
        res_link$Haebara[2],
        4
      ),
      Stocking.Lord = round(
        res_link$StockLord[2],
        4
      ),
      stringsAsFactors = FALSE
    )
  }

  for (j in seq_along(jenjang)) {

    for (k in seq_along(mapel)) {

      nm <- paste0(
        jenjang[j],
        "_",
        mapel[k]
      )

      cat(
        "\n===== Processing:",
        nm,
        "=====\n"
      )

      f24 <- grep(
        "_24",
        list.param.file,
        value = TRUE
      )

      f25 <- grep(
        "_25",
        list.param.file,
        value = TRUE
      )

      f26 <- grep(
        "_26",
        list.param.file,
        value = TRUE
      )

      f24K <- grep(
        mapel[k],
        grep(
          jenjang[j],
          f24,
          value = TRUE
        ),
        value = TRUE
      )

      f25K <- grep(
        mapel[k],
        grep(
          jenjang[j],
          f25,
          value = TRUE
        ),
        value = TRUE
      )

      f26K <- grep(
        mapel[k],
        grep(
          jenjang[j],
          f26,
          value = TRUE
        ),
        value = TRUE
      )

      par24 <- read.csv(
        f24K[1],
        skip = 1
      )

      par25 <- read.csv(
        f25K[1],
        skip = 1
      )

      par26 <- read.csv(
        f26K[1],
        skip = 1
      )

      par.24X <- clean_param_df(
        par24
      )

      par.25X <- clean_param_df(
        par25
      )

      par.26X <- clean_param_df(
        par26
      )

      res_2425 <- process_pair(
        par.24X,
        par.25X,
        "2024-2025",
        nm
      )

      res_2426 <- process_pair(
        par.24X,
        par.26X,
        "2024-2026",
        nm
      )

      hasil[[no]] <- rbind(
        res_2425,
        res_2426
      )

      no <- no + 1
    }
  }

   res<- do.call(
    rbind,
    hasil
  )
    print(res)
  do.call(
    rbind,
    hasil
  )
}
