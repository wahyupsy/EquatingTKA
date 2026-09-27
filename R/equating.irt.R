equating.irt <- function(fd) {
require(statmod)
  
  list.param <- list.files(
    path = fd,
    pattern = "item",
    full.names = TRUE
  )

  jenjang <- c("SD", "SMP")
  mapel <- c("Lit", "Num")

  hasil_equating <- data.frame(
    Equating = character(),
    Kombinasi = character(),
    Num_Anchor = numeric(),
    Mean.Mean = numeric(),
    Haebara = numeric(),
    Stocking.Lord = numeric(),
    stringsAsFactors = FALSE
  )

  hasil.2425 <- list()
  hasil.2426 <- list()

  for (j in seq_along(jenjang)) {
    for (k in seq_along(mapel)) {

      nm <- paste0(jenjang[j], "_", mapel[k])

      cat("\n===== Processing:", nm, "=====\n")

      tryCatch({

        f24 <- grep("_24", list.param, value = TRUE)
        f25 <- grep("_25", list.param, value = TRUE)
        f26 <- grep("_26", list.param, value = TRUE)

        f24J <- grep(jenjang[j], f24, value = TRUE)
        f25J <- grep(jenjang[j], f25, value = TRUE)
        f26J <- grep(jenjang[j], f26, value = TRUE)

        f24K <- grep(mapel[k], f24J, value = TRUE)
        f25K <- grep(mapel[k], f25J, value = TRUE)
        f26K <- grep(mapel[k], f26J, value = TRUE)

        par.24 <- read.csv(f24K[1], skip = 1)
        par.25 <- read.csv(f25K[1], skip = 1)
        par.26 <- read.csv(f26K[1], skip = 1)

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


        # =================================================
        # 2024 - 2025
        # =================================================

        common_2425 <- intersect(
          par.24X$item,
          par.25X$item
        )

        d24_c <- par.24X[
          par.24X$item %in% common_2425,
        ]

        d25_c <- par.25X[
          par.25X$item %in% common_2425,
        ]

        d24_c <- d24_c[order(d24_c$item), ]
        d25_c <- d25_c[order(d25_c$item), ]

        mat24 <- cbind(
          a = 1,
          d = d24_c$param
        )

        mat25 <- cbind(
          a = 1,
          d = d25_c$param
        )

        rownames(mat24) <- d24_c$item
        rownames(mat25) <- d25_c$item

        mod_2425 <- modIRT(
          coef = list(mat24, mat25),
          var = NULL,
          display = FALSE
        )

        sl_2425 <- summary(
          direc(
            mods = mod_2425,
            which = c(1, 2),
            method = "Stocking-Lord"
          )
        )$coef["B", "Estimate"]

        mm_2425 <- summary(
          direc(
            mods = mod_2425,
            which = c(1, 2),
            method = "mean-mean"
          )
        )$coef["B", "Estimate"]

        hb_2425 <- summary(
          direc(
            mods = mod_2425,
            which = c(1, 2),
            method = "Haebara"
          )
        )$coef["B", "Estimate"]


        # Simpan detail 2024-2025
        hasil.2425[[nm]] <- list(
          par.24 = par.24X,
          par.25 = par.25X,
          common.item = common_2425,
          model = mod_2425,
          Mean.Mean = mm_2425,
          Haebara = hb_2425,
          Stocking.Lord = sl_2425
        )


        # =================================================
        # 2024 - 2026
        # =================================================

        common_2426 <- intersect(
          par.24X$item,
          par.26X$item
        )

        d24_c2 <- par.24X[
          par.24X$item %in% common_2426,
        ]

        d26_c <- par.26X[
          par.26X$item %in% common_2426,
        ]

        d24_c2 <- d24_c2[order(d24_c2$item), ]
        d26_c <- d26_c[order(d26_c$item), ]

        mat24_2 <- cbind(
          a = 1,
          d = d24_c2$param
        )

        mat26 <- cbind(
          a = 1,
          d = d26_c$param
        )

        rownames(mat24_2) <- d24_c2$item
        rownames(mat26) <- d26_c$item

        mod_2426 <- modIRT(
          coef = list(mat24_2, mat26),
          var = NULL,
          display = FALSE
        )

        sl_2426 <- summary(
          direc(
            mods = mod_2426,
            which = c(1, 2),
            method = "Stocking-Lord"
          )
        )$coef["B", "Estimate"]

        mm_2426 <- summary(
          direc(
            mods = mod_2426,
            which = c(1, 2),
            method = "mean-mean"
          )
        )$coef["B", "Estimate"]

        hb_2426 <- summary(
          direc(
            mods = mod_2426,
            which = c(1, 2),
            method = "Haebara"
          )
        )$coef["B", "Estimate"]


        # Simpan detail 2024-2026
        hasil.2426[[nm]] <- list(
          par.24 = par.24X,
          par.26 = par.26X,
          common.item = common_2426,
          model = mod_2426,
          Mean.Mean = mm_2426,
          Haebara = hb_2426,
          Stocking.Lord = sl_2426
        )


        # =================================================
        # RANGKUMAN
        # =================================================

        hasil_equating <- rbind(
          hasil_equating,

          data.frame(
            Equating = "2024-2025",
            Kombinasi = nm,
            Num_Anchor = length(common_2425),
            Mean.Mean = round(mm_2425, 4),
            Haebara = round(hb_2425, 4),
            Stocking.Lord = round(sl_2425, 4)
          ),

          data.frame(
            Equating = "2024-2026",
            Kombinasi = nm,
            Num_Anchor = length(common_2426),
            Mean.Mean = round(mm_2426, 4),
            Haebara = round(hb_2426, 4),
            Stocking.Lord = round(sl_2426, 4)
          )
        )

        cat("✓ Berhasil memproses", nm, "\n")

      }, error = function(e) {

        cat(
          "❌ Gagal pada",
          nm,
          ":",
          e$message,
          "\n"
        )
      })
    }
  }

  # =====================================================
  # OUTPUT
  # =====================================================

    print(hasil_equating[[1]])

  return(
    list(
      hasil_equating,
      list(
        hasil.2425 = hasil.2425,
        hasil.2426 = hasil.2426
      )
    )
  )
}



# -----------------

direc <- function(mods, which = NULL, mod1, mod2, method = "mean-mean", suff1 = ".1", suff2 = ".2", D = 1, quadrature = TRUE, nq = 30, items.select = NULL)
{
 if (method!= "mean-mean" & method!= "mean-sigma" & method!= "mean-gmean" & method!= "Haebara" & method!= "Stocking-Lord") warning("Method not implemented.")
 if (!missing(mod1)) { # new in version 2.0-3
 warning("Argument mod1 is deprecated; please use mods and which instead.", call. = FALSE) # new in version 2.0-3
 } # new from version 2.0-3
 if (!missing(mod2)) { # new in version 2.0-3
 warning("Argument mod2 is deprecated; please use mods and which instead.", call. = FALSE) # new in version 2.0-3
 } # new from version 2.0-3
 if (!missing(mods)) { # new from version 2.0-3	
 mod1 <- mods[which[1]] # new from version 2.0-3
 mod2 <- mods[which[2]] # new from version 2.0-3
 } # new from version 2.0-3
 
 name1 <- names(mod1)
 name2 <- names(mod2)
 forms <- paste(name1, name2, sep = ".")
 mod1 <- mod1[[1]]
 mod2 <- mod2[[1]]
 tab1 <- data.frame(value1 = mod1$coef)
 tab2 <- data.frame(value2 = mod2$coef)
 tab1$tmp <- NA
 tab1 <- tab1[order(rownames(tab1)), ]
 tab1$tmp <- c()
 tab2$tmp <- NA
 tab2 <- tab2[order(rownames(tab2)), ]
 tab2$tmp <- c()
 var1 <- mod1$var
 var2 <- mod2$var
 itmp1 <- mod1$itmp
 itmp2 <- mod2$itmp
 if (itmp1!= itmp2) stop("Mixed type models not allowed.")
 itmp <- itmp1
 tab <- merge(tab1, tab2, by = 0)
 if (nrow(tab) == 0) {
 warning("no common items")
 out <- list(ni = 0)
 }
 if (nrow(tab)>0) {
 comuni <- tab$Row.names
 taball <- merge(tab1, tab2, by = 0, all = T)
 taball$value12 <- NA
 niall <- nrow(taball)/itmp
 ni <- nrow(tab)/itmp # number of common items
 if (ni == 0) comuni <- "None"
 if (any(!items.select%in%substring(tab$Row.names, 8))) {
  itemout <- items.select[!items.select%in%substring(tab$Row.names, 8)] 
  if (length(itemout == 1)) warning("Item ", itemout, " is not in item list.\n")
  if (length(itemout>1)) warning("Items ", itemout, " are not in item list.\n")
 }
 if (!is.null(items.select)) {
  tab$select <- (substring(tab$Row.names, 8)%in%items.select)*1
  taball$select <- (substring(taball$Row.names, 8)%in%items.select)*1
 }
 else {
  tab$select <- 1
  taball$select <- 0
  taball$select[taball$Rownames%in%tab$Rownames] <- 1
 }
 com.select <- tab$Row.names[tab$select == 1]
 tabDff <- tab[substr(tab$Row.names, 1, 6) == "Dffclt", ]
 tabDsc <- tab[substr(tab$Row.names, 1, 6) == "Dscrmn", ]
 tabGss <- tab[substr(tab$Row.names, 1, 6) == "Gussng", ]
 if (!is.null(items.select)) {
  tabDff <- tabDff[tabDff$select == 1, ]
  tabDsc <- tabDsc[tabDsc$select == 1, ]
  tabGss <- tabGss[tabGss$select == 1, ]
 }
 nis <- nrow(tabDff) #number of common items selected
 if (nrow(tabDff) == 0 & ni>0) warning("missing difficulty parameters")
 if (nrow(tabGss)!= nrow(tabDff) & itmp == 3) warning("missing guessing parameters")
 if (nrow(tabDsc)!= nrow(tabDff) & itmp == 2) warning("missing discrimination parameters")
 ifelse (itmp>1, a1 <- tabDsc$value1, a1 <- rep(1, nis))
 ifelse (itmp>1, a2 <- tabDsc$value2, a2 <- rep(1, nis))
 b1 <- tabDff$value1
 b2 <- tabDff$value2
 ifelse (itmp == 3, c1 <- tabGss$value1, c1 <- rep(0, nis))
 ifelse (itmp == 3, c2 <- tabGss$value2, c2 <- rep(0, nis))
 
 if (method == "mean-sigma" & itmp>1) A <- sd(b2)/sd(b1)
 if (method == "mean-sigma" & itmp == 1) A <- 1
 if (method == "mean-mean") A <- mean(a1)/mean(a2)
 if (method == "mean-gmean") A <- exp(sum(log(a1/a2)))^(1/nis)
 if (method == "mean-sigma" | method == "mean-mean" | method == "mean-gmean") 
  B <- mean(b2)-A*mean(b1)
 if (method == "Haebara" | method == "Stocking-Lord") {
  if (quadrature) {
  gq <- gauss.quad.prob(nq, dist = "normal")
  ab <- gq$nodes
  wt <- gq$weights
  }
  else {
  ab <- seq(-4, 4, l = 40)
  wt <- rep(1, 40)
  }
  P2 <- matrix(NA, length(ab), nis)
  for (i in 1:nis)
  P2[, i] <- irtp1(ab, diff = b2[i], discr = a2[i], guess = c2[i], D = D)
  par <- nlminb(start = c(1, 0), objective = obj, P2 = P2, ab = ab, a1 = a1, b1 = b1, c1 = c1, met = method, itmp = itmp, wt = wt, D = D)$par
  
  A <- par[1]
  B <- par[2]
  b12 <- A*b1+B
  a12 <- a1/A
  P1 <- matrix(NA, length(ab), nis)
  for (i in 1:nis)
  P1[, i] <- irtp1(ab, diff = b12[i], discr = a12[i], guess = c1[i], D = D)
 }
 if (!is.null(var1) & !is.null(var2)) {
  if (method == "mean-sigma") {
  partialA_b2 <- A*sd(b2)^(-2)*(b2-mean(b2))/nis
  partialA_b1 <- -A*sd(b1)^(-2)*(b1-mean(b1))/nis
  if (itmp == 1) {partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)}
  partialB_b2 <- 1/nis-partialA_b2*mean(b1)
  partialB_b1 <- -partialA_b1*mean(b1)-A/nis
  partialA_a2 <- rep(0, nis)
  partialA_a1 <- rep(0, nis)
  partialB_a2 <- rep(0, nis)
  partialB_a1 <- rep(0, nis)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "mean-mean") {
  partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)
  partialB_b2 <- rep(1/nis, nis)
  partialB_b1 <- rep(-A/nis, nis)
  partialA_a2 <- rep(-sum(a1)/(sum(a2)^2), nis)
  partialA_a1 <- rep(1/sum(a2), nis)
  partialB_a2 <- -partialA_a2*mean(b1)
  partialB_a1 <- -partialA_a1*mean(b1)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "mean-gmean") {
  partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)
  partialB_b2 <- rep(1/nis, nis)
  partialB_b1 <- rep(-A/nis, nis)
  partialA_a2 <- -1/nis*A/a2
  partialA_a1 <- 1/nis*A/a1
  partialB_a2 <- -partialA_a2*mean(b1)
  partialB_a1 <- -partialA_a1*mean(b1)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "Haebara") {
  P1 <- t(P1)
  P2 <- t(P2)
  tmp <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*(P1-c1)/(1-c1)^2*(1-P1)*a1^2*D
  tmp <- apply(tmp, 2, sum)*wt
  partialSIR_AB <- matrix(0, 2, 2)
  for (i in 1:length(ab)) 
   partialSIR_AB <- partialSIR_AB-tmp[i]*c(ab[i], 1)%*%t(c((ab[i]-B)/A^2, 1/A))
  if (itmp == 1) {partialSIR_AB[1, ] <- 0
  partialSIR_AB[, 1] <- 0}
  abMAT <- matx(ab, nis)
  wtMAT <- matx(wt, nis)
  tmp <- (P1-c1)/(1-c1)*(1-P1)*a1*(1-P2)/(1-c2)*wtMAT
  tmp_a2 <- tmp*(P2-c2)*D*(abMAT-b2)
  partialSIR_a2 <- rbind(rowSums(tmp_a2*abMAT), rowSums(tmp_a2))
  tmp_b2 <- tmp*(P2-c2)*D*(-a2)
  partialSIR_b2 <- rbind(rowSums(tmp_b2*abMAT), rowSums(tmp_b2))
  partialSIR_c2 <- rbind(rowSums(tmp*abMAT), rowSums(tmp))
  tmp_a1 <- (((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*a1/(1-c1)*
     D*(abMAT-A*b1-B)/A+P2-P1)*(P1-c1)/(1-c1)*(1-P1)*wtMAT
  partialSIR_a1 <- rbind(rowSums(tmp_a1*abMAT), rowSums(tmp_a1))
  tmp_b1 <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*
   (P1-c1)/(1-c1)^2*(1-P1)*(-D*a1^2)*wtMAT
  partialSIR_b1 <- rbind(rowSums(tmp_b1*abMAT), rowSums(tmp_b1))
  tmp_c1 <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2-(P2-P1)*(1-P1))*
   (1-P1)*a1/(1-c1)^2*wtMAT
  partialSIR_c1 <- rbind(rowSums(tmp_c1*abMAT), rowSums(tmp_c1))
  }
  if (method == "Stocking-Lord") {
  P1 <- t(P1)
  P2 <- t(P2)
  tmp1 <- -(P1-c1)/(1-c1)*(1-P1)*a1
  tmp2 <- P2-P1
  tmp3 <- (1+c1-2*P1)/(1-c1)*a1
  tmp4 <- (P1-c1)/(1-c1)*(1-P1)*a1*D
  tmp <- colSums(tmp1)*colSums(tmp4)+colSums(tmp2)*colSums(tmp3*tmp4)
  tmp <- tmp*wt
  partialSIR_AB <- matrix(0, 2, 2)
  for (i in 1:length(ab)) 
   partialSIR_AB <- partialSIR_AB-tmp[i]*c(ab[i], 1)%*%t(c((ab[i]-B)/A^2, 1/A))
  if (itmp == 1) {partialSIR_AB[1, ] <- 0
  partialSIR_AB[, 1] <- 0}
  abMAT <- matrix(rep(ab, each = nis), nrow = nis)
  abMAT <- matx(ab, nis)
  wtMAT <- matx(wt, nis)
  tmp <- (P1-c1)/(1-c1)*(1-P1)*a1
  tmp <- matx(colSums(tmp), nis)*(1-P2)/(1-c2)*wtMAT
  tmp_a2 <- tmp*(P2-c2)*D*(abMAT-b2)
  partialSIR_a2 <- rbind(rowSums(tmp_a2*abMAT), rowSums(tmp_a2))
  tmp_b2 <- tmp*(P2-c2)*D*(-a2)
  partialSIR_b2 <- rbind(rowSums(tmp_b2*abMAT), rowSums(tmp_b2))
  partialSIR_c2 <- rbind(rowSums(tmp*abMAT), rowSums(tmp))
  tmp1 <- -(P1-c1)/(1-c1)*(1-P1)*a1
  tmp2 <- P2-P1
  tmp3 <- (1+c1-2*P1)/(1-c1)*a1
  tmp4 <- D*(abMAT-A*b1-B)/A
  tmp6 <- (P1-c1)/(1-c1)*(1-P1)
  tmp_a1 <- matx(colSums(tmp1), nis)*(tmp4*tmp6)+matx(colSums(tmp2), nis)*(tmp3*tmp4*tmp6)+matx(colSums(tmp2), nis)*(tmp6)
  tmp_a1 <- tmp_a1*wtMAT
  partialSIR_a1 <- rbind(rowSums(tmp_a1*abMAT), rowSums(tmp_a1))
  tmp61 <- -tmp6*D*a1
  tmp_b1 <- matx(colSums(tmp1), nis)*tmp61+matx(colSums(tmp2), nis)*(tmp3*tmp61)
  tmp_b1 <- tmp_b1*wtMAT
  partialSIR_b1 <- rbind(rowSums(tmp_b1*abMAT), rowSums(tmp_b1))
  tmp7 <- (P1-c1)/(1-c1)*a1
  tmp8 <- (1-P1)/(1-c1)
  tmp_c1 <- matx(colSums(tmp1), nis)*tmp8-matx(colSums(tmp2), nis)*(tmp7*tmp8)
  tmp_c1 <- tmp_c1*wtMAT
  partialSIR_c1 <- rbind(rowSums(tmp_c1*abMAT), rowSums(tmp_c1))
  }
  
  if (method == "mean-mean" | method == "mean-sigma" | method == "mean-gmean") { 
  if (itmp == 1) mat <- cbind(
   c(partialA_b1, partialA_b2), 
   c(partialB_b1, partialB_b2))
  if (itmp == 2) mat <- cbind(
   c(partialA_b1, partialA_a1, partialA_b2, partialA_a2), 
   c(partialB_b1, partialB_a1, partialB_b2, partialB_a2))
  if (itmp == 3) mat <- cbind(
   c(partialA_b1, partialA_a1, partialA_c1, partialA_b2, partialA_a2, partialA_c2), 
   c(partialB_b1, partialB_a1, partialB_c1, partialB_b2, partialB_a2, partialB_c2))
  }
  if (method == "Haebara" | method == "Stocking-Lord") {
  if (itmp == 1) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_b2)
  if (itmp == 1) partialSIR_gamma[1, ] <- 0
  if (itmp == 2) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_a1, partialSIR_b2, partialSIR_a2)
  if (itmp == 3) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_a1, partialSIR_c1, partialSIR_b2, partialSIR_a2, partialSIR_c2)
  if (itmp == 1) {invpartialSIR_AB <- partialSIR_AB
  invpartialSIR_AB[2, 2] <- 1/invpartialSIR_AB[2, 2]}
  if (itmp == 1) mat1 <- -invpartialSIR_AB%*%partialSIR_gamma
  if (itmp>1) mat1 <- -solve(partialSIR_AB)%*%partialSIR_gamma
  mat <- t(mat1)
  }
  if (nis == 0) mat <- matrix(NA, 2, 2)
  colnames(mat) <- c("A", "B")

  rownames(mat) <- c(paste(com.select, suff1, sep = ""), paste(com.select, suff2, sep = ""))
  if (nis>0) {
  var1r <- var1[tab$Row.names[tab$select == 1], tab$Row.names[tab$select == 1]]
  var2r <- var2[tab$Row.names[tab$select == 1], tab$Row.names[tab$select == 1]]
  rownames(var1r) <- colnames(var1r) <- paste(rownames(var1r), suff1, sep = "")
  rownames(var2r) <- colnames(var2r) <- paste(rownames(var2r), suff2, sep = "")
  var12 <- blockdiag(var1r, var2r)
  varAB <- t(mat)%*%var12[rownames(mat), rownames(mat)]%*%mat
  }
  else { 
  varAB <- matrix(NA, 2, 2)
  var12 <- NULL 
  }
 }
 if (is.null(var1) | is.null(var2)) {
  var12 <- NULL
  mat <- NULL
  varAB <- matrix(NA, 2, 2)
 }
 if (!is.null(var12)) {
  rownames(var1) <- colnames(var1) <- paste(rownames(var1), suff1, sep = "")
  rownames(var2) <- colnames(var2) <- paste(rownames(var2), suff2, sep = "")
  varFull <- list(var1, var2)
 }
 else varFull <- NULL
 taball$value12[1:niall] <- A*taball$value1[1:niall]+B
 if (itmp>1) taball$value12[(niall+1):(2*niall)] <- taball$value1[(niall+1):(2*niall)]/A
 if (itmp == 3) taball$value12[(2*niall+1):(3*niall)] <- taball$value1[(2*niall+1):(3*niall)]
 colnames(taball) <- c("Item", name1, name2, paste(name1, name2, sep = ".as."), "select")
 out <- list(tab1 = tab1, tab2 = tab2, tab = taball, var12 = var12, varFull = varFull, partial = mat, A = A, B = B, 
    varAB = varAB, commonitem = list(common = comuni, common.select = com.select), 
    suffixes = c(suff1, suff2), ni = ni, nis = nis)
 
 }
 out$forms <- forms
 out$method <- method
 out$itmp <- itmp
 class(out) <- "eqc"
 return(out)
}


modIRT <- function(est.mods = NULL, coef = NULL, var = NULL, names = NULL, ltparam = TRUE, lparam = TRUE, display = TRUE, digits = 2)
{
	if (!is.null(est.mods))
	{
	  ltparam <- TRUE
	  lparam <- TRUE
	  m1<-est.mods[[1]]
	  cl<-class(m1)
	  if (cl=="ltm")
	  {
      imports<-lapply(est.mods,import.ltm,display=FALSE)
      coef<-lapply(imports,function(x) x$coef)
      var<-lapply(imports,function(x) x$var)
	  }
	  else 
	  {
	    if (attributes(cl)$package=="mirt")
	    {
	      imports<-lapply(est.mods,import.mirt,display=FALSE)
	      coef<-lapply(imports,function(x) x$coef)
	      var<-lapply(imports,function(x) x$var)
	    }
	    else stop("est.mods should be a list including the models estimated with packages ltm or mirt")
	  }
	}
  coef <- lapply(coef, FUN = function(x) x <- subset(x, select = colSums(x == 1)!= nrow(x)))
	coef <- lapply(coef, FUN = function(x) x <- subset(x, select = colSums(x == 0)!= nrow(x)))
	itmp <- sapply(coef, ncol)
	if (length(unique(itmp))!= 1) stop("Mixed model types not allowed.")
	itmp <- itmp[1]
	if (itmp == 1 | itmp == 2) lparam <- FALSE
	lc <- length(coef)
	if (is.null(names)) names <- paste("T", 1:lc, sep = "")
	mods <- list()
	for (i in 1:lc) {
		c0 <- c1 <- c2 <- NULL
		D1 <- D2 <- matrix(NA, 0, 0)
		coefi <- coef[[i]]
		if (!is.null(var)) vari <- var[[i]]
		else vari <- NULL
		if (!is.null(vari)) if (any(is.na(vari))) vari <- NULL
		n1 <- nrow(coefi)
		if (itmp == 1)
			c1 <- coefi[, 1]
		if (itmp == 2) { 
			c1 <- coefi[, 1]
			c2 <- coefi[, 2]
		}
		if (itmp == 3) { 
			c0 <- coefi[, 1]
			c1 <- coefi[, 2]
			c2 <- coefi[, 3]
		}
		if (ltparam & itmp == 1) {
			c1 <- -c1
		}
		if (ltparam & itmp>1) {
			D1 <- rbind(diag(-1/c2), diag(c1/c2^2))
			D1 <- cbind(D1, rbind(matrix(0, n1, n1), diag(1, n1)))
			c1 <- -c1/c2
		}
		if (lparam) {
			D2 <- diag(exp(c0)/(1+exp(c0))^2)
			c0 <- exp(c0)/(1+exp(c0))
		}
		if (!lparam & itmp == 3) {
			D2 <- diag(1, n1)
		}
		if (!is.null(vari)) {
			if ((ltparam | lparam) & itmp>1) {
				D1 <- blockdiag(D2, D1)
				vari <- t(D1)%*%vari%*%D1
			}
		}
		if (itmp == 3) names(c0) <- paste("Gussng", names(c0), sep = ".")
		names(c1) <- paste("Dffclt", names(c1), sep = ".")
		if (itmp>1) names(c2) <- paste("Dscrmn", names(c2), sep = ".")
		coefi <- c(c0, c1, c2)
		if (!is.null(vari)) rownames(vari) <- colnames(vari) <- names(coefi)
		mods[[i]] <- list(coefficients = coefi, var = vari, itmp = itmp)
		if (display) {
			if (!is.null(var)) out <- cbind(coefi, diag(vari)^0.5)
			else out <- cbind(coefi, NA)
			out <- round(out, digits)
			colnames(out) <- c("value", "std.err")
			cat("Form:", names[i], "\n")
			print(out)
		}
	}
	names(mods) <- names
	class(mods) <- "modIRT"
	return(mods)
}


irtp1 <- function(ab, diff, discr, guess, D)
{
	elp <- exp(D*discr*(ab-diff))
	if(any(is.infinite(elp))) elp[is.infinite(elp)] <- 1e10
	guess+(1-guess)*elp/(1+elp)
}



obj <- function(eqc, P2, ab, a1, b1, c1, met, itmp, wt, D = D){
	ifelse(itmp == 1, A <- 1, A <- eqc[1])
	B <- eqc[2]

	b12 <- A*b1+B
	a12 <- a1/A

	ni <- ncol(P2)
	P1 <- matrix(NA, length(ab), ni)
	for (i in 1:ni)
		P1[, i] <- irtp1(ab, diff = b12[i], discr = a12[i], guess = c1[i], D = D)

	if (met == "Haebara") f <- 0.5*sum(rowSums((P2-P1)^2)*wt)
	if (met == "Stocking-Lord") f <- 0.5*sum(((rowSums(P2)-rowSums(P1))^2)*wt)
	return(f)
}


direc <- function(mods, which = NULL, mod1, mod2, method = "mean-mean", suff1 = ".1", suff2 = ".2", D = 1, quadrature = TRUE, nq = 30, items.select = NULL)
{
 if (method!= "mean-mean" & method!= "mean-sigma" & method!= "mean-gmean" & method!= "Haebara" & method!= "Stocking-Lord") warning("Method not implemented.")
 if (!missing(mod1)) { # new in version 2.0-3
 warning("Argument mod1 is deprecated; please use mods and which instead.", call. = FALSE) # new in version 2.0-3
 } # new from version 2.0-3
 if (!missing(mod2)) { # new in version 2.0-3
 warning("Argument mod2 is deprecated; please use mods and which instead.", call. = FALSE) # new in version 2.0-3
 } # new from version 2.0-3
 if (!missing(mods)) { # new from version 2.0-3	
 mod1 <- mods[which[1]] # new from version 2.0-3
 mod2 <- mods[which[2]] # new from version 2.0-3
 } # new from version 2.0-3
 
 name1 <- names(mod1)
 name2 <- names(mod2)
 forms <- paste(name1, name2, sep = ".")
 mod1 <- mod1[[1]]
 mod2 <- mod2[[1]]
 tab1 <- data.frame(value1 = mod1$coef)
 tab2 <- data.frame(value2 = mod2$coef)
 tab1$tmp <- NA
 tab1 <- tab1[order(rownames(tab1)), ]
 tab1$tmp <- c()
 tab2$tmp <- NA
 tab2 <- tab2[order(rownames(tab2)), ]
 tab2$tmp <- c()
 var1 <- mod1$var
 var2 <- mod2$var
 itmp1 <- mod1$itmp
 itmp2 <- mod2$itmp
 if (itmp1!= itmp2) stop("Mixed type models not allowed.")
 itmp <- itmp1
 tab <- merge(tab1, tab2, by = 0)
 if (nrow(tab) == 0) {
 warning("no common items")
 out <- list(ni = 0)
 }
 if (nrow(tab)>0) {
 comuni <- tab$Row.names
 taball <- merge(tab1, tab2, by = 0, all = T)
 taball$value12 <- NA
 niall <- nrow(taball)/itmp
 ni <- nrow(tab)/itmp # number of common items
 if (ni == 0) comuni <- "None"
 if (any(!items.select%in%substring(tab$Row.names, 8))) {
  itemout <- items.select[!items.select%in%substring(tab$Row.names, 8)] 
  if (length(itemout == 1)) warning("Item ", itemout, " is not in item list.\n")
  if (length(itemout>1)) warning("Items ", itemout, " are not in item list.\n")
 }
 if (!is.null(items.select)) {
  tab$select <- (substring(tab$Row.names, 8)%in%items.select)*1
  taball$select <- (substring(taball$Row.names, 8)%in%items.select)*1
 }
 else {
  tab$select <- 1
  taball$select <- 0
  taball$select[taball$Rownames%in%tab$Rownames] <- 1
 }
 com.select <- tab$Row.names[tab$select == 1]
 tabDff <- tab[substr(tab$Row.names, 1, 6) == "Dffclt", ]
 tabDsc <- tab[substr(tab$Row.names, 1, 6) == "Dscrmn", ]
 tabGss <- tab[substr(tab$Row.names, 1, 6) == "Gussng", ]
 if (!is.null(items.select)) {
  tabDff <- tabDff[tabDff$select == 1, ]
  tabDsc <- tabDsc[tabDsc$select == 1, ]
  tabGss <- tabGss[tabGss$select == 1, ]
 }
 nis <- nrow(tabDff) #number of common items selected
 if (nrow(tabDff) == 0 & ni>0) warning("missing difficulty parameters")
 if (nrow(tabGss)!= nrow(tabDff) & itmp == 3) warning("missing guessing parameters")
 if (nrow(tabDsc)!= nrow(tabDff) & itmp == 2) warning("missing discrimination parameters")
 ifelse (itmp>1, a1 <- tabDsc$value1, a1 <- rep(1, nis))
 ifelse (itmp>1, a2 <- tabDsc$value2, a2 <- rep(1, nis))
 b1 <- tabDff$value1
 b2 <- tabDff$value2
 ifelse (itmp == 3, c1 <- tabGss$value1, c1 <- rep(0, nis))
 ifelse (itmp == 3, c2 <- tabGss$value2, c2 <- rep(0, nis))
 
 if (method == "mean-sigma" & itmp>1) A <- sd(b2)/sd(b1)
 if (method == "mean-sigma" & itmp == 1) A <- 1
 if (method == "mean-mean") A <- mean(a1)/mean(a2)
 if (method == "mean-gmean") A <- exp(sum(log(a1/a2)))^(1/nis)
 if (method == "mean-sigma" | method == "mean-mean" | method == "mean-gmean") 
  B <- mean(b2)-A*mean(b1)
 if (method == "Haebara" | method == "Stocking-Lord") {
  if (quadrature) {
  gq <- gauss.quad.prob(nq, dist = "normal")
  ab <- gq$nodes
  wt <- gq$weights
  }
  else {
  ab <- seq(-4, 4, l = 40)
  wt <- rep(1, 40)
  }
  P2 <- matrix(NA, length(ab), nis)
  for (i in 1:nis)
  P2[, i] <- irtp1(ab, diff = b2[i], discr = a2[i], guess = c2[i], D = D)
  par <- nlminb(start = c(1, 0), objective = obj, P2 = P2, ab = ab, a1 = a1, b1 = b1, c1 = c1, met = method, itmp = itmp, wt = wt, D = D)$par
  
  A <- par[1]
  B <- par[2]
  b12 <- A*b1+B
  a12 <- a1/A
  P1 <- matrix(NA, length(ab), nis)
  for (i in 1:nis)
  P1[, i] <- irtp1(ab, diff = b12[i], discr = a12[i], guess = c1[i], D = D)
 }
 if (!is.null(var1) & !is.null(var2)) {
  if (method == "mean-sigma") {
  partialA_b2 <- A*sd(b2)^(-2)*(b2-mean(b2))/nis
  partialA_b1 <- -A*sd(b1)^(-2)*(b1-mean(b1))/nis
  if (itmp == 1) {partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)}
  partialB_b2 <- 1/nis-partialA_b2*mean(b1)
  partialB_b1 <- -partialA_b1*mean(b1)-A/nis
  partialA_a2 <- rep(0, nis)
  partialA_a1 <- rep(0, nis)
  partialB_a2 <- rep(0, nis)
  partialB_a1 <- rep(0, nis)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "mean-mean") {
  partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)
  partialB_b2 <- rep(1/nis, nis)
  partialB_b1 <- rep(-A/nis, nis)
  partialA_a2 <- rep(-sum(a1)/(sum(a2)^2), nis)
  partialA_a1 <- rep(1/sum(a2), nis)
  partialB_a2 <- -partialA_a2*mean(b1)
  partialB_a1 <- -partialA_a1*mean(b1)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "mean-gmean") {
  partialA_b2 <- rep(0, nis)
  partialA_b1 <- rep(0, nis)
  partialB_b2 <- rep(1/nis, nis)
  partialB_b1 <- rep(-A/nis, nis)
  partialA_a2 <- -1/nis*A/a2
  partialA_a1 <- 1/nis*A/a1
  partialB_a2 <- -partialA_a2*mean(b1)
  partialB_a1 <- -partialA_a1*mean(b1)
  partialA_c2 <- rep(0, nis)
  partialA_c1 <- rep(0, nis)
  partialB_c2 <- rep(0, nis)
  partialB_c1 <- rep(0, nis)
  }
  if (method == "Haebara") {
  P1 <- t(P1)
  P2 <- t(P2)
  tmp <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*(P1-c1)/(1-c1)^2*(1-P1)*a1^2*D
  tmp <- apply(tmp, 2, sum)*wt
  partialSIR_AB <- matrix(0, 2, 2)
  for (i in 1:length(ab)) 
   partialSIR_AB <- partialSIR_AB-tmp[i]*c(ab[i], 1)%*%t(c((ab[i]-B)/A^2, 1/A))
  if (itmp == 1) {partialSIR_AB[1, ] <- 0
  partialSIR_AB[, 1] <- 0}
  abMAT <- matx(ab, nis)
  wtMAT <- matx(wt, nis)
  tmp <- (P1-c1)/(1-c1)*(1-P1)*a1*(1-P2)/(1-c2)*wtMAT
  tmp_a2 <- tmp*(P2-c2)*D*(abMAT-b2)
  partialSIR_a2 <- rbind(rowSums(tmp_a2*abMAT), rowSums(tmp_a2))
  tmp_b2 <- tmp*(P2-c2)*D*(-a2)
  partialSIR_b2 <- rbind(rowSums(tmp_b2*abMAT), rowSums(tmp_b2))
  partialSIR_c2 <- rbind(rowSums(tmp*abMAT), rowSums(tmp))
  tmp_a1 <- (((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*a1/(1-c1)*
     D*(abMAT-A*b1-B)/A+P2-P1)*(P1-c1)/(1-c1)*(1-P1)*wtMAT
  partialSIR_a1 <- rbind(rowSums(tmp_a1*abMAT), rowSums(tmp_a1))
  tmp_b1 <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2)*
   (P1-c1)/(1-c1)^2*(1-P1)*(-D*a1^2)*wtMAT
  partialSIR_b1 <- rbind(rowSums(tmp_b1*abMAT), rowSums(tmp_b1))
  tmp_c1 <- ((c1+1)*P2+c1-2*(P2+c1+1)*P1+3*P1^2-(P2-P1)*(1-P1))*
   (1-P1)*a1/(1-c1)^2*wtMAT
  partialSIR_c1 <- rbind(rowSums(tmp_c1*abMAT), rowSums(tmp_c1))
  }
  if (method == "Stocking-Lord") {
  P1 <- t(P1)
  P2 <- t(P2)
  tmp1 <- -(P1-c1)/(1-c1)*(1-P1)*a1
  tmp2 <- P2-P1
  tmp3 <- (1+c1-2*P1)/(1-c1)*a1
  tmp4 <- (P1-c1)/(1-c1)*(1-P1)*a1*D
  tmp <- colSums(tmp1)*colSums(tmp4)+colSums(tmp2)*colSums(tmp3*tmp4)
  tmp <- tmp*wt
  partialSIR_AB <- matrix(0, 2, 2)
  for (i in 1:length(ab)) 
   partialSIR_AB <- partialSIR_AB-tmp[i]*c(ab[i], 1)%*%t(c((ab[i]-B)/A^2, 1/A))
  if (itmp == 1) {partialSIR_AB[1, ] <- 0
  partialSIR_AB[, 1] <- 0}
  abMAT <- matrix(rep(ab, each = nis), nrow = nis)
  abMAT <- matx(ab, nis)
  wtMAT <- matx(wt, nis)
  tmp <- (P1-c1)/(1-c1)*(1-P1)*a1
  tmp <- matx(colSums(tmp), nis)*(1-P2)/(1-c2)*wtMAT
  tmp_a2 <- tmp*(P2-c2)*D*(abMAT-b2)
  partialSIR_a2 <- rbind(rowSums(tmp_a2*abMAT), rowSums(tmp_a2))
  tmp_b2 <- tmp*(P2-c2)*D*(-a2)
  partialSIR_b2 <- rbind(rowSums(tmp_b2*abMAT), rowSums(tmp_b2))
  partialSIR_c2 <- rbind(rowSums(tmp*abMAT), rowSums(tmp))
  tmp1 <- -(P1-c1)/(1-c1)*(1-P1)*a1
  tmp2 <- P2-P1
  tmp3 <- (1+c1-2*P1)/(1-c1)*a1
  tmp4 <- D*(abMAT-A*b1-B)/A
  tmp6 <- (P1-c1)/(1-c1)*(1-P1)
  tmp_a1 <- matx(colSums(tmp1), nis)*(tmp4*tmp6)+matx(colSums(tmp2), nis)*(tmp3*tmp4*tmp6)+matx(colSums(tmp2), nis)*(tmp6)
  tmp_a1 <- tmp_a1*wtMAT
  partialSIR_a1 <- rbind(rowSums(tmp_a1*abMAT), rowSums(tmp_a1))
  tmp61 <- -tmp6*D*a1
  tmp_b1 <- matx(colSums(tmp1), nis)*tmp61+matx(colSums(tmp2), nis)*(tmp3*tmp61)
  tmp_b1 <- tmp_b1*wtMAT
  partialSIR_b1 <- rbind(rowSums(tmp_b1*abMAT), rowSums(tmp_b1))
  tmp7 <- (P1-c1)/(1-c1)*a1
  tmp8 <- (1-P1)/(1-c1)
  tmp_c1 <- matx(colSums(tmp1), nis)*tmp8-matx(colSums(tmp2), nis)*(tmp7*tmp8)
  tmp_c1 <- tmp_c1*wtMAT
  partialSIR_c1 <- rbind(rowSums(tmp_c1*abMAT), rowSums(tmp_c1))
  }
  
  if (method == "mean-mean" | method == "mean-sigma" | method == "mean-gmean") { 
  if (itmp == 1) mat <- cbind(
   c(partialA_b1, partialA_b2), 
   c(partialB_b1, partialB_b2))
  if (itmp == 2) mat <- cbind(
   c(partialA_b1, partialA_a1, partialA_b2, partialA_a2), 
   c(partialB_b1, partialB_a1, partialB_b2, partialB_a2))
  if (itmp == 3) mat <- cbind(
   c(partialA_b1, partialA_a1, partialA_c1, partialA_b2, partialA_a2, partialA_c2), 
   c(partialB_b1, partialB_a1, partialB_c1, partialB_b2, partialB_a2, partialB_c2))
  }
  if (method == "Haebara" | method == "Stocking-Lord") {
  if (itmp == 1) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_b2)
  if (itmp == 1) partialSIR_gamma[1, ] <- 0
  if (itmp == 2) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_a1, partialSIR_b2, partialSIR_a2)
  if (itmp == 3) partialSIR_gamma <- cbind(partialSIR_b1, partialSIR_a1, partialSIR_c1, partialSIR_b2, partialSIR_a2, partialSIR_c2)
  if (itmp == 1) {invpartialSIR_AB <- partialSIR_AB
  invpartialSIR_AB[2, 2] <- 1/invpartialSIR_AB[2, 2]}
  if (itmp == 1) mat1 <- -invpartialSIR_AB%*%partialSIR_gamma
  if (itmp>1) mat1 <- -solve(partialSIR_AB)%*%partialSIR_gamma
  mat <- t(mat1)
  }
  if (nis == 0) mat <- matrix(NA, 2, 2)
  colnames(mat) <- c("A", "B")

  rownames(mat) <- c(paste(com.select, suff1, sep = ""), paste(com.select, suff2, sep = ""))
  if (nis>0) {
  var1r <- var1[tab$Row.names[tab$select == 1], tab$Row.names[tab$select == 1]]
  var2r <- var2[tab$Row.names[tab$select == 1], tab$Row.names[tab$select == 1]]
  rownames(var1r) <- colnames(var1r) <- paste(rownames(var1r), suff1, sep = "")
  rownames(var2r) <- colnames(var2r) <- paste(rownames(var2r), suff2, sep = "")
  var12 <- blockdiag(var1r, var2r)
  varAB <- t(mat)%*%var12[rownames(mat), rownames(mat)]%*%mat
  }
  else { 
  varAB <- matrix(NA, 2, 2)
  var12 <- NULL 
  }
 }
 if (is.null(var1) | is.null(var2)) {
  var12 <- NULL
  mat <- NULL
  varAB <- matrix(NA, 2, 2)
 }
 if (!is.null(var12)) {
  rownames(var1) <- colnames(var1) <- paste(rownames(var1), suff1, sep = "")
  rownames(var2) <- colnames(var2) <- paste(rownames(var2), suff2, sep = "")
  varFull <- list(var1, var2)
 }
 else varFull <- NULL
 taball$value12[1:niall] <- A*taball$value1[1:niall]+B
 if (itmp>1) taball$value12[(niall+1):(2*niall)] <- taball$value1[(niall+1):(2*niall)]/A
 if (itmp == 3) taball$value12[(2*niall+1):(3*niall)] <- taball$value1[(2*niall+1):(3*niall)]
 colnames(taball) <- c("Item", name1, name2, paste(name1, name2, sep = ".as."), "select")
 out <- list(tab1 = tab1, tab2 = tab2, tab = taball, var12 = var12, varFull = varFull, partial = mat, A = A, B = B, 
    varAB = varAB, commonitem = list(common = comuni, common.select = com.select), 
    suffixes = c(suff1, suff2), ni = ni, nis = nis)
 
 }
 out$forms <- forms
 out$method <- method
 out$itmp <- itmp
 class(out) <- "eqc"
 return(out)
}




dif.test.compute <- function(DIFtype, alleq, coef_trasf, var_trasf)
{
 ng <- length(alleq) # number groups
 ref <- (1:ng)[sapply(alleq, FUN = function(x) is.null(x))] #reference group
 foc <- (1:ng)[sapply(alleq, FUN = function(x) !is.null(x))] #focal groups
 itmp <- alleq[[foc[1]]]$itmp
 if (itmp == 1 & (DIFtype == "beta2" | DIFtype == "beta3" | DIFtype == "beta12" | DIFtype == "beta123")) stop("Rasch model: set DIFtype = \"beta1\"")
 if (itmp == 2 & (DIFtype == "beta3" | DIFtype == "beta123")) stop("1PL or 2PL model: set DIFtype = \"beta1\" or DIFtype = \"beta12\"")
 comuniall <- lapply(alleq, FUN = function(x) x$commonitem[[1]])
 comuni <- comuniall[[foc[1]]]
 for (i in foc[-1]) comuni <- merge(comuni, comuniall[[i]], by = 1) #common items between all groups
 if (ng>2) comuni <- comuni$x
 itms <- unique(substring(comuni, 8))
 dif_test <- matrix(NA, length(itms), 3)
 i <- 1
 for (it in itms) {
 if (DIFtype == "beta3") {if(var_trasf[paste("c0", it, ref, sep = "."), paste("c0", it, ref, sep = ".")] == 0)	dif_test[i, 3] <- 1}
 if (is.na(dif_test[i, 3])) {
  DIFtypenew <- DIFtype
  if (DIFtype == "beta123") {if(var_trasf[paste("c0", it, ref, sep = "."), paste("c0", it, ref, sep = ".")] == 0)	{DIFtypenew <- "beta12";dif_test[i, 3] <- 1}}
  
  wh <- switch(DIFtypenew, beta2 = "beta2", beta1 = "beta1", beta3 = "c0", beta12 = c("beta1", "beta2"), beta123 = c("c0", "beta1", "beta2"))
  l <- length(wh)
  
  coefi <- sapply(coef_trasf, FUN = function(x, it) x[it, ], it = it)
  if (itmp == 1) {coefi <- matrix(coefi, nrow = 1); rownames(coefi) <- "beta1"}
  coefi <- coefi[, c(ref, foc)]
  if (itmp == 1) {coefi <- matrix(coefi, nrow = 1); rownames(coefi) <- "beta1"}
  coefi <- coefi[wh, ]
  coefi <- as.vector(coefi)
  wh <- paste(wh, it, sep = ".")
  C <- matrix(0, (ng-1)*l, ng*l) #design matrix
  if (ng == 2 & l == 1) C[, -1] <- -1
  else diag(C[, -(1:l)]) <- -1
  if (l>1) for (j in 1:(ng-1)) diag(C[((j-1)*l+1):(j*l), 1:l]) <- 1
  else C[, 1] <- 1
  diffr <- C%*%coefi
  eg <- expand.grid(wh, c(ref, foc), stringsAsFactors = FALSE)
  sel <- rep(NA, nrow(eg))
  for (k in 1:nrow(eg))
  sel[k] <- paste(eg[k, ], collapse = ".")
  vv <- var_trasf[sel, sel]
  vv[upper.tri(vv)] <- t(vv)[upper.tri(vv)]
  chi2stat <- t(diffr)%*%solve(C%*% vv %*%t(C))%*%diffr
  p.value <- pchisq(chi2stat, nrow(diffr), lower.tail = FALSE)
  dif_test[i, 1] <- chi2stat
  dif_test[i, 2] <- p.value
 }
 i <- i+1
 }
 colnames(dif_test) <- c("statistic", "p.value", "noGuess")
 rownames(dif_test) <- itms
 return(dif_test)
}


dif.test <- function(est.mods = NULL, coef = NULL, var = NULL, names = NULL, 
                     reference = NULL, method = "mean-mean", 
                     quadrature = TRUE, nq = 30, DIFtype = NULL, purification = FALSE, 
                     signif.level = 0.05, trace = FALSE, maxiter = 30, anchor = NULL)
{
  if (!is.null(est.mods))
  {
    ltparam <- TRUE
    lparam <- TRUE
    m1<-est.mods[[1]]
    cl<-class(m1)
    if (cl=="ltm")
    {
      imports<-lapply(est.mods,import.ltm,display=FALSE)
      coef<-lapply(imports,function(x) x$coef)
      var<-lapply(imports,function(x) x$var)
    }
    else 
    {
      if (attributes(cl)$package=="mirt")
      {
        imports<-lapply(est.mods,import.mirt,display=FALSE)
        coef<-lapply(imports,function(x) x$coef)
        var<-lapply(imports,function(x) x$var)
      }
      else stop("est.mods should be a list including the models estimated with packages ltm or mirt")
    }
    for (i in 1:length(var)) if(all(is.na(var[[i]]))) stop("Variance matrix contains NAs. \n")
    mods <- modIRT(est.mods, display = FALSE)
  }
  else
  {
    for (i in 1:length(var)) if(all(is.na(var[[i]]))) stop("Variance matrix contains NAs. \n")
    mods <- modIRT(coef = coef, var = var, names = names, display = FALSE)
  }
 itmp <- mods[[1]]$itmp
 
 if (is.null(DIFtype)) DIFtype <- switch(itmp, "1" = "beta1", "2" = "beta12", "3" = "beta123")
 
 if (is.null(reference)) reference <- names(mods)[1]
 if (is.numeric(reference)) reference <- names(mods)[reference]
 ref <- which(names(mods) == reference)
 focal <- names(mods)[names(mods)!= reference]
 if (!is.null(anchor) & purification) {
 purification <- FALSE; 
 cat("Purification set to FALSE because anchor is not NULL. \n")
 }
 alleq <- vector("list", length(mods))
 if (is.null(anchor)) {
 for (i in 1:length(mods)) {
  if (names(mods[i])!= reference) {
  alleq[[i]] <- direc(mods = mods, which = c(i, ref), suff1 = paste(".", i, sep = ""), suff2 = paste(".", ref, sep = ""), method = method, D = 1, quadrature = quadrature, nq = nq)
  }
 }
 }
 else {
 for (i in 1:length(mods)) {
  if (names(mods[i])!= reference) {
  alleq[[i]] <- direc(mods = mods, which = c(i, ref), suff1 = paste(".", i, sep = ""), suff2 = paste(".", ref, sep = ""), method = method, D = 1, quadrature = quadrature, nq = nq, items.select = anchor)
  }
 }
 }
 cvtrasf <- trasf.compute(alleq = alleq, coef = coef, var = var, itmp = itmp)
 coef_trasf <- cvtrasf$coef_trasf
 var_trasf <- cvtrasf$var_trasf
 dif_test_pre <- dif.test.compute(DIFtype = DIFtype, alleq = alleq, coef_trasf = coef_trasf, var_trasf = var_trasf)
 cond <- dif_test_pre[, "p.value"]>signif.level
 itms.ok <- rownames(dif_test_pre)[cond]
 itms.dif <- rownames(dif_test_pre)[!cond]
 if (!is.null(anchor)) itms.ok <- itms.ok[itms.ok%in%anchor]
 if (length(itms.ok) == 0) stop("All items show DIF. Try to reduce the significance level.")
 if (trace) {print(round(dif_test_pre, 3)); cat(itms.dif, "\n")}
 different <- TRUE
 count <- 1
 dif_test_new <- dif_test_pre
 alleqnew <- alleq
 while (different) {
 alleqpre <- alleqnew
 dif_test_pre <- dif_test_new
 alleqnew <- vector("list", length(mods))
 for (i in 1:length(mods)) {
  if (names(mods[i])!= reference) {
  alleqnew[[i]] <- direc(mods = mods, which = c(i, ref), suff1 = paste(".", i, sep = ""), suff2 = paste(".", ref, sep = ""), method = method, D = 1, quadrature = quadrature, nq = nq, items.select = itms.ok)
  }
 }
 
 cvtrasf <- trasf.compute(alleq = alleqnew, coef = coef, var = var, itmp = itmp)
 coef_trasf <- cvtrasf$coef_trasf
 var_trasf <- cvtrasf$var_trasf
 cov_trasf <- cvtrasf$cov_trasf
 dif_test_new <- dif.test.compute(DIFtype = DIFtype, alleq = alleqnew, coef_trasf = coef_trasf, var_trasf = var_trasf)
 
 itms.ok.new <- rownames(dif_test_new)[dif_test_new[, "p.value"]>signif.level]
 itms.dif <- rownames(dif_test_new)[dif_test_new[, "p.value"]<signif.level]
 if (length(itms.ok.new) == 0) stop("All items show DIF. Try to reduce the significance level.")
 if (!is.null(anchor)) itms.ok.new <- itms.ok.new[itms.ok.new%in%anchor]
 if (trace) {print(round(dif_test_new, 3)); cat(itms.dif, "\n")}
 if (identical(itms.ok, itms.ok.new)) different <- FALSE
 if ((count>= maxiter & length(itms.ok.new)>length(itms.ok)) | count == (maxiter+10)) {
  dif_test <- dif_test_pre
  if (trace) cat("stopped at iteration", count, "\n")
  different <- FALSE
 }
 else dif_test <- dif_test_new
 if (!purification) {
  different <- FALSE
  dif_test <- dif_test_pre
 }
 equatings <- alleqnew
 itms.ok <- itms.ok.new
 count <- count+1
 }
 itms.dif <- rownames(dif_test)[dif_test[, "p.value"]<signif.level]
 out <- list(test = dif_test, eqmet = method, DIFtype = DIFtype, reference = reference, focal = focal, 
   names = names(mods), purification = purification, signif.level = signif.level, equatings = equatings, 
   coef_trasf = coef_trasf, var_trasf = var_trasf, items.dif = itms.dif, anchor = anchor, niter = count)
 class(out) <- "dift"
 return(out)
}


# computation of:
# 1. item parameters transformed (beta1* = beta1-beta2*B/A; beta2* = beta2/A)
# 2. variance matrix of item parameters transformed
trasf.compute <- function(alleq, coef, var, itmp){
 ng <- length(alleq) # number groups
 ref <- (1:ng)[sapply(alleq, FUN = function(x) is.null(x))] #reference group
 foc <- (1:ng)[sapply(alleq, FUN = function(x) !is.null(x))] #focal groups

 varref <- var[ref][[1]]
 coefref <- coef[ref][[1]]
 niref <- nrow(coefref)
 itmsnames <- rownames(coefref)
 if (itmp == 1) {
 rownames(varref) <- colnames(varref) <- paste("beta1", itmsnames, ref, sep = ".")
 colnames(coefref)[1] <- "beta1"
 coefref <- subset(coefref, select = "beta1")
 beta1 <- coefref[, 1]
 der_abc_cbeta12_ref <- matrix(0, niref, niref)
 diag(der_abc_cbeta12_ref[1:niref, 1:niref]) <- -1
 colnames(der_abc_cbeta12_ref) <- paste("beta1", itmsnames, ref, sep = ".")
 rownames(der_abc_cbeta12_ref) <- paste("Dffclt", itmsnames, ref, sep = ".")
 }
 if (itmp == 2) {
 rownames(varref) <- colnames(varref) <- c(paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 colnames(coefref) <- c("beta1", "beta2")
 beta1 <- coefref[, 1]
 beta2 <- coefref[, 2]
 der_abc_cbeta12_ref <- matrix(0, 2*niref, 2*niref)
 diag(der_abc_cbeta12_ref[1:niref, 1:niref]) <- -1/beta2
 diag(der_abc_cbeta12_ref[1:niref, (niref+1):(2*niref)]) <- beta1/beta2^2
 diag(der_abc_cbeta12_ref[(niref+1):(2*niref), (niref+1):(2*niref)]) <- 1
 colnames(der_abc_cbeta12_ref) <- c(paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 rownames(der_abc_cbeta12_ref) <- c(paste("Dffclt", itmsnames, ref, sep = "."), paste("Dscrmn", itmsnames, ref, sep = "."))
 }
 if (itmp == 3) {
 rownames(varref) <- colnames(varref) <- c(paste("c0", itmsnames, ref, sep = "."), paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 colnames(coefref) <- c("c0", "beta1", "beta2")
 c0 <- coefref[, 1]
 beta1 <- coefref[, 2]
 beta2 <- coefref[, 3]
 der_abc_cbeta12_ref <- matrix(0, 3*niref, 3*niref)
 diag(der_abc_cbeta12_ref[1:niref, (niref+1):(2*niref)]) <- -1/beta2
 diag(der_abc_cbeta12_ref[1:niref, (2*niref+1):(3*niref)]) <- beta1/beta2^2
 diag(der_abc_cbeta12_ref[(niref+1):(2*niref), (2*niref+1):(3*niref)]) <- 1
 diag(der_abc_cbeta12_ref[(2*niref+1):(3*niref), 1:niref]) <- plogis(c0)*(1-plogis(c0))
 colnames(der_abc_cbeta12_ref) <- c(paste("c0", itmsnames, ref, sep = "."), paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 rownames(der_abc_cbeta12_ref) <- c(paste("Dffclt", itmsnames, ref, sep = "."), paste("Dscrmn", itmsnames, ref, sep = "."), paste("Gussng", itmsnames, ref, sep = "."))
 }
 allitmsnames <- c()
 for (i in 1:ng)
 {
 itmsnames <- rownames(coef[[i]])
 if (itmp == 1) namestmp <- paste("beta1", itmsnames, i, sep = ".")
 if (itmp == 2) namestmp <- c(paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
 if (itmp == 3) namestmp <- c(paste("c0", itmsnames, i, sep = "."), paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
 allitmsnames <- c(allitmsnames, namestmp)
 }
 coef_trasf <- list()
 var_trasf <- matrix(NA, length(allitmsnames), length(allitmsnames))
 rownames(var_trasf) <- colnames(var_trasf) <- allitmsnames
 for (i in foc) {
 eqi <- alleq[[i]]
 mat <- eqi$partial
 A <- eqi$A
 B <- eqi$B
 coefi <- coef[[i]]
 vari <- var[[i]]
 ni <- nrow(coefi)
 itmsnames <- rownames(coefi)
 
 if (itmp == 1) {
  beta1 <- coefi[, 1]
  beta1t = beta1-B
  coefit <- matrix(beta1t)
  rownames(coefit) <- itmsnames
  colnames(coefit) <- "beta1"
  coef_trasf[[i]] <- coefit
  
  rownames(vari) <- colnames(vari) <- paste("beta1", itmsnames, i, sep = ".")
  
  der_abc_cbeta12_foc <- matrix(0, ni, ni)
  diag(der_abc_cbeta12_foc[1:ni, 1:ni]) <- -1
  colnames(der_abc_cbeta12_foc) <- paste("beta1", itmsnames, i, sep = ".")
  rownames(der_abc_cbeta12_foc) <- paste("Dffclt", itmsnames, i, sep = ".")
  der_abc_cbeta12 <- blockdiag(der_abc_cbeta12_foc, der_abc_cbeta12_ref)
  if (nrow(mat)<nrow(der_abc_cbeta12)) {
  mat1 <- matrix(0, nrow(der_abc_cbeta12), 2)
  rownames(mat1) <- rownames(der_abc_cbeta12)
  mat1[rownames(mat), ] <- mat
  mat <- mat1
  }
  der_AB_cbeta12 <- t(mat[rownames(der_abc_cbeta12), 2])%*%der_abc_cbeta12
  
  der1 <- cbind(diag(1, ni), -1)
  rownames(der1) <- paste("beta1", itmsnames, i, sep = ".")
  der2 <- rbind(cbind(diag(1, ni), matrix(0, niref, niref)), der_AB_cbeta12)
 }
 if (itmp == 2) {
  beta1 <- coefi[, 1]
  beta2 <- coefi[, 2]
  beta1t = beta1-beta2*B/A
  beta2t = beta2/A
  coefit <- cbind(beta1t, beta2t)
  rownames(coefit) <- itmsnames
  colnames(coefit) <- c("beta1", "beta2")
  coef_trasf[[i]] <- coefit
  
  rownames(vari) <- colnames(vari) <- c(paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  
  der_abc_cbeta12_foc <- matrix(0, 2*ni, 2*ni)
  diag(der_abc_cbeta12_foc[1:ni, 1:ni]) <- -1/beta2
  diag(der_abc_cbeta12_foc[1:ni, (ni+1):(2*ni)]) <- beta1/beta2^2
  diag(der_abc_cbeta12_foc[(ni+1):(2*ni), (ni+1):(2*ni)]) <- 1
  colnames(der_abc_cbeta12_foc) <- c(paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  rownames(der_abc_cbeta12_foc) <- c(paste("Dffclt", itmsnames, i, sep = "."), paste("Dscrmn", itmsnames, i, sep = "."))
  der_abc_cbeta12 <- blockdiag(der_abc_cbeta12_foc, der_abc_cbeta12_ref)
  if (nrow(mat)<nrow(der_abc_cbeta12)) {
  mat1 <- matrix(0, nrow(der_abc_cbeta12), 2)
  rownames(mat1) <- rownames(der_abc_cbeta12)
  mat1[rownames(mat), ] <- mat
  mat <- mat1
  }
  der_AB_cbeta12 <- t(mat[rownames(der_abc_cbeta12), ])%*%der_abc_cbeta12
  
  der1 <- rbind(cbind(diag(1, ni), diag(-B/A, ni), (beta2*B/A^2), (-beta2/A)), 
     cbind(matrix(0, ni, ni), diag(1/A, ni), (-beta2/A^2), rep(0, ni)))
  rownames(der1) <- c(paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  der2 <- rbind(cbind(diag(1, 2*ni), matrix(0, 2*ni, 2*niref)), der_AB_cbeta12)
 }
 if (itmp == 3) {
  c0 <- coefi[, 1]
  beta1 <- coefi[, 2]
  beta2 <- coefi[, 3]
  beta1t = beta1-beta2*B/A
  beta2t = beta2/A
  coefit <- cbind(c0, beta1t, beta2t)
  rownames(coefit) <- itmsnames
  colnames(coefit) <- c("c0", "beta1", "beta2")
  coef_trasf[[i]] <- coefit
  
  rownames(vari) <- colnames(vari) <- c(paste("c0", itmsnames, i, sep = "."), paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  
  der_abc_cbeta12_foc <- matrix(0, 3*ni, 3*ni)
  diag(der_abc_cbeta12_foc[1:ni, (ni+1):(2*ni)]) <- -1/beta2
  diag(der_abc_cbeta12_foc[1:ni, (2*ni+1):(3*ni)]) <- beta1/beta2^2
  diag(der_abc_cbeta12_foc[(ni+1):(2*ni), (2*ni+1):(3*ni)]) <- 1
  diag(der_abc_cbeta12_foc[(2*ni+1):(3*ni), 1:ni]) <- plogis(c0)*(1-plogis(c0))
  colnames(der_abc_cbeta12_foc) <- c(paste("c0", itmsnames, i, sep = "."), paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  rownames(der_abc_cbeta12_foc) <- c(paste("Dffclt", itmsnames, i, sep = "."), paste("Dscrmn", itmsnames, i, sep = "."), paste("Gussng", itmsnames, i, sep = "."))
  der_abc_cbeta12 <- blockdiag(der_abc_cbeta12_foc, der_abc_cbeta12_ref)
  if (nrow(mat)<nrow(der_abc_cbeta12)) {
  mat1 <- matrix(0, nrow(der_abc_cbeta12), 2)
  rownames(mat1) <- rownames(der_abc_cbeta12)
  mat1[rownames(mat), ] <- mat
  mat <- mat1
  }
  der_AB_cbeta12 <- t(mat[rownames(der_abc_cbeta12), ])%*%der_abc_cbeta12
  
  der1 <- rbind(cbind(diag(1, ni), matrix(0, ni, 2*ni+2)), 
     cbind(matrix(0, ni, ni), diag(1, ni), diag(-B/A, ni), (beta2*B/A^2), (-beta2/A)), 
     cbind(matrix(0, ni, 2*ni), diag(1/A, ni), (-beta2/A^2), rep(0, ni)))
  rownames(der1) <- c(paste("c0", itmsnames, i, sep = "."), paste("beta1", itmsnames, i, sep = "."), paste("beta2", itmsnames, i, sep = "."))
  der2 <- rbind(cbind(diag(1, 3*ni), matrix(0, 3*ni, 3*niref)), der_AB_cbeta12)
 }
 der <- der1%*%der2
 var_foc_ref <- blockdiag(vari, varref)[colnames(der), colnames(der)]
 var_trasf_foc <- der%*%var_foc_ref%*%t(der)
 var_trasf[rownames(var_trasf_foc), colnames(var_trasf_foc)] <- var_trasf_foc
 der_ref_foc <- der[, rownames(varref)]
 var_trasf_focref <- der_ref_foc%*%varref #covariance of item parameters transformed of focal group and item parameters of reference group 
 var_trasf[rownames(var_trasf_focref), colnames(var_trasf_focref)] <- var_trasf_focref
 if (itmp == 1) selref <- paste("beta1", itmsnames, ref, sep = ".")
 if (itmp == 2) selref <- c(paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 if (itmp == 3) selref <- c(paste("c0", itmsnames, ref, sep = "."), paste("beta1", itmsnames, ref, sep = "."), paste("beta2", itmsnames, ref, sep = "."))
 for (k in foc) {
  if (k<i){
  if (itmp == 1) selfoc <- paste("beta1", itmsnames, k, sep = ".")
  if (itmp == 2) selfoc <- c(paste("beta1", itmsnames, k, sep = "."), paste("beta2", itmsnames, k, sep = "."))
  if (itmp == 3) selfoc <- c(paste("c0", itmsnames, k, sep = "."), paste("beta1", itmsnames, k, sep = "."), paste("beta2", itmsnames, k, sep = "."))
  var_trasf[rownames(der_ref_foc), selfoc] <- der_ref_foc%*%t(var_trasf[selfoc, selref])
  }
 }
 }
 var_trasf[rownames(varref), colnames(varref)] <- varref
 coef_trasf[[ref]] <- coefref
 names(coef_trasf) <- names(coef)
 return(list(coef_trasf = coef_trasf, var_trasf = var_trasf))
}
