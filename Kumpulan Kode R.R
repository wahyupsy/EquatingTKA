








library(data.table)

# Baca ALL items (anchor + non-anchor)
par24_all <- fread("C:/OneDrive/Bahan Analisis/Tabel Parameter/SMPNum_24 item_equated.csv")
par26_all <- fread("C:/OneDrive/Bahan Analisis/Tabel Parameter/SMPNum_26 item_unequated.csv")

# Anchor items dari pvalue_df
anchor_entries <- c(2, 5, 11, 40, 46, 48, 52, 67, 91, 97, 110, 112, 119, 123, 134, 148, 149, 150, 154, 169, 189, 202, 207, 215)

# Pisahkan anchor vs non-anchor untuk form 24
par24_anchor <- par24_all[par24_all$ENTRY %in% anchor_entries, ]
par24_nonanchor <- par24_all[!par24_all$ENTRY %in% anchor_entries, ]

# Untuk form 26, identifikasi anchor (ENTRY yang sama dengan form 24)
anchor_in_26 <- par26_all[par26_all$ENTRY %in% anchor_entries, ]
nonanchor_in_26 <- par26_all[!par26_all$ENTRY %in% anchor_entries, ]

# Bandingkan distribusi MEASURE
cat("=== FORM 24 ===\n")
cat("Anchor items (n=", nrow(par24_anchor), "): mean MEASURE =", mean(par24_anchor$MEASURE, na.rm=T), "\n")
cat("Non-anchor items (n=", nrow(par24_nonanchor), "): mean MEASURE =", mean(par24_nonanchor$MEASURE, na.rm=T), "\n\n")

cat("=== FORM 26 ===\n")
cat("Anchor items (n=", nrow(anchor_in_26), "): mean MEASURE =", mean(anchor_in_26$MEASURE, na.rm=T), "\n")
cat("Non-anchor items (n=", nrow(nonanchor_in_26), "): mean MEASURE =", mean(nonanchor_in_26$MEASURE, na.rm=T), "\n")

# Visualisasi
ggplot() +
  geom_density(data=par24_all, aes(x=MEASURE, fill="Form 24 (semua)"), alpha=0.4, color=NA) +
  geom_density(data=par26_all, aes(x=MEASURE, fill="Form 26 (semua)"), alpha=0.4, color=NA) +
  geom_vline(xintercept=mean(par24_all$MEASURE, na.rm=T), linetype="dashed", color="blue") +
  geom_vline(xintercept=mean(par26_all$MEASURE, na.rm=T), linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Distribusi Difficulty Semua Item: Form 24 vs Form 26 (Unequated)",
       x="MEASURE (Difficulty)", y="Density") +
  theme(legend.position="top")