# PROGRAM visualisasi-Covid19.R

# DEKLARASI - pustaka
library(readr) # membaca file excel/csv/txt/json
library(readxl) # membaca file excel/csv/txt/json
library(dplyr) # memanipulasi data
library(ggpubr) # menghitung uji korelasi
library(ggcorrplot) # visualisasi uji korelasi
library(ggplot2) # membuat diagram
library(plotly) # memperindah diagram

# ALGORITMA
# -input data
df <- read_excel("D:/Bahan Ajar Big Data/dataset/dataset_covid.xlsx", sheet = "data covid indonesia")

# - inputan dataset dilakukan modifikasi tipe data
# karena tanggal dibaca string
df <- data.frame(
  tanggal = as.Date(df$tanggal),
  positif_kumulatif = as.numeric(df$positif_kumulatif),
  sembuh_kumulatif = as.numeric(df$sembuh_kumulatif),
  meninggal_kumulatif = as.numeric(df$meninggal_kumulatif),
  positif_harian = as.numeric(df$positif_harian),
  sembuh_harian = as.numeric(df$sembuh_harian),
  meninggal_harian = as.numeric(df$meninggal_harian),
  perawatan_kumulatif = as.numeric(df$perawatan_kumulatif),
  perawatan_harian = as.numeric(df$perawatan_harian),  
  persentase_sembuh = as.numeric(df$persentase_sembuh),
  persentase_meninggal = as.numeric(df$persentase_meninggal)
)

# Menampilkan isi data
head(df)

# Menampilkan struktur dataset
str(df)

# Menampilkan ringkasan statistik dataset
summary(df)

# ------------------------------------------------------------------------------
# Proses 1 (Statistik Deskriptif Sederhana)
# Cara manual. Analogi menggunakan bahasa C
# int i, j, hasil, jumlah_positif
# for(i=0; i<baris; i++){
#   for(j=0; j<kolom; j++){
#     hasil = hasil + jumlah_positif[i][j]
#   }  
# }
# Menghitung jumlah positif, sembuh dan meninggal di Indonesia
# Cara otomatis meninggunakan fungsi sum()
print(jumlah_positif <- sum(df$positif_harian))
print(jumlah_sembuh <- sum(df$sembuh_harian))
print(jumlah_meninggal <- sum(df$meninggal_harian))

# Menghitung persentase sembuh dan meninggal
print((jumlah_sembuh/jumlah_positif)*100)
print((jumlah_meninggal/jumlah_positif)*100)

# ------------------------------------------------------------------------------
# proses 2 (Uji korelasi)
# Menghitung korelasi dan signifikansi positif dengan sembuh dan positif dengan meninggal jika dilihat dari pergerakan data secara time series.
# Terdapat beberapa metode korelasi seperti Uji-Kontingensi, Uji-Phi, Uji-Spearman Rank, Uji-Kendall, Uji-Pearson.
# 1. Uji-Kontingensi adalah korelasi dengna skala nominal dan bersifat non-parametrik.
# 2. Uji-Phi adalah korelasi dengna skala nominal dan bersifat non-parametrik. Korelasi ini umumnya untuk data-data dikotomik.
# 3. Uji-Spearman Rank adalah korelasi dengan skala ordinal dan bersifat non-parametrik.
# 4. Uji-Kendall adalah korelasi dengan skala ordinal dan bersifat non-parametrik.
# 5. Uji-Pearson adalah korelasi dengan skala interval dan bersifat parametrik. Dimana datanya akan scontinue
# Karena data covid berskala interval dan berisifat parametrik sehingga metode yang cocok adalah uji-pearson.

# - uji korelasi positif harian dengan sembuh harian
cor(df$positif_harian, df$sembuh_harian, method="pearson")
ggscatter(
  # menentukan df, var bebas dan terikat
  df, x="positif_harian", y="sembuh_harian",
  
  # menghitung korelasi dan p-value dengan tingkat kepercayaan 95%
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  
  # membuat label-label
  title="Visual Scatter Plot Menggunakan Uji-Pearson", xlab="Positif harian", ylab="Sembuh harian",
  
  color='green', size = 3
)
# hasil uji korelasi
# r = 0.9495273, p-value = 2.2e-16
# makanya ......?

# - uji korelasi positif harian dengan meninggal harian
cor(df$positif_harian, df$meninggal_harian, method="pearson")
ggscatter(
  df, x="positif_harian", y="meninggal_harian",
  cor.coef=TRUE, conf.int.level = 0.95, cor.method="pearson",
  title="Visual Scatter Plot Menggunakan Uji-Pearson", xlab="Positif harian", ylab="Meninggal harian",
  color='red', size = 3
)

# ggcorrplot untuk visualisasi uji-korelasi
# - langkah 1 pilih variabel
df_corr <- data.frame(
  positif_harian = df$positif_harian,
  sembuh_harian = df$sembuh_harian,
  meninggal_harian = df$meninggal_harian
)

# - langkah 2 hitung korelasi
df_corr <- round(cor(df_corr, method="pearson"),2)
df_corr

# - langkah 3 visualisasi ggcorrplot
ggcorrplot(df_corr, hc.order =FALSE, lab =TRUE, outline.color ="white")
