library(readr)
library(tidyverse) #for visualitazion
library(fpp2) #for forecasting

#load data set
dt <- read_delim("C:/BPS/01 Fungsi Statistik/2024 - Indathon/Indathon Round 1/training_jumlah_penumpang_tj.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dt)

#menambahkan variabel baru "tanggal"
dt <- 
  dt %>%
  mutate(
    tanggal = 1
  )

dt$periode <- as.Date(with(dt,paste(tahun,bulan,tanggal,sep="-")),"%Y-%m-%d")
dt

dt1 <- select(dt, periode, jumlah_penumpang)
dt1

colnames(dt1) <- c("ds", "y")
dt1

#eksplorasi data untuk mengetahui tipe dan struktur variabel
glimpse(dt1)

#visualisasi data
ggplot(dt1, aes(x = ds, y = y)) +
  geom_point(size = 2) + #ukuran point
  geom_path(size = 1) + #ukuran line
  labs(title="Grafik Jumlah Penumpang TJ Periode 2015-2023",
       x="Periode",y="Jumlah Penumpang")


#membuat data jadi berbentuk series
data_penumpang <- ts(dt1[2], frequency = 12, start = c(2015))
data_penumpang

#melakukan forecast
forecast1 <- ets(data_penumpang, model = "ANA")
data_penumpang_forecast <- forecast(forecast1, h=6)
data_penumpang_forecast
plot(data_penumpang_forecast)
