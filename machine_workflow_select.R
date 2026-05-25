https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### Select data
### 1 Read dataset

# Loading necessary libraries
library(tidyverse)
library(httr)

# Downloading and reading data
url <- "http://userpage.fu-berlin.de/soga/soga-py/300/307000_time_series/tageswerte_KL_00403_19500101_20211231_hist.zip"
temp_zip <- tempfile()
download.file(url, temp_zip)
unzipped_file <- unzip(temp_zip, files = "produkt_klima_tag_19500101_20211231_00403.txt", exdir = tempdir())
data_raw <- read.table(unzipped_file, sep = ";", header = TRUE, na.strings = "-999")
head(data_raw)

### 2 Remove unnecessary columns

data <- subset(data_raw, select = -c(STATIONS_ID, MESS_DATUM, QN_3, FX, FM, NM, QN_4, eor))
head(data)

### 3 Rename columns for convenience

data <- rename(data,
  prec = RSK,
  prec_type = RSKF,
  sun_dur = SDK,
  snow_depth = SHK_TAG,
  vapor_pres = VPM,
  pres = PM,
  temp = TMK,
  rel_humid = UPM,
  temp_max = TXK,
  temp_min = TNK,
  temp_sfc = TGK
)
head(data)


### 4 Remove correlated columns
library(ggplot2)
library(GGally)

p <- ggpairs(data, columns = c("temp", "temp_max", "temp_min", "temp_sfc"))
p

data <- subset(data, select = -c(temp_max, temp_min, temp_sfc))
head(data)


### 5 Drop missing data

missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values


data <- drop_na(data)
head(data)

### Handle categorical data

data$prec_type <- as.factor(data$prec_type)
data <- model.matrix(~ . + 0, data)
data <- as.data.frame(data)
head(data)



