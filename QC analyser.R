library(tidyverse)
library(janitor)
library(clipr)
library(lubridate)
library(readxl)



# qc_data data.frame aşağıdaki sütünları içermeli

# qc_result_date: QC çalışma tarihi
# control_name: Kontrol adı
# control_type: Kontrol düzeyi 
# lot_number: Lot numarası 
# device_name: Cihaz
# device_sub_name: Varsa alt cihaz
# test_name: test adı
# calibration: kalibrasyon yapıldı ZORUNLU DEĞİL NA olabilir
# calibration_date: kalibrasyon tarihi ZORUNLU DEĞİL NA olabilir
# result: kontrol sonucu
# lot_start_date: lot başlama tarihi
# mean_defined: üretici ortalama
# sd_defined : üretici sd


qc_summary <- qc_data %>%
  group_by(device_sub_name, test_name, lot_number, control_type) %>%
  summarise(n = n(), mean = mean(result), sd = sd(result)) %>%
  mutate(cv = 100 * sd / mean) %>%
  mutate(low_2_SD = mean - 2 * sd, high_2_SD = mean + 2 * sd) %>%
  mutate(low_3_SD = mean - 3 * sd, high_3_SD = mean + 3 * sd) %>%
  ungroup() 


last_20 <- qc_data %>%
  group_by(device_sub_name, test_name, lot_number, control_type) %>% 
  arrange(desc(qc_result_date)) %>% 
  mutate(id = row_number()) %>% 
  filter(id < 21 ) %>%
  summarise(last_n = n(), last_mean = mean(result), last_sd = sd(result)) %>%
  mutate(last_cv = 100 * last_sd / last_mean) %>%
  mutate(last_low_2_SD = last_mean - 2 * last_sd, last_high_2_SD = last_mean + 2 * last_sd) %>%
  mutate(last_low_3_SD = last_mean - 3 * last_sd, last_high_3_SD = last_mean + 3 * last_sd) %>%
  ungroup() 



all_calculated_QC <- left_join(qc_summary, last_20) %>% 
  select(device_sub_name:cv, last_n, last_mean:last_cv) %>% 
  mutate(change =  abs(last_mean - mean) > 0.75*sd)


calibration_count <- qc_data %>% 
  group_by(ay = lubridate::month(qc_result_date), device_name, test_name) %>% 
  summarise(cal_say = sum(calibration)) %>% 
  arrange(desc(cal_say))
  
  
  
  
