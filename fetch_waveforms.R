get_patient_numerics <- function(file_str) {
  raw_table <- system(paste0("rdsamp -r mimic3wdb/matched/", file_str," -v -p -c"), intern = TRUE) %>%
    str_replace_all("-", "")
  
  units_strs <- str_split(raw_table[2], ",") %>% unlist %>% str_replace_all("'", "") %>% paste(sep = "(")
  col_names <- str_split(raw_table[1], ",") %>% unlist %>% str_replace_all("'", "") %>% 
    paste(units_strs, sep = "(") %>% paste(")", sep = "") %>% str_replace_all("[() % ?]", "_") %>% str_replace_all("___", "")
  
  raw_table %>% paste(collapse = "\n") %>% read_csv(skip = 2, col_names = col_names)
}

waveform_meta <- tibble(patient_str = readLines("RECORDS-numerics.txt"))

s <- sample(patient_str, size = 1);print(s)
get_patient_numerics(s) %>%
  select(Elapsed_time_seconds_,contains("mean"), contains("hr"), contains("resp")) %>%
  gather(vital, value,-Elapsed_time_seconds_) %>%
  ggplot(aes(Elapsed_time_seconds_, value, group = vital, color = vital)) + geom_line() + geom_point()

system.time(l0 <- mclapply(patient_str[1:1000], get_patient_numerics, mc.cores = 4))
system.time(l1000 <- mclapply(patient_str[1001:2000], get_patient_numerics, mc.cores = 4))
system.time(l2000 <- mclapply(patient_str[2001:3000], get_patient_numerics, mc.cores = 4))
system.time(l3000 <- mclapply(patient_str[3001:4000], get_patient_numerics, mc.cores = 4))
system.time(l4000 <- mclapply(patient_str[4001:5158], get_patient_numerics, mc.cores = 4))


l[[1]] %>% select(Elapsed_time_seconds_, contains("mean"), contains("hr"), "resp")) %>%
  gather(vital, value,-Elapsed_time_seconds_) %>%
  ggplot(aes(Elapsed_time_seconds_, value, group = vital, color = vital)) + geom_line() + geom_point() + xlim(390, 1000)

l[[s25332 <- get_patient_numerics("s25332/s25332-2152-04-03-11-39n")
s25332 %>% select(Elapsed_time_seconds_, HR_bpm_, contains("mean")) %>%
  gather(vital, value,-Elapsed_time_seconds_) %>%
  ggplot(aes(Elapsed_time_seconds_, value, group = vital, color = vital)) + geom_line() + geom_point() + xlim(390, 1000)
