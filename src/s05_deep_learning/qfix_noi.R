# This script applies quick fix: no intensity for L2 normalization

source('config/setup.R')
library(tidyverse)

# 递归查找d04和d05下所有带l2的rds文件
dirs <- c('data/d04_split', 'data/d05_dl_ready')
rds_files <- unlist(lapply(dirs, function(d) list.files(d, pattern = 'l2.*\\.rds$', full.names = TRUE, recursive = TRUE)))

for (f in rds_files) {
  dat <- readRDS(f)
  if ("Intensity" %in% colnames(dat)) {
    dat <- dat %>% select(-Intensity)
    # 构造新文件名：在l2后加_noi
    new_f <- sub('(l2)', '\\1_noi', f)
    saveRDS(dat, new_f)
    cat('Saved:', new_f, '\n')
  }
}

