library(data.table)

setwd('~/R/EDGAR')

trim.trailing <- function (x) sub("\\s+$", "", x)

# set up the empty file
filings <- data.table(V1 = character(),
                      V2 = character(), 
                      V3 = numeric(), 
                      V4 = as.Date(x = integer(0), origin = '1970-01-01'), 
                      V5 = character())

# read the index file
form_files <- list.files('./downloaded', full.names = TRUE)

for(current_file in form_files) {
    x <- read.fwf(current_file, skip = 10, widths = c(12, 61, 13, 12, 48), comment.char = '', stringsAsFactors = FALSE, fileEncoding = 'latin1')
    x$V4 <- as.Date(x$V4)
    x[,c(1,2,5)] <- lapply(x[,c(1,2,5)], trim.trailing)
    filings <- rbind(filings, data.table(x))
}

# save the filings data.table to an rds file - compresses down nicely!
# this can be read to a data.table later a la:
# filings <- readRDS('filings.rds')
saveRDS(filings, 'filings.rds')
