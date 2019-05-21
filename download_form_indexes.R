setwd('~/R/EDGAR')

# download all FORM indexes from EDGAR
yrs <- 1993:2019
qtrs <- 1:4
baseurl <- 'https://www.sec.gov/Archives/edgar/full-index'

for(yr in yrs) {
    for(qtr in qtrs) {
        remote_file <- paste(baseurl, toString(yr), paste('QTR', qtr, sep = ''), 'form.idx', sep= '/')
        local_file <- paste('./downloaded/', toString(yr), paste('QTR', qtr, sep = ''), 'form.txt', sep= '')
        download.file(remote_file, local_file)
    }
}
