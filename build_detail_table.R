library(data.table)

setwd('~/R/EDGAR')

filings <- readRDS('filings.rds')

detail_table <- data.table(originalFileName = character(),
                           issuerName = character(),
                           cik = character(),
                           issuerState = character(),
                           issuerZip = character(),
                           issuerEntityType = character(),
                           issuerEntityTypeOtherDesc = character(),
                           industryGroup = character(),
                           industryGroupType = character(),
                           investmentFundType = character(),
                           revenueRange = character(),
                           navRange = character(),
                           federalExemptions = character(),
                           isAmendment = character(),
                           previousAccessionNumber = character(),
                           dateOfFirstSale = character(),
                           durationOfOfferingMoreThanOneYear = character(),
                           isEquityType = character(),
                           isDebtType = character(),
                           isOptionToAcquireType = character(),
                           isSecurityToBeAcquiredType = character(),
                           isOtherType = character(),
                           descriptionOfOtherType = character(),
                           isPooledInvestmentFundType = character(),
                           isTenantInCommonType = character(),
                           isMineralPropertyType = character(),
                           isBusinessCombinationTransaction = character(),
                           minimumInvestmentAccepted = character(),
                           recipientName = character(),
                           recipientCRDNumber = character(),
                           associatedBDName = character(),
                           associatedBDCRDNumber = character(),
                           salesCommission = character(),
                           findersFees = character(),
                           totalOfferingAmount = character(),
                           totalAmountSold = character(),
                           typesOfSecuritiesOfferedOtherDescription = character()
)

# subset the reg D and A docs
reg_d_filings <- filings[filings$V1 %in% c('D', 'D/A'),]

for(row in head(reg_d_filings, 1000)$V5) {
    detail_table <- rbind(detail_table, getFilingDetail(row))
}