library(data.table)
library(httr)
library(XML)

setwd('~/R/EDGAR')

# should load the file only if it is necessary - i.e. it is not already in memory
filings <- readRDS('filings.rds')

getPrimaryDocUrl <- function(original_file_name) {
  baseurl <- 'https://www.sec.gov/Archives/'
  original_file_name <- gsub('-', '', original_file_name)
  primaryDocUrl <- gsub('.txt', '/primary_doc.xml', original_file_name)
  primaryDocUrl <- paste(baseurl, primaryDocUrl, sep = '')
  return(primaryDocUrl)
}

checkForZeroStrings <- function(inString) {
  if(is.null(inString) | length(inString) == 0) {
    return('')
  }
  return(inString)
}

getFilingDetail <- function(form_d_file) {
  form_d_file <- getPrimaryDocUrl(form_d_file)
  zz <- xmlParse(rawToChar(GET(form_d_file)$content))
  ns <- getNodeSet(zz, "/")
  testOrLive <- xpathApply(ns[[1]], "//testOrLive", xmlValue, recursive = FALSE)
  issuerName <- xpathApply(ns[[1]], "//primaryIssuer/entityName", xmlValue, recursive = TRUE)
  cik <- xpathApply(ns[[1]], "//primaryIssuer/cik", xmlValue, recursive = TRUE)
  issuerState <- xpathApply(ns[[1]], "//primaryIssuer/issuerAddress/stateOrCountry", xmlValue, recursive = TRUE)
  issuerZip <- xpathApply(ns[[1]], "//primaryIssuer/issuerAddress/zipCode", xmlValue, recursive = TRUE)
  issuerEntityType <- xpathApply(ns[[1]], "//primaryIssuer/entityType", xmlValue, recursive = TRUE)
  issuerEntityTypeOtherDesc <- xpathApply(ns[[1]], "//primaryIssuer/entityTypeOtherDesc", xmlValue, recursive = TRUE)
  industryGroup <- xpathApply(ns[[1]], "//offeringData/industryGroup", xmlValue, recursive = FALSE)
  industryGroupType <- xpathApply(ns[[1]], "//offeringData/industryGroup/industryGroupType", xmlValue, recursive = TRUE)
  investmentFundType <- xpathApply(ns[[1]], "//offeringData/industryGroup/investmentFundInfo/investmentFundType", xmlValue, recursive = TRUE)
  revenueRange <- xpathApply(ns[[1]], "//offeringData/issuerSize/revenueRange", xmlValue, recursive = FALSE)    
  navRange <- xpathApply(ns[[1]], "//offeringData/issuerSize/aggregateNetAssetValueRange", xmlValue, recursive = FALSE)    
  federalExemptionsExclusions <- xpathApply(ns[[1]], "//offeringData/federalExemptionsExclusions/item", xmlValue, recursive = TRUE)
  isAmendment <- xpathApply(ns[[1]], "//offeringData/typeOfFiling/newOrAmendment/isAmendment", xmlValue, recursive = FALSE)
  previousAccessionNumber <- xpathApply(ns[[1]], "//offeringData/typeOfFiling/newOrAmendment/previousAccessionNumber", xmlValue, recursive = FALSE)
  dateOfFirstSale <- xpathApply(ns[[1]], "//offeringData/typeOfFiling/dateOfFirstSale/value", xmlValue, recursive = FALSE)
  durationOfOfferingMoreThanOneYear <- xpathApply(ns[[1]], "//offeringData/durationOfOffering/moreThanOneYear", xmlValue, recursive = FALSE)
  isEquityType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isEquityType", xmlValue, recursive = FALSE)
  isDebtType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isDebtType", xmlValue, recursive = FALSE)
  isOptionToAcquireType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isOptionToAcquireType", xmlValue, recursive = FALSE)
  isOtherType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isOtherType", xmlValue, recursive = FALSE)
  isSecurityToBeAcquiredType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isSecurityToBeAcquiredType", xmlValue, recursive = FALSE)
  descriptionOfOtherType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/descriptionOfOtherType", xmlValue, recursive = FALSE)
  isPooledInvestmentFundType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isPooledInvestmentFundType", xmlValue, recursive = FALSE)
  isTenantInCommonType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isTenantInCommonType", xmlValue, recursive = FALSE)
  isMineralPropertyType <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isMineralPropertyType", xmlValue, recursive = FALSE)
  isBusinessCombinationTransaction <- xpathApply(ns[[1]], "//offeringData/businessCombinationTransaction/isBusinessCombinationTransaction", xmlValue, recursive = FALSE)
  minimumInvestmentAccepted <- xpathApply(ns[[1]], "//offeringData/minimumInvestmentAccepted", xmlValue, recursive = FALSE)
  recipientName  <- xpathApply(ns[[1]], "//offeringData/salesCompensationList/recipient/recipientName", xmlValue, recursive = FALSE)
  recipientCRDNumber  <- xpathApply(ns[[1]], "//offeringData/salesCompensationList/recipient/recipientCRDNumber", xmlValue, recursive = FALSE)
  associatedBDName  <- xpathApply(ns[[1]], "//offeringData/salesCompensationList/recipient/associatedBDName", xmlValue, recursive = FALSE)
  associatedBDCRDNumber  <- xpathApply(ns[[1]], "//offeringData/salesCompensationList/recipient/associatedBDCRDNumber", xmlValue, recursive = FALSE)
  salesCommission  <- xpathApply(ns[[1]], "//offeringData/salesCommissionsFindersFees/salesCommission", xmlValue, recursive = FALSE)
  findersFees  <- xpathApply(ns[[1]], "//offeringData/salesCommissionsFindersFees/findersFees", xmlValue, recursive = FALSE)
  totalOfferingAmount <- xpathApply(ns[[1]], "//offeringData/offeringSalesAmounts/totalOfferingAmount", xmlValue, recursive = FALSE)
  totalAmountSold <- xpathApply(ns[[1]], "//offeringData/offeringSalesAmounts/totalAmountSold", xmlValue, recursive = FALSE)
  typesOfSecuritiesOfferedOtherDescription <- xpathApply(ns[[1]], "//offeringData/typesOfSecuritiesOffered/isOtherType/descriptionOfOtherType", xmlValue, recursive = FALSE)
  x = data.table(
    originalFileName = form_d_file,
    testOrLive = checkForZeroStrings(unlist(testOrLive)),
    issuerName = checkForZeroStrings(unlist(issuerName)),
    cik = checkForZeroStrings(unlist(cik)),
    issuerState = checkForZeroStrings(unlist(issuerState)),
    issuerZip = checkForZeroStrings(unlist(issuerZip)),
    issuerEntityType = checkForZeroStrings(unlist(issuerEntityType)),
    issuerEntityTypeOtherDesc = checkForZeroStrings(unlist(issuerEntityTypeOtherDesc)),
    industryGroup = checkForZeroStrings(unlist(industryGroup)),
    industryGroupType = checkForZeroStrings(unlist(industryGroupType)),
    investmentFundType = checkForZeroStrings(unlist(investmentFundType)),
    revenueRange = checkForZeroStrings(unlist(revenueRange)),
    navRange = checkForZeroStrings(unlist(navRange)),
    federalExemptions = checkForZeroStrings(paste(unlist(federalExemptionsExclusions), collapse = ' ')),
    isAmendment = checkForZeroStrings(unlist(isAmendment)),
    previousAccessionNumber = checkForZeroStrings(unlist(previousAccessionNumber)),
    dateOfFirstSale = checkForZeroStrings(unlist(dateOfFirstSale)),
    durationOfOfferingMoreThanOneYear = checkForZeroStrings(unlist(durationOfOfferingMoreThanOneYear)),
    isEquityType = checkForZeroStrings(unlist(isEquityType)),
    isDebtType = checkForZeroStrings(unlist(isDebtType)),
    isOptionToAcquireType = checkForZeroStrings(unlist(isOptionToAcquireType)),
    isSecurityToBeAcquiredType = checkForZeroStrings(unlist(isSecurityToBeAcquiredType)),
    isOtherType = checkForZeroStrings(unlist(isOtherType)),
    descriptionOfOtherType = checkForZeroStrings(unlist(descriptionOfOtherType)),
    isPooledInvestmentFundType = checkForZeroStrings(unlist(isPooledInvestmentFundType)),
    isTenantInCommonType = checkForZeroStrings(unlist(isTenantInCommonType)),
    isMineralPropertyType = checkForZeroStrings(unlist(isMineralPropertyType)),
    isBusinessCombinationTransaction = checkForZeroStrings(unlist(isBusinessCombinationTransaction)),
    minimumInvestmentAccepted = checkForZeroStrings(unlist(minimumInvestmentAccepted)),
    recipientName = checkForZeroStrings(paste(unlist(recipientName), collapse = '|')),
    recipientCRDNumber = checkForZeroStrings(paste(unlist(recipientCRDNumber), collapse = '|')),
    associatedBDName = checkForZeroStrings(paste(unlist(associatedBDName), collapse = '|')),
    associatedBDCRDNumber = checkForZeroStrings(paste(unlist(associatedBDCRDNumber), collapse = '|')),
    salesCommission = checkForZeroStrings(unlist(salesCommission)),
    findersFees = checkForZeroStrings(unlist(findersFees)),
    totalOfferingAmount = checkForZeroStrings(unlist(totalOfferingAmount)),
    totalAmountSold = checkForZeroStrings(unlist(totalAmountSold)),
    typesOfSecuritiesOfferedOtherDescription = checkForZeroStrings(unlist(typesOfSecuritiesOfferedOtherDescription))
  )
  return(x)
}

detail_table <- data.table(originalFileName = character(),
                           testOrLive = character(),
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
# remove the filings table from memory
rm(filings)

# first line below is commented as an example of how to use with a smaller dataset than the entire one
# for(row in 1:nrow(head(reg_d_filings, 100))) {
for(row in 1:nrow(reg_d_filings)) {
    print(reg_d_filings[row,]$V4) # the date of the file being processed
    detail_table <- rbind(detail_table, getFilingDetail(reg_d_filings[row,]$V5))
}

# save the reg_d_filings table to an RDS file
saveRDS(detail_table, 'detail_table.rds')
