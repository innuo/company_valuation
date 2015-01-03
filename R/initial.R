read.data <- function(){
  cols <- c("Company.Name",
            "Firm.Value..in.US...",
            "Industry.Group",
            "Revenues",
            "Pre.tax.Operating.Margin",
            "Effective.Tax.Rate",
            "EBITDA",
            "Historical.growth.in.Revenues...Last.5.years",
            "Expected.growth.in.revenues...Next.2.years",
            "Coeff.of.variation...Op.Income",
            "X..held.by.institutions",
            "Total.Debt.incl.leases..in.US...",
            "Cash",
            "Invested.Capital...4.qtre.ago",
            "Net.Cap.Ex",
            "Reinvestment.Rate",
            "Dividends...Buybacks")
  
  df <- read.csv("../../..//Downloads/DamodaranNumerified copy.csv", stringsAsFactors=F)
  df$Coeff.of.variation...Op.Income <- as.numeric(df$Coeff.of.variation...Op.Income)
  
  test.ids <- rep(FALSE, nrow(df))
  test.ids[sample(1:nrow(df), 1000, replace =FALSE)] <- TRUE
  
  df.train <-subset(df, !test.ids, select=cols)
  saveRDS(list(ids = df.train$Company.Name, 
               data=df.train[, 3:(ncol(df.train)-1)], 
               target= df.train[[2]]), "data/company_train_data.Rdata")
  
  df.test <-subset(df, test.ids, select=cols)  
  saveRDS(list(ids = df.test$Company.Name, 
               data=df.test[, 3:(ncol(df.train)-1)], 
               target= df.test[[2]]), "data/company_test_data.Rdata")
}