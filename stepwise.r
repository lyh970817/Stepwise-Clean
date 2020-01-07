source("./stepwise_fun.r")
load("~/Downloads/stepwise (1).RData")

# Read googlesheet
sheet <- stepwise_sheet("Stepwise_variables")

sheet[["variable.swedish"]][sheet[["variable.english"]] == "weight.t1"]

sheet_extract("Min", "vikt1", sheet)
# Run test
test_nonames(sheet, stepwise)

# Clean data
stepwise_clean <- stepwise_recode_df(stepwise, sheet)

range(stepwise_clean[["weight.t1"]])

# Export data
save(stepwise_clean, file = "./stepwise_clean.Rdata")

