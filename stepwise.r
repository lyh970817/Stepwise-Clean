source("./stepwise_fun.r")
load("~/Downloads/stepwise (1).RData")

# Read googlesheet
sheet <- stepwise_sheet("Stepwise_variables")

# Run test
test_nonames(sheet, stepwise)

# Clean data

# Recode and applying limits
stepwise_clean <- stepwise_recode_df(stepwise, sheet)

# Which variables are factors?
which_factor <- which(map_lgl(stepwise_clean, is.factor))

# Create numeric version variable names
numeric_names <- paste0(names(which_factor), ".numeric")

# Assign numeric version variables
stepwise_clean[numeric_names] <- map_df(stepwise_clean[which_factor], as.numeric)

# Export data
save(stepwise_clean, file = "./stepwise_clean.Rdata")

