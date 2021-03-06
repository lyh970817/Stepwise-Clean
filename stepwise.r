source("./stepwise_fun.r")
source("./stepwise_continuous.r")
source("./stepwise_density.r")
source("./stepwise_discrete.r")
load("./stepwise.RData")

stepwise <- read_sav("~/Downloads/Trauma3_Main_Data_anonymized.sav")

# Need to run the below line to avoid errors
stepwise <- stepwise %>% select(-c("kön", "ålder1", "ed1"))

# Read googlesheet
sheet <- stepwise_sheet("Stepwise_variables") %>%
  rename(
    oldvar = variable.swedish,
    newvar = variable.english,
    title = description,
    values = values,
    labels = labels.english,
    min = Min,
    max = Max,
    unit = Unit,
    type = Type
  )

# Test if there is a one-to-one mapping between `newvar` and `oldvar`
test_nonames(sheet, stepwise)

# Clean data

# Recode and applying limits
stepwise_clean <- stepwise_recode_df(stepwise, sheet)

# Which variables are numeric factors?

# These would have been recoded to factors in `stepwise_clean` because in
# the googlesheet they have labels of "Categorical" or "Binary". However,
# they would have numeric values (instead of character strings which would
# also have "Categorical" or "Binary" labels in googlesheet) in the raw
# data file.

which_numeric_factor <- which(map2_lgl(
  stepwise_clean, stepwise,
  function(clean, raw) {
    is.factor(clean) & is.numeric(raw)
  }
))

# Create numeric version variable names
numeric_names <- paste0(names(which_numeric_factor), ".numeric")

# Assign numeric version variables
stepwise_clean[numeric_names] <- stepwise[which_numeric_factor]

# Plots
continous_point_plot(stepwise_clean, "SASB3.self.love.t5", googlesheet = sheet, include_outlier = FALSE)
density_plot(stepwise_clean, "SASB3.self.love.t5", googlesheet = sheet, include_outlier = TRUE)
factor_plot(stepwise_clean, "confirmed.diagnosis.t5", googlesheet = sheet)

# For gender-split plots, need to specify which column stores gender info.
hist_count_sex(stepwise_clean, "SASB3.self.love.t5", sex = "sex", googlesheet = sheet, include_outlier = FALSE)
density_plot_bysex(stepwise_clean, "SASB3.self.love.t5", sex = "sex", googlesheet = sheet, include_outlier = FALSE)
sex_plot(stepwise_clean, sex = "sex")

# Export data
save(stepwise_clean, file = "./stepwise_clean.Rdata")
