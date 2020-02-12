require(googlesheets4)
require(tidyverse)
# For date processing
require(lubridate)

stepwise_url <- "https://docs.google.com/spreadsheets/d/1ygdc0t3BPl0436YDS0Fqs3z5ucposKSzE9YjYY4YNIk/edit#gid=356553963"

stepwise_sheet <- function(sheetname) {
  googlesheet <- read_sheet(stepwise_url,
    sheet = sheetname,
    # Read everything as character. Remember to convert to numeric later
    col_types = "c"
  )
  return(googlesheet)
}

test_nonames <- function(sheet, data) {
  # Test for the number of corresponding names

  # get value positions in "variable.swedish" that are in the colnames
  # of the data frame and that have corresponding "variable.english"
  eng_avai_pos <- (sheet[["oldvar"]] %in% colnames(data)) & !is.na(sheet[["newvar"]])

  # Take the whole columns of "variable.swedish" and "variable.english"
  # based on defined positions
  swe_name <- sheet[["oldvar"]][eng_avai_pos]
  eng_name <- sheet[["newvar"]][eng_avai_pos]

  # For each "variable.swedish", find how many corresponding
  # "variable.english" there are.
  no_names <- c()
  for (i in seq_along(unique(swe_name))) {
    each_name <- unique(swe_name)[i]
    no_names[i] <- length(unique(eng_name[swe_name == each_name]))
  }
  # If any "variable.swedish" have more than one corresponding
  # "variable.english", print.
  if (length(unique(no_names)) != 1) {
    uniq_repname <- unique(swe_name)[which(no_names != 1)]
    corres_names <- eng_name[swe_name == uniq_repname]
    print(paste("Swedish name is :", uniq_repname))
    print(paste("English name is :", corres_names))
  }

  # For each "variable.english", find how many corresponding
  # "variable.swedish" there are.
  no_names <- c()
  for (i in seq_along(unique(eng_name))) {
    each_name <- unique(eng_name)[i]
    no_names[i] <- length(unique(swe_name[eng_name == each_name]))
  }

  # If any "variable.english" have more than one corresponding
  # "variable.swedish", print.
  if (length(unique(no_names)) != 1) {
    uniq_repname <- unique(eng_name)[which(no_names != 1)]
    corres_names <- swe_name[eng_name == uniq_repname]
    print(paste("English name is :", uniq_repname))
    print(paste("Swedish name is :", corres_names))
  }
}

sheet_extract <- function(col, var, googlesheet) {
  # Extract values for a specified variable from a specified column
  return(googlesheet[[col]][googlesheet[["oldvar"]] == var &
    !is.na(googlesheet[["oldvar"]])])
  # Careful that `!is.na` is needed for the `==` not to evaluate to
  # `NA`, which is to be treated as subsetting indices and could lead to
  # unexpected behaviour? (that I'm actually not very sure of).
}

stepwise_recode <- function(x, var, googlesheet) {
  if (all(is.na(sheet_extract("oldvar", var, googlesheet)))) {
    warning(paste(var, "is not in the dictionary."))
    return(x)
  }

  type <- sheet_extract("type", var, googlesheet) %>%
    unique()

  if (length(type) > 1) {
    stop(paste(var, "has more than one type :", type, "\n"))
  }

  # Similar reason as above - `NA` might be problematic in conditional
  # evaluation.

  # notin <- colnames(stepwise)[which(!colnames(stepwise) %in% sheet[["oldvar"]])]
  # if (var %in% notin) {
  #   type <- "NA"
  # }

  if (is.na(type)) {
    warning(paste(var, "has no type info."))
    type <- "NA"
  }

  if (type == "Categorical") {
    levels <- sheet_extract("values", var, googlesheet)
    # Some "values" in the googlesheet are left blank and will be
    # coded to NA by R. Here it's to code a character string "NA" to
    # make assigning factor levels and labels easier.
    levels[is.na(levels)] <- "NA"
    labels <- sheet_extract("labels", var, googlesheet)

    if (all(is.na(levels))) {
      levels <- levels[1]
      warning(paste(var, "has no values."))
    }

    if (length(unique(levels)) != length(levels)) {
      stop(paste(var, "does not have distinct levels.\n"))
    }
    # There are expectedly corresponding empty strings in the data frame
    # to "values" in the dictionary. These are also coded to a string
    # "NA" to ensure we have a correct map between data values and
    # levels.

    x[x == ""] <- "NA"

    # Implement a check to see if there are levels in the data but no in
    # dict
    if (all(is.na(labels))) {
      x <- tryCatch(
        expr = {
          factor(x, levels = levels)
        },
        error = function(e) {
          msg <- paste0(
            "Error occurs at ", var,
            ", with levels: ", levels, "\n"
          )
          print(msg)
          stop(x)
        }
      )
    } else {
      x <- tryCatch(
        expr = {
          factor(x, levels = levels, labels = labels)
        },
        error = function(e) {
          msg <- paste0(
            "Error occurs at ", var,
            ", with levels: ", paste(levels, collapse = ", "),
            ", and labels: ", paste(labels, collapse = ", "), "\n"
          )
          stop(msg)
        }
      )
    }

    x[x == "NA"] <- NA
    x <- factor(x, levels = levels, labels = labels)
  } else if (type == "Numeric/Continuous") {

    # If there are no "Min" or "Max" in the dictionary
    # R will throw an error. We catch it and use the min and max computed
    # from the data instead (so essentially we will not discard anything
    # if the limits are from the data itself).
    min <- tryCatch(
      expr = {
        unique(as.numeric(sheet_extract("min", var, googlesheet)))
      },
      error = function(e) {
        min(x)
      }
    )
    max <- tryCatch(
      expr = {
        unique(as.numeric(sheet_extract("max", var, googlesheet)))
      },
      error = function(e) {
        max(x)
      }
    )
    if (!is.numeric(min)) {
      stop(paste("Minimum for", var, min, "is not numeric."))
    }
    if (!is.numeric(max)) {
      stop(paste("Maximum for", var, max, "is not numeric."))
    }
    x[(x < min | x > max)] <- NA
  } else if (type == "Date") {
    x <- as_date(x)
  }
  return(x)
}

stepwise_rename <- function(data, googlesheet) {
  swe_name <- googlesheet[["oldvar"]]
  eng_name <- googlesheet[["newvar"]]
  item_pos <- (swe_name %in% colnames(data)) & (!is.na(eng_name))

  oldnames <- swe_name[item_pos] %>% unique()
  newnames <- eng_name[item_pos] %>% unique()
  names(newnames) <- oldnames
  return(plyr::rename(data, newnames))
}

stepwise_recode_df <- function(data, googlesheet) {
  data_cleaned <- data %>%
    # Be careful that the order of applying `stepwise_reaname`
    # and `stepwise_recode` is important. Because function
    # `sheet_extract` used in `stepwise_recode` extract googlesheet
    # columns based only on either `variable.swedish` or
    # `variable.english` which can be changed.
    imap_dfc(stepwise_recode, googlesheet = googlesheet) %>%
    stepwise_rename(googlesheet = googlesheet)
  return(data_cleaned)
}
