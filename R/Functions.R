#######################################
# Files to eventually be made into a
# package.
#######################################

# Degrees Included in the Review Table
  # Level (BS, MS, etc.)
  # Program Name
  # Emphases/Tracks (if applicable)
  # Mode of Delivery (Location)
  # URL - Catalog


# Program Metrics Table
  # Data for last 5 Academic Years
  # Rows for
    #1st Year Retention Rates
    ## of Degrees Awarded
    #4 Yr. Graduation Rates
    #6 Yr. Graduation Rates

# Matriculation Data Table
  # Data for last 5 Falls
  # Columns for Applied Admitted Enrolled Percent Enrolled

# Institutional Support Table (by Degree)
  # Academic Year, # Assisted, % Assisted, Average Amount


createQMD_parameterList_for_APR <- function(theDept, degreesIncluded, table1){

  # Helper function to standardize filtering, NA replacement, and conversion
  clean_dept_df <- function(df, filter_faculty = TRUE, filter_year = TRUE) {
    if (is.null(df)) return(data.frame())

    out <- as.data.frame(df)

    if (filter_year && "calYear" %in% names(out)) {
      out <- out[as.character(out$calYear) == target_year, , drop = FALSE]
    }

    if (filter_faculty && "Faculty.ID" %in% names(out)) {
      out <- out[as.character(out$Faculty.ID) == target_id, , drop = FALSE]
    }

    # Replace NAs cleanly depending on type
    out %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ tidyr::replace_na(.x, "-9999"))) %>%
      dplyr::mutate(dplyr::across(where(is.numeric),   ~ tidyr::replace_na(.x, -9999)))
  }

  # Build Parameter Output List
  theOutput <- list(
    theDept                     = if (is.function(theDept)) theDept() else theDept,
    theDegrees                  = if (is.function(degreesIncluded)) degreesIncluded() else degreesIncluded,
    table1Data                  = if (is.function(table1)) table1() else table1
  )

  return(theOutput)
}

readMetricsLocal <- function(theDept){
  # reads the data from a locally produced excel file
  req(theDept)
  inFileStem <- "Academic_Program_Metrics_"
  inFileName <- paste0(inFileStem, theDept, ".xlsx")
  theData <- readxl::read_excel(inFileName) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), -9999, .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "-9999", .))) %>%
    data.frame()

  return(theData)
}

createTable1 <- function(inData, inDept, inProgram){


  useData <- inData %>%
    data.frame() %>%
    filter(Department==inDept) %>%
    filter(shortProgram==inProgram) %>%
    select(-c("Department", "shortProgram"))

  localProgramName <- unique(useData$Program.Name)
  theTable <- useData %>%
    select(-"Program.Name") %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Academic Program Metrics:", localProgramName)
    ) %>%
    gt::sub_values(
      values = c(-9999, "-9999"),
      replacement = ""
    )
  return(theTable)

}

createTable1_kable <- function(inData, inDept, inProgram) {
  if (is.list(inData) && !is.data.frame(inData)) {
    inData <- dplyr::bind_rows(inData)
  }

  useData <- inData %>%
    dplyr::filter(Department == inDept, shortProgram == inProgram) %>%
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(. == -9999 | . == "-9999", "", .))) %>%
    dplyr::select(
      Metric,
      `AY 20-21` = AY.20.21,
      `AY 21-22` = AY.21.22,
      `AY 22-23` = AY.22.23,
      `AY 23-24` = AY.23.24,
      `AY 24-25` = AY.24.25
    )

  # Generate kable object
  tbl <- knitr::kable(
    useData,
    format = "markdown",
    align = c("l", "c", "c", "c", "c", "c"),
    caption = paste("Academic Program Metrics:", inProgram, "-", inDept)
  )

  # Ensure output is flattened into a single character string
  return(paste(as.character(tbl), collapse = "\n"))
}

create5yrTrendRetentionChart <- function(inData, inDept, inPrograms) {
  req(inData, inDept, inPrograms)

  # Filter data for retention rates across selected programs
  plotData <- inData %>%
    data.frame() %>%
    dplyr::filter(Department == inDept, shortProgram %in% inPrograms) %>%
    dplyr::filter(Metric == "1st Year Retention Rates (%)") %>%
    dplyr::select(shortProgram, dplyr::starts_with("AY")) %>%
    # COERCE ALL AY COLUMNS TO CHARACTER TO PREVENT PIVOT TYPE CONFLICTS
    dplyr::mutate(dplyr::across(dplyr::starts_with("AY"), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("AY"),
      names_to = "AcademicYear",
      values_to = "RetentionRate"
    ) %>%
    dplyr::mutate(
      RetentionRate = as.numeric(RetentionRate),
      AcademicYear = stringr::str_replace(AcademicYear, "AY.", "AY ")
    ) %>%
    dplyr::filter(!is.na(RetentionRate), RetentionRate != -9999)

  # Build ggplot trend chart
  ggplot2::ggplot(plotData, ggplot2::aes(x = AcademicYear, y = RetentionRate, color = shortProgram, group = shortProgram)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(y = "Retention (%)", color = "Program")
}

create5yrDegreesAwardedChart <- function(inData, inDept, inPrograms) {
  req(inData, inDept, inPrograms)

  # Filter and reshape data for degrees awarded
  plotData <- inData %>%
    data.frame() %>%
    dplyr::filter(Department == inDept, shortProgram %in% inPrograms) %>%
    dplyr::filter(Metric == "# of Degrees Awarded") %>%
    dplyr::select(shortProgram, dplyr::starts_with("AY")) %>%
    # Coerce all AY columns to character to prevent pivot type conflicts
    dplyr::mutate(dplyr::across(dplyr::starts_with("AY"), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("AY"),
      names_to = "AcademicYear",
      values_to = "DegreesAwarded"
    ) %>%
    dplyr::mutate(
      DegreesAwarded = as.numeric(DegreesAwarded),
      AcademicYear = stringr::str_replace_all(AcademicYear, "^AY\\.|\\.", " ")
    ) %>%
    dplyr::filter(!is.na(DegreesAwarded), DegreesAwarded != -9999)

  # Generate grouped bar chart
  ggplot2::ggplot(plotData, ggplot2::aes(x = AcademicYear, y = DegreesAwarded, fill = shortProgram)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_y_continuous(breaks = function(x) seq(0, ceiling(max(x, na.rm = TRUE)), by = 1)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(y = "Degrees Conferred", fill = "Program")
}


