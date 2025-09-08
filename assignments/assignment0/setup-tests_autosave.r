# APEC 8221 - Setup Verification Script
# This4 script checks that your R/RStudio environment is properly configured
# for the course. Run this entire script for Assignment 0.

# ===================================================================
#   STUDENT: PLEASE EDIT THIS LINE
# ===================================================================
x500_id <- "yang9291" # <-- Replace with your UMN x500 ID (e.g., "gemini001")
# ===================================================================

# Function to run all tests and collect results
run_setup_tests <- function() {
  results <- list()
  
  # Section 1: Capture User Info
  git_user_name <- tryCatch(gert::git_config_global()$value[gert::git_config_global()$name == "user.name"], error = function(e) NA)
  results$user_info <- list(
    test = "User Information",
    x500 = x500_id,
    name = if (is.na(git_user_name) || git_user_name == "") "Not Configured" else git_user_name
  )

  # Section 2: Capture Diagnostic Info
  rstudio_version_string <- if (exists(".rs.api.versionInfo")) .rs.api.versionInfo()$version else "Not running in RStudio"
  results$diag_info <- list(
    r_version_string = R.version.string,
    os = utils::sessionInfo()$running,
    rstudio_version = rstudio_version_string,
    timestamp = format(Sys.time())
  )

  # Section 3: Perform Pass/Fail Tests
  # Test 1: R Version
  r_version_numeric <- tryCatch({
    as.numeric(paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = "."))
  }, error = function(e) { NA })
  
  r_version_ok <- !is.na(r_version_numeric) && r_version_numeric >= 4.4
  results$r_version <- list(
    test = "R Version", pass = r_version_ok,
    message = if (r_version_ok) paste(R.version.string, "is sufficient (≥ 4.4.0)") else paste(R.version.string, "is too old - please update")
  )
  
  # Test 2: RStudio Version
  rstudio_version_ok <- package_version(rstudio_version_string) >= package_version("2023.06.0")
  results$rstudio_version <- list(
    test = "RStudio Version", pass = rstudio_version_ok,
    message = if (rstudio_version_ok) paste("RStudio version", rstudio_version_string, "is sufficient (≥ 2023.06.0)") else paste("RStudio version", rstudio_version_string, "is too old - please update")
  )
  
  # Test 3: Core Packages
  req_pkgs <- c("tidyverse", "quarto", "usethis", "knitr", "rmarkdown", "tinytex", "gert")
  installed_status <- sapply(req_pkgs, requireNamespace, quietly = TRUE)
  pkgs_ok <- all(installed_status)
  results$packages <- list(
    test = "Core Packages", pass = pkgs_ok,
    message = if (pkgs_ok) "All required packages are installed" else paste("Missing packages:", paste(names(installed_status[!installed_status]), collapse = ", "))
  )
  
  # Test 4: TinyTeX
  tinytex_ok <- tinytex::is_tinytex()
  results$tinytex <- list(
    test = "TinyTeX (for PDF)", pass = tinytex_ok,
    message = if (tinytex_ok) "TinyTeX is installed" else "TinyTeX installation not found"
  )
  
  # Test 5: Git Installation
  git_path <- tryCatch({ gert::git_find() }, error = function(e) { NULL })
  git_ok <- !is.null(git_path)
  results$git_install <- list(
    test = "Git Installed", pass = git_ok,
    message = if (git_ok) paste("Git executable found by 'gert' package") else "Git executable not found by the 'gert' package"
  )
  
  # Test 6: Git Configuration
  git_config_ok <- !is.na(git_user_name) && git_user_name != ""
  results$git_config <- list(
    test = "Git User Configured", pass = git_config_ok,
    message = if (git_config_ok) paste("Git user.name is set to:", git_user_name) else "Git user.name is not configured globally"
  )

  # Test 7: Tidyverse Functionality
  tidyverse_ok <- tryCatch({
    suppressPackageStartupMessages(require(tidyverse))
    df <- mtcars %>% group_by(cyl) %>% summarize(n = n())
    is.data.frame(df) && nrow(df) == 3
  }, error = function(e) { FALSE })
  results$tidyverse <- list(
    test = "Tidyverse Function", pass = tidyverse_ok,
    message = if (tidyverse_ok) "dplyr pipe and summarize functions work" else "Error running a basic tidyverse command"
  )

  # Test 8: ggplot2 Plotting
  ggplot_ok <- tryCatch({
    p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
    inherits(p, "ggplot")
  }, error = function(e) { FALSE })
  results$ggplot <- list(
    test = "ggplot2 Plotting", pass = ggplot_ok,
    message = if (ggplot_ok) "ggplot object created successfully" else "Error creating a basic ggplot"
  )

  # Test 9: File System Access
  fs_ok <- tryCatch({
    test_file <- "test_fs.tmp"
    cat("hello world", file = test_file)
    content <- readLines(test_file, warn = FALSE)
    unlink(test_file) # Clean up the test file
    content == "hello world"
  }, error = function(e) { FALSE })
  results$filesystem <- list(
    test = "File System Write", pass = fs_ok,
    message = if (fs_ok) "Can write and read a file successfully" else "Error writing or reading a file in the current directory"
  )
  
  return(results)
}

# Function to format and display results
display_results <- function(results, all_tests_passed) {
  cat("=========================================\n")
  cat("   APEC 8221 SETUP VERIFICATION REPORT\n")
  cat("=========================================\n\n")

  # Print Diagnostic Information
  diag_info <- results$diag_info
  cat("DIAGNOSTIC INFORMATION:\n")
  cat("-----------------------\n")
  cat(sprintf("R version: %s\n", diag_info$r_version_string))
  cat(sprintf("OS:        %s\n", diag_info$os))
  cat(sprintf("RStudio:   %s\n", diag_info$rstudio_version))
  cat(sprintf("Date/time: %s\n\n", diag_info$timestamp))

  # Print User Information
  user_info <- results$user_info
  cat("STUDENT INFORMATION:\n")
  cat("--------------------\n")
  cat(sprintf("Student Name: %s\n", user_info$name))
  cat(sprintf("x500 ID:      %s\n\n", user_info$x500))
  
  cat("TEST RESULTS:\n")
  cat("--------------------\n")
  # Display test results
  for (test_name in names(results)) {
    if (test_name %in% c("user_info", "diag_info")) next 
    
    res <- results[[test_name]]
    status <- if (res$pass) "✅ PASS" else "❌ FAIL"
    cat(sprintf("%-20s | %-6s | %s\n", res$test, status, res$message))
  }
  
  # Conditionally display the "NEXT STEPS" section only on failure
  if (!all_tests_passed) {
    cat("\n=========================================\n")
    cat("   NEXT STEPS: HOW TO FIX FAILURES\n")
    cat("=========================================\n\n")

    for (test_name in names(results)) {
      if (test_name %in% c("user_info", "diag_info") || isTRUE(results[[test_name]]$pass)) next
      
      res <- results[[test_name]]
      cat(sprintf("For '%s' failure:\n", res$test))

      if (test_name == "packages") {
        req_pkgs <- c("tidyverse", "quarto", "usethis", "knitr", "rmarkdown", "tinytex", "gert")
        installed_status <- sapply(req_pkgs, requireNamespace, quietly = TRUE)
        missing_pkgs <- names(installed_status[!installed_status])
        cat("  → Install missing packages by running this command in the console:\n")
        cat(paste0("    install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))\n"))
      } else if (test_name == "tinytex") {
        cat("  → Run this command in the console: tinytex::install_tinytex()\n")
      } else if (test_name == "git_install") {
        cat("  → Download and install Git from: https://git-scm.com/downloads\n")
        cat("  → After installing, completely CLOSE and REOPEN RStudio.\n")
      } else if (test_name == "git_config") {
        cat("  → Your Git identity is not set. Run these two commands in the RStudio Terminal tab:\n")
        cat('    git config --global user.name "Your Full Name"\n')
        cat('    git config --global user.email "your.email@example.com"\n')
      } else {
        cat("  → Re-install the core packages with: install.packages('tidyverse')\n")
      }
      cat("\n")
    }
  }
}

# ===================================================================
#   MAIN SCRIPT BODY: RUNS TESTS AND SAVES OUTPUT
# ===================================================================

# --- Configuration for output file ---
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  output_file <- file.path(script_dir, "assignment0-setup.txt")
} else {
  output_file <- "assignment0-setup.txt"
}

# --- Run tests and determine overall status ---
message("Running setup verification tests...")
results <- run_setup_tests()
pass_values <- sapply(results, `[[`, "pass")
all_tests_passed <- all(unlist(pass_values))

# --- Logic to save output to file ---
action_taken <- "None"

if (!file.exists(output_file)) {
  sink(output_file)
  cat("=========================================\n")
  cat("--- FIRST ATTEMPT ---\n")
  cat("=========================================\n")
  display_results(results, all_tests_passed)
  sink()
  action_taken <- paste("Created", output_file, "with the first run's results.")
} else {
  log_content <- readLines(output_file)
  success_already_logged <- any(grepl("FINAL SUCCESSFUL RUN", log_content))
  
  if (all_tests_passed && !success_already_logged) {
    sink(output_file, append = TRUE)
    cat("\n\n=========================================\n")
    cat("--- FINAL SUCCESSFUL RUN ---\n")
    cat("=========================================\n")
    display_results(results, all_tests_passed)
    sink()
    action_taken <- paste("Appended the final successful results to", output_file)
  }
}

# --- Final message to the user in the console ---
display_results(results, all_tests_passed)

cat("-----------------------------------------\n")
cat("LOG FILE STATUS:\n")
if (action_taken != "None") {
  message("✅ ", action_taken)
} else {
  if (all_tests_passed) {
    message("ℹ️ A successful run is already logged. No changes were made to the file.")
  } else {
    message("ℹ️ Tests still failing. Fix the issues and re-run. The log file will not be updated until all tests pass.")
  }
}
cat("-----------------------------------------\n")