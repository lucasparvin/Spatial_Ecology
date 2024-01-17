lock_folder <- "C:/Users/lkp0038/AppData/Local/R/win-library/4.3/00LOCK"

# Check if the folder exists
if (file.exists(lock_folder)) {
  # Delete the folder
  unlink(lock_folder, recursive = TRUE)
  cat("The lock folder has been deleted.\n")
} else {
  cat("The lock folder does not exist.\n")
}
