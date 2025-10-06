# Enhanced R Package Installation Script for LightSpan Cyepi Analysis
# Updated: June 26, 2025

cat("=== LightSpan Cyepi Analysis - R Package Installation ===\n")
cat("Installing required R packages...\n\n")

# Read packages from requirements/r.txt file
if (!file.exists("requirements/r.txt")) {
  stop("Error: requirements/r.txt file not found!")
}

lines <- readLines("requirements/r.txt", warn = FALSE)

# Extract package names (remove comments and empty lines)
packages <- lines[!grepl("^#", lines) & 
                 !grepl("^\\s*$", lines) & 
                 !grepl("^=", lines)]

# Clean package names and remove inline comments
packages <- trimws(gsub("#.*$", "", packages))
packages <- packages[nzchar(packages)]

# Print packages to be installed
cat("Packages to install:\n")
for (i in seq_along(packages)) {
  cat(sprintf("%2d. %s\n", i, packages[i]))
}
cat("\n")

# Check which packages are already installed
installed_packages <- installed.packages()[, "Package"]
missing_packages <- packages[!packages %in% installed_packages]

if (length(missing_packages) == 0) {
  cat("✓ All packages are already installed!\n")
} else {
  cat("Installing", length(missing_packages), "missing packages...\n")
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n\n")
  
  # Install missing packages with better error handling
  for (pkg in missing_packages) {
    cat("Installing", pkg, "...\n")
    tryCatch({
      install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org/")
      cat("✓", pkg, "installed successfully\n")
    }, error = function(e) {
      cat("✗ Failed to install", pkg, ":", e$message, "\n")
    })
  }
}

# Setup IRkernel for Jupyter if available
if("IRkernel" %in% packages) {
  cat("\nSetting up IRkernel for Jupyter...\n")
  tryCatch({
    library(IRkernel)
    IRkernel::installspec()
    cat("✓ IRkernel setup complete\n")
  }, error = function(e) {
    cat("✗ IRkernel setup failed:", e$message, "\n")
  })
}

# Verify installation
cat("\n=== Installation Verification ===\n")
final_check <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(final_check) == 0) {
  cat("✓ All packages successfully installed!\n")
} else {
  cat("✗ The following packages failed to install:", paste(final_check, collapse = ", "), "\n")
  cat("Please try installing them manually with install.packages()\n")
}

cat("\n=== Installation Complete ===\n")
cat("You can now proceed with the LightSpan Cyepi Analysis workflow.\n")