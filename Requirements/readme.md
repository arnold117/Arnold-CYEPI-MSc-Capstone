# 📦 Package Requirements & Installation Guide

This directory contains the package requirements and installation instructions for the LightSpan Cyepi Analysis project.

## 📁 File Structure

```
requirements/
├── readme.md           # This file - installation guide
├── r.txt              # R package requirements list
├── install_require.R        # R script for installing required packages
└── python.txt         # Python package requirements list
```

## 🔧 Installation Instructions

### Prerequisites
- **JDK or OpenJDK** (Java Development Kit)
- **R** (version 4.0 or higher)
- **Rtools** (for Windows users)
- **Python** (version 3.8 or higher)

---
## ☕ Setting up JDK Environment

Before installing packages, ensure your JDK is properly configured:

### Windows:
Please use the following commands in Powershell to set up your JDK environment:
```powershell
# Set environment variables
$env:JAVA_HOME = "C:\Program Files\Java\jdk-11.0.x"  # Adjust path as necessary
$env:CLASSPATH = "$env:JAVA_HOME\lib\*;."
$env:PATH = "$env:JAVA_HOME\bin;$env:PATH"

# Verify installation
java -version
javac -version
```

For permanent setup, you need to add these to your system environment variables with the following commands:
```powershell
[Environment]::SetEnvironmentVariable("JAVA_HOME", "C:\Program Files\Java\jdk-11.0.x", "Machine")
[Environment]::SetEnvironmentVariable("CLASSPATH", ".;$env:JAVA_HOME\lib\dt.jar;$env:JAVA_HOME\lib\tools.jar", "Machine")
[Environment]::SetEnvironmentVariable("PATH", "$env:JAVA_HOME\bin;$env:PATH", "Machine")
```

Or manually set them in the System Properties:
1. Right-click on "This PC" or "Computer" and select "Properties".
2. Click on "Advanced system settings".
3. Click on "Environment Variables".
4. Under "System variables", click "New" or "Edit" to add or modify the following variables:
- `JAVA_HOME`: the path to your JDK installation (e.g., `C:\Program Files\Java\jdk-11.0.x`)
- `CLASSPATH`: `.;%JAVA_HOME%\lib\dt.jar;%JAVA_HOME%\lib\tools.jar`
- `PATH`: Add `%JAVA_HOME%\bin`

### macOS:
```bash
# Install via Homebrew
brew install openjdk@11

# Set JAVA_HOME in ~/.zshrc or ~/.bash_profile
export JAVA_HOME=$(/usr/libexec/java_home -v 11)
export PATH=$JAVA_HOME/bin:$PATH

# Reload shell configuration
source ~/.zshrc  # or source ~/.bash_profile
```

### Linux (Ubuntu/Debian):
```bash
# Install OpenJDK
sudo apt update
sudo apt install openjdk-11-jdk
# find the installed JDK path
java_home=$(readlink -f /usr/bin/java | sed "s:bin/java::")

# Set JAVA_HOME in ~/.bashrc
echo "export JAVA_HOME=$java_home" >> ~/.bashrc
echo 'export PATH=$JAVA_HOME/bin:$PATH' >> ~/.bashrc

# Reload bash configuration
source ~/.bashrc
```

---
## 🅿️ Python Package Installation

### Method 1: Using pip (Recommended)

```bash
# Install packages from requirements file
pip install -r python.txt
```

### Method 2: Manual Installation

Copy each package from `python.txt` and install them individually with `pip install package_name`.

---

## 🅡 R Package Installation

### Method 1: Using Installation Script (Recommended)

```r
# In R console or RStudio
source("Requirements/install_require.R")
```

If you prefer to run the script directly in command line, use:
```bash
Rscript Requirements/install_require.R
```

### Method 2: Manual Installation

Copy each package from `r.txt` and install them individually with `install.packages("package_name")`.

---

## 🚀 Quick Setup (One-Command Installation)

### For R Packages:
```r
install.packages(c("IRkernel", "remotes", "devtools", "rJava", "tidyverse", "tidymodels", "LightLogR", "Spectran", "blandr", "factoextra", "corrplot", "h2o", "themis", "ggstream", "Polychrome", "patchwork", "cowplot", "gridExtra", "RColorBrewer", "viridis", "FactoMineR", "stringr", "lubridate"), dependencies = TRUE); IRkernel::installspec()
```

### For Python Packages:
```bash
pip install jupyterlab ipykernel bash_kernel jupyterlab-system-monitor jupyterlab-drawio jupyterlab-link-share
```

---

## 🔍 Package Categories

### R Packages
| Category | Packages | Purpose |
|----------|----------|---------|
| **Core Environment** | `IRkernel`, `remotes`, `devtools`, `rJava` | R kernel setup and development tools |
| **Data I/O** | `readxl`, `writexl` | Excel file operations |
| **Data Science** | `tidyverse`, `tidymodels` | Data manipulation and modeling framework |
| **Statistical Analysis** | `LightLogR`, `Spectran`, `blandr`, `factoextra`, `corrplot`, `FactoMineR` | Specialized statistical analysis and PCA |
| **Machine Learning** | `h2o`, `themis` | ML platform and imbalanced data handling |
| **Visualization** | `ggstream`, `Polychrome`, `patchwork`, `cowplot`, `gridExtra`, `RColorBrewer`, `viridis` | Advanced plotting, layouts, and color schemes |
| **Utilities** | `stringr`, `lubridate` | String and date/time manipulation (included in tidyverse) |

### Python Packages
| Category | Packages | Purpose |
|----------|----------|---------|
| **Core Environment** | `jupyterlab`, `ipykernel`, `bash_kernel` | Jupyter development environment |
| **Extensions** | `jupyterlab-system-monitor`, `jupyterlab-drawio`, `jupyterlab-link-share` | JupyterLab enhancements |

---

## ⚠️ Troubleshooting

### Common Issues:

#### R Package Installation Issues:
```r
# If Java-related packages fail (rJava, h2o):
# Install Java JDK first, then:
R CMD javareconf

# If specific packages fail, try installing individually:
install.packages("package_name", dependencies = TRUE, repos = "https://cran.r-project.org/")
```

#### Python Package Installation Issues:
```bash
# If permission errors occur:
pip install --user package_name

# If behind corporate firewall:
pip install --trusted-host pypi.org --trusted-host pypi.python.org --trusted-host files.pythonhosted.org package_name
```

#### IRkernel Setup Issues:
```r
# If R kernel doesn't appear in Jupyter:
IRkernel::installspec(user = FALSE)  # System-wide installation
# or
IRkernel::installspec(user = TRUE)   # User-specific installation
```

---

## 📊 Environment Testing

After installation, test your environment:

### Test R Environment:
```r
# Test core packages
library(tidyverse)
library(h2o)
library(ggplot2)

# Test data loading
data(iris)
print("R environment setup successful!")
```

### Test Python Environment:
```python
# Test in Jupyter notebook
import pandas as pd
import numpy as np
print("Python environment setup successful!")
```

---

## 📋 Version Information

This requirements file was last updated: **June 26, 2025**

For issues or questions, please refer to the main project documentation or create an issue in the project repository.