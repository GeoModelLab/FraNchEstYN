# --- 0) One-time installs (safe if already installed) ---
pkgs <- c("usethis", "pkgdown", "rmarkdown", "knitr", "desc")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(usethis)
library(pkgdown)
library(desc)
devtools::build_vignettes()
# <<< EDIT THIS ONLY IF NEEDED >>>
gh_user <- "GeoModelLab"           # your GitHub org/user
repo    <- basename(getwd())       # repo name = current folder

# --- 1) Basic pkgdown scaffolding ---
#usethis::use_pkgdown()             # adds starter _pkgdown.yml (if missing)
# --- 4) Build locally (sanity check) ---
#install.packages("pkgdown")
pkgdown::build_site()  # writes to ./docs
#pkgdown::build_site(preview = interactive())

# --- 5) Set up GitHub + Pages via Actions ---
# If the repo isn't on GitHub yet, uncomment the next line and follow prompts:
# usethis::use_github()

# Creates the GitHub Actions workflow and configures Pages for pkgdown
usethis::use_pkgdown_github_pages()

# Commit everything (if repo already has git)
if (uses_git()) {
  usethis::use_git_message("Set up pkgdown site, vignettes, and Pages workflow")
}

message("\n✅ pkgdown configured. Push to GitHub; Actions will build & publish the site.\nURL: https://", gh_user, ".github.io/", repo, "\n")
