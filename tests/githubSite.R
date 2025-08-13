# --- 0) One-time installs (safe if already installed) ---
pkgs <- c("usethis", "pkgdown", "rmarkdown", "knitr", "desc")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(usethis)
library(pkgdown)
library(desc)

# <<< EDIT THIS ONLY IF NEEDED >>>
gh_user <- "GeoModelLab"           # your GitHub org/user
repo    <- basename(getwd())       # repo name = current folder

# --- 1) Basic pkgdown scaffolding ---
usethis::use_pkgdown()             # adds starter _pkgdown.yml (if missing)

# Overwrite _pkgdown.yml with our config
yaml <- sprintf('
url: https://%s.github.io/%s
title: %s
template:
  bootstrap: 5
navbar:
  structure:
    left:  [reference, articles]
    right: [github]
  components:
    reference:
      text: Reference
      href: reference/index.html
    articles:
      text: Articles
      menu:
        - text: Quick start
          href: articles/quick-start.html
        - text: Reference data format
          href: articles/reference-format.html
        - text: C# integration notes
          href: articles/csharp-integration.html
    github:
      icon: fa-github
      href: https://github.com/%s/%s
home:
  links:
    - text: GitHub repository
      href: https://github.com/%s/%s
reference:
  - title: Main functions
    contents:
      - franchestyn
      - starts_with("read")
  - title: Utilities
    contents:
      - everything()
', gh_user, repo, repo, gh_user, repo, gh_user, repo)
writeLines(yaml, "_pkgdown.yml")

# --- 2) DESCRIPTION fields (URL / BugReports / Suggests / VignetteBuilder) ---
d <- desc::desc(file = "DESCRIPTION")
d$set("URL", sprintf("https://%s.github.io/%s, https://github.com/%s/%s", gh_user, repo, gh_user, repo))
d$set("BugReports", sprintf("https://github.com/%s/%s/issues", gh_user, repo))
d$set_dep("pkgdown", type = "Suggests")
d$set_dep("rmarkdown", type = "Suggests")
d$set_dep("knitr", type = "Suggests")
d$set("VignetteBuilder", "knitr")
d$write(file = "DESCRIPTION")

# --- 3) Add articles (vignettes) ---
dir.create("vignettes", showWarnings = FALSE, recursive = TRUE)

# Quick start
qs_path <- file.path("vignettes", "quick-start.Rmd")
qs_txt <- c(
  "---",
  'title: "Quick start"',
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{Quick start}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "```r",
  sprintf("# remotes::install_github('%s/%s')", gh_user, repo),
  "library(FraNchEstYN)",
  "# df <- franchestyn(weather_data, management_data, calibration = 'all', start_end = c(2000, 2025))",
  "```"
)
writeLines(qs_txt, qs_path)

# Reference data format
rf_path <- file.path("vignettes", "reference-format.Rmd")
rf_txt <- c(
  "---",
  'title: "Reference data format"',
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{Reference data format}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "**CSV columns (case-insensitive; underscores/spaces ignored):**",
  "",
  "- `year`, `doy`",
  "- `variety`, `site`",
  "- `FINT` (or `lightInterception`)",
  "- **YieldActual** (aliases: `YieldActual`, `YieldDiseased`, `YieldAct`, `yieldact`, `YieldLimited`, `WGRN`, `GrainYield`) — *optional*",
  "- **YieldAttainable** (aliases: `YieldAttainable`, `YieldUnlimited`, `YieldPotential`, `Yield`) — *optional*",
  "- Per-disease column named exactly the `disease` you pass (also matches `<disease>_sev`, `<disease>_severity` in the C# reader).",
  "",
  "```csv",
  "variety,site,year,doy,FINT,YieldActual,YieldAttainable,thisDisease",
  "Generic,SiteA,2010,200,0.62,5.1,6.2,0.15",
  "```"
)
writeLines(rf_txt, rf_path)

# C# integration notes
cs_path <- file.path("vignettes", "csharp-integration.Rmd")
cs_txt <- c(
  "---",
  'title: "C# integration notes"',
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{C# integration notes}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "- Reader matches headers case-insensitively and ignores `_`/spaces.",
  "- Optional: **YieldAttainable**, **YieldActual** (separate columns).",
  "- Disease severity column is looked up by the `disease` name you pass (also `<disease>_sev`, `<disease>_severity`)."
)
writeLines(cs_txt, cs_path)

message("Wrote vignettes to: ", normalizePath("vignettes", mustWork = FALSE))

# --- 4) Build locally (sanity check) ---
pkgdown::build_site(preview = interactive())

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
