library(dplyr)
library(usethis)

setwd("C://GitHub//FraNchEstYN")

df <- read.csv(
  "src_csharp/FraNchEstYN/FraNchEstYN/files/parameters/franchestynParametersForPackage.csv",
  stringsAsFactors = FALSE
)

# Helper: build one parameter set
make_param_list <- function(data) {
  setNames(
    lapply(seq_len(nrow(data)), function(i) {
      list(
        description = data$Description[i],
        unit        = data$unit[i],
        min         = data$min[i],
        max         = data$max[i],
        value       = data$value[i],
        calibration = tolower(data$calibration[i]) %in% c("x","true","1","yes")
      )
    }),
    data$Parameter
  )
}

# Split by model and set name
split_sets <- function(df, model) {
  sub <- df[tolower(df$Model) == tolower(model), ]
  if (!nrow(sub)) return(list())
  if (!"Set" %in% names(sub)) sub$Set <- "Default"

  sets <- split(sub, sub$Set)
  lapply(sets, make_param_list)
}

cropParameters    <- split_sets(df, "crop")
diseaseParameters <- split_sets(df, "disease")
fungicideParameters <- split_sets(df, "fungicide")

usethis::use_data(cropParameters, diseaseParameters, fungicideParameters, overwrite = TRUE)
