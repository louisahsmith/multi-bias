poss_misclassification <- purrr::possibly(misclassification, otherwise = NA)
poss_selection <- purrr::possibly(selection, otherwise = NA)

get_selection_args <- function(bias) {
  logs <- unlist(attributes(bias)[c("selected", "selected", "increased_risk", 
                                    "decreased_risk", "SU")])
  logs[1] <- !logs[1]
  paste0(
    "selection(",
    paste(
      c("\"general\"", "\"selected\"", "\"increased risk\"",
        "\"decreased risk\"", "\"S = U\"")[logs],
      collapse = ", "
    ), ")"
  )
}

get_misclassification_args <- function(bias) {
  logs <- attributes(bias)[c("names", "rare_outcome", "rare_exposure")]
  nm <- sub(pattern = "\\smisclassification", "", logs$names)
  logs$names <- NULL
  logs[sapply(logs, is.null)] <- NULL
  after_nm <- ifelse(length(logs) > 0, "\", ", "\"")
  paste0(
    "misclassification(\"", nm, after_nm,
    paste(paste(names(logs), logs, sep = " = "), collapse = ", "),
    ")"
  )
}

get_confounding_args <- function(bias) {
  "confounding()"
}

get_bias_args <- function(bias) {
  arg_func <- switch(names(bias),
                     "confounding" = get_confounding_args,
                     "selection" = get_selection_args,
                     "outcome misclassification" = get_misclassification_args,
                     "exposure misclassification" = get_misclassification_args
  )
  arg_func(bias)
}

get_multibias_args <- function(multi) {
  biases <- lapply(multi, get_bias_args)
  paste0("multi_bias(\n", paste(biases, collapse = ",\n"), ")")
}