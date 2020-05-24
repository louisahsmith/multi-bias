# message to display if non-null true value
nonnull.mess <- 'Note: You are calculating a "non-null" selection bias E-value, i.e., a value for the minimum
                amount of selection bias needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'

# various functions
BF <- function(RR1, RR2) {
  (RR1 * RR2) / (RR1 + RR2 - 1)
}

RRUY0 <- function(num) {
  numericInput(paste0("RRUY0", num), "$\\text{RR}_{UY \\mid (A = 0)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum outcome risk ratio comparing two levels of U among the unexposed",
          placement = "top"
        )
    )
}

RRUY1 <- function(num) {
  numericInput(paste0("RRUY1", num), "$\\text{RR}_{UY \\mid (A = 1)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum outcome risk ratio comparing two levels of U among the exposed",
          placement = "top"
        )
    )
}

RRSU0 <- function(num) {
  numericInput(paste0("RRSU0", num), "$\\text{RR}_{SU \\mid (A = 0)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum factor by which non-selection increases some value of U among the unexposed",
          placement = "top"
        )
    )
}

RRSU1 <- function(num) {
  numericInput(paste0("RRSU1", num), "$\\text{RR}_{SU \\mid (A = 1)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum factor by which selection increases some value of U among the exposed",
          placement = "top"
        )
    )
}

thresh_sel <- function (x, true = 1, sel_pop = FALSE, S_eq_U = FALSE, risk_inc = FALSE, 
                        risk_dec = FALSE) {
  if (is.na(x)) 
    return(NA)
  if (x <= 1) {
    x = 1/x
    true = 1/true
  }
  if (true <= x) {
    rat = x/true
  }
  else if (true > x) {
    rat = true/x
  }
  if (sel_pop) {
    return(rat + sqrt(rat * (rat - 1)))
  }
  if (!S_eq_U & !risk_inc & !risk_dec) {
    return(sqrt(rat) + sqrt(sqrt(rat) * (sqrt(rat) - 1)))
  }
  if (S_eq_U & !risk_inc & !risk_dec) {
    return(sqrt(rat))
  }
  if (S_eq_U & risk_inc) {
    return(rat)
  }
  if (S_eq_U & risk_dec) {
    return(rat)
  }
  if (risk_inc) {
    return(rat + sqrt(rat * (rat - 1)))
  }
  if (risk_dec) {
    return(rat + sqrt(rat * (rat - 1)))
  }
}

svalues <- function(est, lo = NA, hi = NA, true = 1, sel_pop = FALSE, 
                    S_eq_U = FALSE, risk_inc = FALSE, risk_dec = FALSE) {
  values = c(est, lo, hi)
  if (est < 0) 
    stop("RR cannot be negative")
  if (true < 0) 
    stop("True value is impossible")
  if (risk_inc & risk_dec) 
    stop("You have made incompatible assumptions about the \n association between selection and risk.")
  null.CI = NA
  if (est > true & !is.na(lo)) {
    null.CI = (lo < true)
  }
  if (est < true & !is.na(hi)) {
    null.CI = (hi > true)
  }
  if (!is.na(lo) & !is.na(hi)) {
    if (lo > hi) 
      stop("Lower confidence limit should be less than upper confidence limit")
  }
  if (!is.na(lo) & est < lo) 
    stop("Point estimate should be inside confidence interval")
  if (!is.na(hi) & est > hi) 
    stop("Point estimate should be inside confidence interval")
  E = vapply(values, FUN = function(x) thresh_sel(x, true = true, sel_pop = sel_pop, 
                                                                    S_eq_U = S_eq_U, risk_inc = risk_inc, 
                                                                    risk_dec = risk_dec),
                      FUN.VALUE = numeric(1))
  if (!is.na(null.CI) & null.CI == TRUE) {
    E[2:3] = 1
  }
  if (!is.na(lo) | !is.na(hi)) {
    if (est > true) 
      E[3] = NA
    if (est < true) 
      E[2] = NA
    if (est == true) {
      E[2] = 1
      E[3] = NA
    }
  }
  result = rbind(values, E)
  result
}
