# Selection bias

Studies are often conducted in a subset of a population, whether by necessity or convenience. When the selected portion of the population differs from the total population with respect to the exposure and outcome of interest, selection bias can result. This may be the case even if the selected population is the only group about which you are attempting to make a causal claim.

However, it can be difficult to know the extent to which an estimate of a causal effect in a selected population systematically differs from the true causal effect, as it often depends on unmeasured factors. One way that you can deal with this uncertainty is by conducting a sensitivity analysis for selection bias. We have developed a simple approach for this:

- You can **compute bounds** based on hypothesized or estimated relationships with the unmeasured factor(s) creating the bias.
- You can **calculate selection bias E-values**, which describe the minimum strength that those relationships would have to have to explain away your estimate.
