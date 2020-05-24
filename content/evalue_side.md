# Computing an E-value for selection bias

Like the E-value for unmeasured confounding, the selection bias E-value describes the minimum strength of association between several (possibly unmeasured) factors that would be sufficient to have created enough selection bias to explain away an observed exposure-outcome association. In other words, if the true causal relationship were null ($\text{RR}_{true} = 1$ or $\text{OR}_{true} = 1$), how strong would selection bias need to be to have resulted in your observed estimate.

The parameters that the E-value refers to depends on the structure of selection bias and the assumptions an investigator is willing to make, and are printed with the results. Descriptions as well as mathematical definitions of those parameters are available here<a href="#" data-toggle="modal" data-target="#modal_parameters_S"> <i class="fa fa-info-circle"></i> </a>. As with the previous page, more information about the assumptions is available by clicking the icon.

For example, with no additional assumptions, if the selection bias E-value for a risk ratio is 4, then if $\text{RR}_{UY |(A = 1)} = \text{RR}_{UY |(A = 0)} = \text{RR}_{SU |(A = 1)} = \text{RR}_{SU |(A = 0)} = 4$, selection bias could explain your observed result, but weaker relationships between those factors could not.

