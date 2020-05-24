Mathematical definitions of all of the possible parameters follow. Only certain parameters will be applicable in any given situation.

## For general selection bias:

$$
	\text{RR}_{UY |(A = 1)} = \frac{\max_u P(Y = 1 | A = 1, u)}{\min_u P(Y = 1 | A = 1, u)}
$$
$$
	\text{RR}_{UY |(A = 0)} = \frac{\max_u P(Y = 1 | A = 0, u)}{\min_u P(Y = 1 | A = 0, u)}
$$
$$
	\text{RR}_{SU |(A = 1)} = \max_u \frac{P(U = u | A = 1, S = 1)}{P(U = u | A = 1, S = 0)}
$$
$$
	\text{RR}_{SU |(A = 0)} = \max_u \frac{P(U = u | A = 0, S = 0)}{P(U = u | A = 0, S = 1)} \;.
$$

These values can be interpreted as the maximum relative risks comparing any two values of $U$ on $Y$ within strata of $A = 1$ and $A = 0$, respectively; and the maximum factors by which selection increases the prevalence of some value of $U$ within the stratum $A = 1$ and by which non-selection increases the relative prevalence of some value of $U$ within stratum $A = 0$.

## When the selected population is the target population:

$$
\text{RR}_{UY |(S = 1)} = \max_a \frac{\max_u P(Y = 1 | A = a, S = 1, U = u)}{\min_u P(Y = 1 | A = a, S = 1, U = u)}
$$
$$
\text{RR}_{AU |(S = 1)} = \max_u \frac{P(U = u | A = 1, S = 1)}{P(U = u | A = 0, S = 1)} \;.
$$

The first value can be interpreted as the maximum factor by which the unmeasured variable increases risk of the outcome within some exposure group, among the selected population. The second is less intuitive, describing the relationship between the exposure and the unmeasured factor that's induced when conditioning on selection. However, it can generally be interpreted as referring to the extent to which $U$ increases the probability of selection.

## Control selection bias:

$$
	\text{RR}_{UA_1} = \frac{\max_u P(A = 1 | Y = 0, u, c)}{\min_u P(A = 1 | Y = 0, u, c)}
$$
$$
	\text{RR}_{UA_0} = \frac{\max_u P(A = 0 | Y = 0, u, c)}{\min_u P(A = 0 | Y = 0, u, c)}
$$
$$
	\text{RR}_{S_1U} =\max_u \frac{P(U = u | Y = 0, S = 1, c)}{P(U = u | Y = 0, S = 0, c)}
$$
$$
	\text{RR}_{S_0U} = \max_u \frac{P(U = u | Y = 0, S = 0, c)}{P(U = u | Y = 0, S = 1, c)} \;.
$$

The first two values describe how much more likely controls are to be exposed (or unexposed) given two different values of $U$. The second two values describe the maximum extent to which some value of $U$ is more likely among the selected controls compared to the general population of possible controls, and vice versa.