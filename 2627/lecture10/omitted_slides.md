
## $F$-test: alternative explanation

There is another identical way to reason about the $F$ statistic.

The result is identical, but conceptually different.

Suppose we compute

- $\mathrm{MSR} = \RSS / (p - 1)$
- $\mathrm{MSE} = \SSE / (N - p)$

<!-- Recall that variance of y is TSS / (N - 1) -->

- $N$ = the sample size
- $p$ = the number of parameters in the model
  * for bivariate regression we only have $a$ and $b$ so $p = 2$.

These are called _mean squares_.

These are very related to variances.

## $F$-test: alternative explanation

Recall:

::: {.columns}

::: {.column width="60%"}



 - $s_y^2 = TSS / (N - 1)$
 - $\SSE = \sum_{i=1}^N (y_i - \hat{y})^2 = \sum_{i=1}^N \res_i^2$


:::

::: {.column width="40%"}


::: {style="font-size: 70%;"}

$$
\begin{align}
y_i &= a + b \times x_i + \res_i \\
y_i - (a + b \times x_i) &= \res_i \\
y_i - \hat{y} &= \res_i
\end{align}
$$

:::

:::

:::

. . .

We estimate both $a$ and $b$ we divide by $N - 2$:


$$
\MSE = \frac{1}{N - 2} \sum_{i=1} \res_i^2
$$

This is the _residual variance_: the variance that remains after accounting for the predictor.

::: {.notes}

mention here that we divide by N - 1 because we already estimated the sample mean from the data.

Now we've estimated two things from the data, the intercept and the sample mean.

:::

## $F$-test: alternative explanation

We assumed that $\res \sim \mathcal{N}(0, \sigma_\res^2)$.

So $\MSE = \frac{1}{N - 2} \sum_{i=1} \res_i^2$ estimates the variance $\sigma_\res^2$.


. . .

::: {.callout-insight}

In the gitbook and videos $\sigma_\res^2$ is (sometimes) called $\sigma^2_{\text{Error}}$

<!-- TODO: compare this to the lecture qmd once more! -->

:::



<!--  -->
<!--  -->


## F-test: alternative explanation

Recall: the null model predicts the mean for everyone; the regression model also estimates a slope.

If $\hypo_0$ is true, then we know that:

- $\TSS$ estimates $(N - 1)\times \sigma_y^2$
- $\SSE$ estimates $(N - 2)\times\sigma_\res^2 \underbrace{= (N - 2) \times \sigma_y^2}_{\substack{\text{Under $\hypo_0$}}}$

. . .

We've already seen this!

## Explained Variance: Demo

{{< include ../common_resources/components/variance-demo.qmd >}}


::: {.notes}

show that as R^2 goes to zero that RSS = 0, and that TSS and SSE become the same thing.

:::

## F-test: alternative explanation

If $\hypo_0$ is true, then we know that the expectations are:

- $\TSS$ estimates $(N - 1)\times \sigma_y^2$
- $\SSE$ estimates $(N - 2)\times\sigma_\res^2 \underbrace{= (N - 2) \times \sigma_y^2}_{\substack{\text{Under $\hypo_0$}}}$

. . .

If we do this for $\RSS = \TSS - \SSE$ we get

$$
\begin{align}
\RSS &\text{ estimates } (N - 1)\times \sigma_y^2 - (N - 2) \times \sigma_y^2\\
\RSS &\text{ estimates } \sigma_y^2
\end{align}
$$

## F-test: alternative explanation

Divide each sum of squares by its degrees of freedom to obtain the _mean squares_:

- $\mathrm{MSR} = \RSS / (p - 1)$
- $\mathrm{MSE} = \SSE / (N - p)$

- $N$ = the sample size
- $p$ = the number of parameters in the model
  * for bivariate regression we only have $a$ and $b$ so $p = 2$.

## From mean squares to F

Their ratio is:

$$
F=\frac{\MSR}{\MSE} = \frac{(N - p)}{p - 1} \frac{R^2}{1 - R^2}
$$

* $df_1$: p-1
* $df_2$: n-p


## F-test: summary

- Under $\hypo_0$, MSR and MSE estimate the same error variance.
- A large ratio $F = \MSR / \MSE$ is evidence against $\hypo_0$.
- The $p$-value is the area to the **right** of the observed $F$.

. . .

A significant test says there is a linear association. $R^2$ tells us how much sample variation the line explains.



## F-test: alternative explanation

<!-- Teaching suggestion: this repeats the mean-squares ratio above. I would combine those explanations or move this slide to an appendix to leave more time for interpreting the faces tests. Retained here as requested. -->

The F-test is a ratio of two sources of variance:

$$
F = \frac{\MSR}{\MSE} = \frac{\RSS/(p-1)}{\SSE/(N-p)}
$$

$p$: number of parameters ($a$ and $b$); $N$: number of observations (here, faces).

* $df_1 = p - 1 = 1$
* $df_2 = N - p = 1002$

::: {.notes}

These mean squares are estimates formed from sums of squares, not two known population variances. Under the null they estimate the same error variance; their ratio fluctuates across samples rather than being exactly one.

:::
