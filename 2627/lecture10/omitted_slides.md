
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
