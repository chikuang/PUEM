# PUEM: EM algorithm and staitsitcal inference on PU data using Empirical Likelihood

*Qinglong Tian, Pengfei Li, Xin Zhang, Siyan Liu, Chi-Kuang Yeh*

*March 12, 2024*

---

### Description

Implementation of the statistical inference on Positive and Unlabeled (PU) data under the situation where the mixture is sample at random (SAR) or sample complete at random (SCAR). This can be translated to perform a statistical test on a density ratio model under the assumption of exponential tilting $p_s/q_t = \exp(\alpha + \beta^\top T(x))$, where $T(x)$ is some known function of $x$. 

### Installation

Install the **R** [**`devtools`**](https://CRAN.R-project.org/package=devtools) package and run
```{r}
devtools::install_github("chikuang/PUEM")
```

### Examples

After installing and loading the package, please install the Vignette by `vignette(package = "PUEM")`.

### TODO

- [ ] Add inference on $\pi$
- [ ] Add examples
- [ ] Fix some descriptions of the functions
- [ ] Update and add documentation with more details
- [ ] Rename the package (maybe)
- [ ] Add the adjustment of $\hat{\pi}$ by adding the "flip" to prevent the proportion mismatch
