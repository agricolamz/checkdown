# Create check-fields and check-boxes with `checkdown`

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN version](http://www.r-pkg.org/badges/version/checkdown)](https://cran.r-project.org/package=checkdown)
[![](http://cranlogs.r-pkg.org/badges/grand-total/checkdown)](https://CRAN.R-project.org/package=checkdown)
[![DOI](https://zenodo.org/badge/240126674.svg)](https://zenodo.org/badge/latestdoi/240126674)



G. Moroz

## Instalation

Get the stable version from CRAN:

```{r, eval=FALSE}
install.packages("lingtypology")
```

… or get the development version from GitHub:

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github("agricolamz/checkdown")
```

## Demo

The main goal of this package to create autochecking boxes in `rmarkdown`. It could be used in class, when teacher share materials and tasks, so student can solve some problems and check themselves. In contrast with the `learnr` package the `checkdown` package works without shiny. Load the library:

```{r}
library(checkdown)
```

Imagine that we want to create a checkbox with the answer 4. All you need is to create a following chunk with the chunck atribute `results='asis'` in your `rmarkdown` document:
```{r, results='asis'}
autocheck_question(answer =  4)
```

It is possible to change wrong and right answer's messages using `wrong` and `right` arguments of the `autocheck_question()` function. Let's create some more questions.

Solve 3+3:
```{r, results='asis'}
autocheck_question(answer =  6, right = "correct", wrong = "not correct")
```

Type *la-la*:
```{r, results='asis'}
autocheck_question(answer =  "la-la")
```

It is also possible to create a list of answers for students to choose:

```{r, results="asis"}
autocheck_question("banana", options = c("apple", "banana", "bread"))
autocheck_question("banana", options = c("apple", "banana", "bread"), type = "radio")
```

## Some hints

* It is possible to avoid code in the output with the chunk argument `echo=FALSE`.
* For now the package `checkdown` works only with html output and will not print anything for other otputs.
