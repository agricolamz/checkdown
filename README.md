# Create check-fields and check-boxes with `checkdown`

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN version](http://www.r-pkg.org/badges/version/checkdown)](https://cran.r-project.org/package=checkdown)
[![](http://cranlogs.r-pkg.org/badges/grand-total/checkdown)](https://CRAN.R-project.org/package=checkdown)
[![DOI](https://zenodo.org/badge/240126674.svg)](https://zenodo.org/badge/latestdoi/240126674)

G. Moroz

## Instalation

Get the stable version from CRAN:

```{r, eval=FALSE}
install.packages("checkdown")
```

… or get the development version from GitHub:

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github("agricolamz/checkdown")
```

## 1. Demo

The main goal of this package to create checking fields and boxes in `rmarkdown`. It could be used in class, when teacher share materials and tasks, so student can solve some problems and check themselves. It is really important since some students are too shy to ask a question, so you can create tasks that will check on the fly the understanding of the class material and give some hints to those students that get stuck. In contrast with the `learnr` package the `checkdown` package works without shiny. Load the library:

```{r}
library(checkdown)
```

### 1.1 Ask question with the `check_question()` function

Imagine that we want to create a checkbox with the answer 4. All you need is to create a following chunk with the chunck atribute `results='asis'` in your `rmarkdown` document:
```{r, results='asis'}
check_question(answer =  4)
```

It is possible to change wrong and right answer's messages using `wrong` and `right` arguments of the `check_question()` function. Let's create some more questions.

Solve 3+3:
```{r, results='asis'}
check_question(answer =  6, right = "correct", wrong = "not correct")
```

Type *la-la*:
```{r, results='asis'}
check_question(answer =  "la-la")
```

Number of answers is not limited:
```{r, results='asis'}
check_question(answer =  1:5)
```

It is also possible to create a list of answers for students to choose:

```{r, results="asis"}
check_question("banana", options = c("apple", "banana", "bread"))

check_question("banana", options = c("apple", "banana", "bread"), type = "radio")
```

### 1.2 Give some hints with the `check_hint()` function

Sometimes you now in advance what kind of mistakes will your students do. Some students are shy and don't like ask questions, so hints could partially solve  this problem. Again all you need is to create a following chunk with the chunck atribute `results='asis'` in your `rmarkdown` document:

```{r, results="asis"}
check_hint("You can use the rmarkdown package")
```

Of course it is possible to change the message:
```{r, results="asis"}
check_hint("You can use the rmarkdown package",
           click_text = "CLICK HERE")
```

It is possible to use Markdown inside messages:

```{r, results="asis"}
check_hint("* You can use the **`rmarkdown` package**",
           click_text = "Click he`R`e")
check_question(answer =  4, 
               wrong = "a**R**e you su**R**e", 
               right = "### `R`ight")
```

## 2. Some important notes

* It is possible to avoid code in the output `.html`. Just use the chunk argument `echo=FALSE`.
* For now the package `checkdown` works only with html output and will not print anything for other otputs.
