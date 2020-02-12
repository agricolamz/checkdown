# `Checkdown`

G. Moroz

## Instalation

This package is not on CRAN, but it will be there. Until then install this package with the following command:

```
install.packages("devtools")
devtools::install_github("agricolamz/checkdown")
```

## Demo

The main goal of this package to create autochecking boxes. Load the library:

```
library(checkdown)
```

Imagine that we want to create a checkbox with the answer 4. All you need is to create a following chunk with the chunck atribute `results='asis'` in your `rmarkdown` document:
```
autocheck_question(question_id = 1, answer =  4)
```

`question_id` argument should be different for all questions. It is possible to avoid code in the output with the chunk argument `echo=FALSE`, but I will keep it `TRUE`. It is also possible to change wrong and right answer's messages using `wrong` and `right` arguments of the `autocheck_question()` function. Let's create some more questions.

Solve 3+3:
```
autocheck_question(question_id = 2, answer =  6, right = "correct", wrong = "not correct")
```

Type *la-la*:
```
autocheck_question(question_id = 3, answer =  "la-la")
```

All you need now is finish your document with a following chunk with the chunck atribute `results='asis'`:
```
autocheck_code()
```

[Here](https://agricolamz.github.io/checkdown/) is the on-line version. Check it out!
