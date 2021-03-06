% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_hint.R
\name{check_hint}
\alias{check_hint}
\title{Create check-fields and check-boxes for 'rmarkdown'}
\usage{
check_hint(
  hint_text,
  hint_title = "Click here to see/close the hint",
  hint_id = sample(1e+05:1, 1)
)
}
\arguments{
\item{hint_text}{hint paragraph text; can contain markdown}

\item{hint_title}{click paragraph text; can contain markdown}

\item{hint_id}{unique identidier of the question}
}
\value{
returns the html and javascript code
}
\description{
Create check-fields and check-boxes for 'rmarkdown'
}
\examples{

# ```{r, results='asis', echo=FALSE}
# check_hint("You can use argument `asis`!")
# ```

}
\author{
George Moroz <agricolamz@gmail.com>
}
