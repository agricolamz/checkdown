% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_question.R
\name{check_question}
\alias{check_question}
\title{Create check-fields and check-boxes for 'rmarkdown'}
\usage{
check_question(
  answer,
  right = "Correct",
  wrong = "I have a different answer",
  options = NULL,
  type = "select",
  alignment = FALSE,
  button_label = "check",
  random_answer_order = FALSE,
  question_id = sample(1:1e+05, 1)
)
}
\arguments{
\item{answer}{correct answer (can be a double or a string). It is possible to put here a vector of several answers.}

\item{right}{form reaction on right answer}

\item{wrong}{form reaction on wrong answer}

\item{options}{vector of values for the selection list type}

\item{type}{character that defines type of the list. Possible values: \code{select}, \code{radio}, \code{checkbox}}

\item{alignment}{logical argument for options' alignment: vertical if \code{TRUE}, horizontal if \code{FALSE}}

\item{button_label}{character value that will be displayed on the button}

\item{random_answer_order}{logical argument that denotes whether answers should be shuffled}

\item{question_id}{unique identidier of the question}
}
\value{
returns the html and javascript code
}
\description{
Create check-fields and check-boxes for 'rmarkdown'
}
\examples{

# ```{r, results='asis', echo=FALSE}
# check_question(answer = 5)
# ```

}
\author{
George Moroz <agricolamz@gmail.com>
}
