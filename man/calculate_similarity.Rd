% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_similarity.R
\name{calculate_similarity}
\alias{calculate_similarity}
\title{Calculate the Cosine Similarity Between Vectors}
\usage{
calculate_similarity(words = NULL, dimensions, by = 1)
}
\arguments{
\item{words}{If you want to filter the matrix to only
a set of tokens, include a vector of tokens. The `by`
argument will be used to filter the data to only these
values. Note that if you want to filter by rows, the
row names should represent your tokens, not the first column.}

\item{dimensions}{Your matrix of numerical dimension values.}

\item{by}{use `1` to calculate by row and `2` to calculate
by column.}
}
\value{
A matrix of cosine values between either rows
or columns.
}
\description{
This function uses the `lsa` package to calculate the
`cos` between vectors. For this function, you should have
a matrix of tokens by dimensions, and you can specify
which direction you want to calculate the cosine (by
rows or columns).
}
\examples{
# af_cosine <- calculate_similarity(
#   words = af_final$sentence, # the tokens you want to filter
#   dimensions = af_dims, # the matrix of items
#   by = 1 # 1 for rows, 2 for columns)

}
