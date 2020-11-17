\name{scoreModels_culture}
\alias{scoreModels_culture}
\title{Score models by culture group}
\usage{
scoreModels_culture(test_vars, df, culture_key)
}
\arguments{
\item{test_vars}{A character vector containing relevant variables from the main coded eHRAF dataframe (df)}

\item{df}{The main (numeric) coded eHRAF dataframe}

\item{culture_key}{The culture key that maps culture group names to culture codes, which already exists in this package (called `culture_key`)}
}
\value{
a dataframe
}
\description{
Scores a model by cultural group based on variables, generating evidence for, against, and absence of evidence.
}
