% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRxgboost_data_prep_forecast.R
\name{SRxgboost_data_prep_forecast}
\alias{SRxgboost_data_prep_forecast}
\title{SRxgboost_data_prep_forecast}
\usage{
SRxgboost_data_prep_forecast(df, label_encoding = TRUE, nthreads = NULL)
}
\arguments{
\item{df}{data.frame}

\item{label_encoding}{boolean}

\item{nthreads}{integer}
}
\value{
assigns output into global workspace
}
\description{
Prepare data for forecasting with XGBOOST model. Creates a sparse matrix and dmatrix.
}
