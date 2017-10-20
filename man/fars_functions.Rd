\name{fars_functions}
\alias{fars_functions}

\title{
Read and Analyze data
}
\description{
  read csv files about accident and analyze the data
}
\usage{
 fars_read('accident_2013.csv')
 make_filename(2006)
 fars_read_years(c(2013,2014,2015))
 fars_summarize_years(c(2013,2014,2015))
 fars_map_state(3,2005)
}

\details{
More detailed description is available in the vignette folder.
}

\value{
fars_read() doesn't return any value.

make_filename() returns a string.

fars_read_years() returns a dataframe whoes size depends on the input.

fars_summarize_years() will return a table with 12 rows and number of columns
depends on the length of input.

fars_map_state() returns a figure of the map of the specified state.
}

\author{
Sherry Xiu
}

