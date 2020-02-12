\name{wiDB_validate}

\alias{wiDB_validate}

\title{
Validate query input
}

\description{
This function is called by \code{\link{wiDB_data}} and \code{\link{wiDB_sites}} to validate arguments passed by the user.
}

\usage{
wiDB_validate = function(minLat, maxLat, minLong, maxLong, minElev, maxElev,
                         minDate, maxDate, countries, states, types, projects)
}

\arguments{
  \item{minLat}{
    numeric. Minimum latitude for query region, in decimal degrees. South negative.
  }
  \item{maxLat}{
    numeric. Maximum latitude for query region, in decimal degrees. South negative.
  }
  \item{minLong}{
    numeric. Minimum longitude for query region, in decimal degrees. West negative.
  }
  \item{maxLong}{
    numeric. Maximum longitude for query region, in decimal degrees. West negative.
  }
  \item{minElev}{
    numeric. Minimum elevation for query. Meters.
  }
  \item{maxElev}{
    numeric. Maximum elevation for query. Meters.
  }
  \item{minDate}{
    character. Minimum date for query. Format: "YYYY-MM-DD"
  }  
  \item{maxDate}{
    character. Maximum date for query. Format: "YYYY-MM-DD"
  }
  \item{countries}{
    character. Vector of one or more two-letter country codes for query.
  }
  \item{states}{
    character. Vector of one or more two-letter state or province codes for query.
  }
  \item{types}{
    character. Vector of one or more sample types for query. See vocabulary in the \href{http://wateriso.utah.edu/waterisotopes/pages/spatial_db/contribute.html}{wiDB tempate}.
  }
  \item{projects}{
    character. Vector of one or more project codes for query.
  }
}

\details{
Not intended to be called by user. Calling functions pad all arguments with NULL values if not provided by user.
}

\examples{
#Call wiDB_sites with incorrectly formatted date, returns error
sites = wiDB_sites(minDate = "9/1/2019", types = "Tap")
}
