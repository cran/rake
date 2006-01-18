\name{certify}
\alias{certify}
\alias{statpoptotal}
\docType{data}
\keyword{datasets}
\title{1994 Survey of ASA Membership Certification Data}
\description{In 1994, the American Statistical Association member survey
  included questions regarding a proposed certification program. This
  data set contains two objects:
  \describe{
    \item{certify}{the 5001 survey responses for questions related to the
      certification program.}
    \item{statpoptotal}{population characteristics of the ASA of
      1994, provided for Rake analysis.}
  }
}
\usage{data(certify)}

\format{
  \describe{
    \item{certify}{data.frame with 11 columns:}
    \describe{
      \item{CERTIFY}{Should the ASA develop some form of certification?}
      \item{APPROVE}{Would you approve of a certification program
	similar to that described in the July 1993 issue of
	\emph{Amstat News}?}
      \item{SPECCERT}{Should there be specific certification programs
	for statistics subdisciplines?}
      \item{WOULDYOU}{If the ASA developed a certification program,
	would you attempt to become certified?}
      \item{SUBDISC}{Major subdiscipline: BA = Bayesian, BE = business &
	economic, BI = biometrics, BP = biopharmaceutical, CM =
	computing, EN  = environment, EP = epidemiology, GV =
	government, MR = marketing, PE = physical & engineering
	sciences, QP = quality & productivity, SE = statistical
	education, SG = statistical graphics, SP = sports, SR = survey
	research, SS = social statistics, TH = teaching statistics in
	health sciences, O = other.}
      \item{COLLEGE}{Highest collegiate degree: B = BS or BA, M = MS, N
	= none, P = Ph.D., O = other.}
      \item{EMPLOY}{Employment status: E = employed, I = in school, R =
	retired, S = self-employed, U = unemployed, O = other.}
      \item{WORKENV}{Primary work environment: A = academia, G =
	government, I = Industry, O = other.}
      \item{WORKACT}{Primary work activity: C = consultant, E =
	educator, P = practitioner, R = researcher, S = student, O =
	other.}
      \item{YEARSMEM}{For how many years have you been a member of the ASA?}
    }
    (For the first
    five columns, the response is coded: 0 = no response, 1 = yes, 2 =
    possibly, 3 = no opinion, 4 = unlikely, 5 = no. Missing values for
    other questions are coded as blanks.)
    \item{statpoptotal}{data.frame with 2 columns:}
    \describe{
      \item{name}{label for population characteristic. An element from
	the "WORKENV" or "COLLEGE" column of \code{certify}, but with the
	following meanings: A = Academia, I = Industry, O = All others, P
	= Ph.D., N = All others.}
      \item{weight}{number of members of the 1994 ASA with the
	characteristic (i.e. the population weights.)}
    }
  }
}
  

\note{
  This data set is provided as an example for using the \code{rake}
  package. It exhibits categorical data for which the sample does
  not match the population, and thus is a suitable target for
  re-weighting using Raking.
}

\references{Sharon L. Lohr. Sampling: Design and Analysis,
p. 439. Brooks/Cole, Pacific Grove, CA, 1999.}

\source{\url{http://lib.stat.cmu.edu/asacert/certsurvey}}