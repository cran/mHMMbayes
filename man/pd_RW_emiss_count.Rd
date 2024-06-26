% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pd_RW_emiss_count.R
\name{pd_RW_emiss_count}
\alias{pd_RW_emiss_count}
\title{Proposal distribution settings RW Metropolis sampler for mHMM
Poisson-lognormal emission distribution(s)}
\usage{
pd_RW_emiss_count(gen, emiss_scalar, emiss_w)
}
\arguments{
\item{gen}{List containing the following elements denoting the general model
properties:
\itemize{
\item{\code{m}: numeric vector with length 1 denoting the number
of hidden states}
\item{\code{n_dep}: numeric vector with length 1 denoting the
number of dependent variables}
\item{\code{q_emiss}: only to be specified if the data represents
categorical data. Numeric vector with length \code{n_dep} denoting the
number of observed categories for the categorical emission distribution
for each of the dependent variables.}}}

\item{emiss_scalar}{A list containing \code{n_dep} elements
corresponding to each of the dependent variables, where each element is a
numeric vector with length 1 denoting the scale factor \code{s}. That is,
the scale of the proposal distribution is composed of a covariance matrix
Sigma, which is then tuned by multiplying it by a scaling factor \code{s}^2.}

\item{emiss_w}{A list containing \code{n_dep} elements corresponding
to each of the dependent variables, where each element is a numeric vector
with length 1 denoting the weight for the overall log likelihood (i.e., log
likelihood based on the pooled data over all subjects) in the fractional
likelihood.}
}
\value{
\code{pd_RW_emiss_count} returns an object of class
  \code{mHMM_pdRW_emiss}, containing settings of the proposal distribution of
  the random walk (RW) Metropolis sampler on the categorical emission
  distribution(s) of the multilevel hidden Markov model. The object is
  specifically created and formatted for use by the function \code{mHMM}, and
  checked for correct input dimensions. The object contains the following
  components:
   \describe{
  \item{\code{gen}}{A list containing the elements \code{m} and
  \code{n_dep}, used for checking equivalent general model properties
  specified under \code{pd_RW_emiss_count} and \code{mHMM}.}
  \item{\code{emiss_scalar}}{A list containing \code{n_dep} elements denoting
  the scale factor \code{s} of the proposal distribution.}
  \item{\code{emiss_w}}{A list containing \code{n_dep} elements denoting
  denoting the weight for the overall log likelihood in the fractional
  likelihood.}
  }
}
\description{
\code{pd_RW_emiss_count} provides a framework to manually specify the
settings of the proposal distribution of the random walk (RW) Metropolis
sampler of Poisson emission distribution(s) of the multilevel hidden Markov
model, and creates on object of the class \code{mHMM_pdRW_emiss}. The RW
metropolis sampler is used for sampling the subject level parameter estimates
relating to the emission distributions of the dependent variables \code{k},
that is, the Poisson parameters lambda.
}
\details{
When no manual values for the settings of the proposal distribution of the
random walk (RW) Metropolis sampler are specified at all (that is, the
function \code{pd_RW_emiss_count} is not used), \code{emiss_scalar} set
to 2.38, and \code{emiss_w} set to 0.1. See the section \emph{Scaling the
proposal distribution of the RW Metropolis sampler} in
\code{vignette("estimation-mhmm")} for details.

Within the function \code{mHMM}, the acceptance rate of the RW metropolis
sampler relating to the emission distribution(s) can be tracked using the
output parameter \code{emiss_naccept}. An acceptance rate of about 45\% is
considered optimal when a single parameter is being updated (Gelman,
Carlin, Stern & Rubin, 2014).
}
\examples{
###### Example using simulated data
# specifying general model properties:
n_t     <- 200     # Number of observations on the dependent variable
m       <- 3        # Number of hidden states
n_dep   <- 2        # Number of dependent variables
n_subj  <- 30        # Number of subjects

gamma   <- matrix(c(0.9, 0.05, 0.05,
                    0.2, 0.7, 0.1,
                    0.2,0.3, 0.5), ncol = m, byrow = TRUE)

emiss_distr <- list(matrix(c(20,
                             10,
                             5), nrow = m, byrow = TRUE),
                    matrix(c(50,
                             3,
                             20), nrow = m, byrow = TRUE))

# Define between subject variance to use on the simulating function:
# here, the variance is varied over states within the dependent variable.
var_emiss <- list(matrix(c(5.0, 3.0, 1.5), nrow = m),
                  matrix(c(5.0, 5.0, 5.0), nrow = m))

# Simulate count data:
data_count <- sim_mHMM(n_t = n_t,
                       n = n_subj,
                       data_distr = "count",
                       gen = list(m = m, n_dep = n_dep),
                       gamma = gamma,
                       emiss_distr = emiss_distr,
                       var_gamma = 0.1,
                       var_emiss = var_emiss,
                       return_ind_par = TRUE)


# Transition probabilities
start_gamma <- diag(0.8, m)
start_gamma[lower.tri(start_gamma) | upper.tri(start_gamma)] <- (1 - diag(start_gamma)) / (m - 1)

# Emission distribution
start_emiss <- list(matrix(c(20,10, 5), nrow = m, byrow = TRUE),
                    matrix(c(50, 3,20), nrow = m, byrow = TRUE))

# Specify hyper-prior for the count emission distribution
manual_prior_emiss <- prior_emiss_count(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(20, 10, 5), byrow = TRUE, ncol = m),
                   matrix(c(50, 3, 20), byrow = TRUE, ncol = m)),
  emiss_K0  = rep(list(0.1),n_dep),
  emiss_nu  = rep(list(0.1),n_dep),
  emiss_V   = rep(list(rep(10, m)),n_dep)
)

# Specify the desired values for the sampler
manual_emiss_sampler <- pd_RW_emiss_count(gen = list(m = m, n_dep = n_dep),
                                        emiss_scalar = rep(list(2.38),n_dep),
                                        emiss_w = rep(list(0.1), n_dep))

# Run model
# Note that for reasons of running time, J is set at a ridiculous low value.
# One would typically use a number of iterations J of at least 1000,
# and a burn_in of 200.
out_3st_count_RWemiss <- mHMM(s_data = data_count$obs,
                          data_distr = 'count',
                          gen = list(m = m, n_dep = n_dep),
                          start_val = c(list(start_gamma), start_emiss),
                          emiss_hyp_prior = manual_prior_emiss,
                          emiss_sampler = manual_emiss_sampler,
                          mcmc = list(J = 11, burn_in = 5),
                          show_progress = TRUE)

# Examine acceptance rates over dependent variable, individual, and states:
lapply(out_3st_count_RWemiss$emiss_naccept, function(e) e/out_3st_count_RWemiss$input$J)

# Finally, take the average acceptance rate by dependent variable and state:
lapply(out_3st_count_RWemiss$emiss_naccept, function(e) colMeans(e/out_3st_count_RWemiss$input$J))


}
\references{
\insertRef{gelman2014}{mHMMbayes}

\insertRef{rossi2012}{mHMMbayes}
}
