% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{loadd}
\alias{loadd}
\title{Load multiple targets or imports from the drake cache.}
\usage{
loadd(..., list = character(0), imported_only = FALSE, path = getwd(),
  search = TRUE, cache = drake::get_cache(path = path, search = search,
  verbose = verbose), namespace = NULL, envir = parent.frame(), jobs = 1,
  verbose = 1, deps = FALSE, lazy = FALSE, graph = NULL,
  replace = TRUE)
}
\arguments{
\item{...}{targets to load from the cache, as names (unquoted)
or character strings (quoted). Similar to \code{...} in
\code{\link{remove}(...)}.}

\item{list}{character vector naming targets to be loaded from the
cache. Similar to the \code{list} argument of \code{\link[=remove]{remove()}}.}

\item{imported_only}{logical, whether only imported objects
should be loaded.}

\item{path}{Root directory of the drake project,
or if \code{search} is \code{TRUE}, either the
project root or a subdirectory of the project.}

\item{search}{logical. If \code{TRUE}, search parent directories
to find the nearest drake cache. Otherwise, look in the
current working directory only.}

\item{cache}{optional drake cache. See code{\link{new_cache}()}.
If \code{cache} is supplied,
the \code{path} and \code{search} arguments are ignored.}

\item{namespace}{character scalar,
name of an optional storr namespace to load from.}

\item{envir}{environment to load objects into. Defaults to the
calling environment (current workspace).}

\item{jobs}{number of parallel jobs for loading objects. On
non-Windows systems, the loading process for multiple objects
can be lightly parallelized via \code{parallel::mclapply()}.
just set jobs to be an integer greater than 1. On Windows,
\code{jobs} is automatically demoted to 1.}

\item{verbose}{logical, whether to print console messages}

\item{deps}{logical, whether to load any cached
dependencies of the targets
instead of the targets themselves.
This is useful if you know your
target failed and you want to debug the command in an interactive
session with the dependencies in your workspace.
One caveat: to find the dependencies,
\code{\link[=loadd]{loadd()}} uses information that was stored
in a \code{\link[=drake_config]{drake_config()}} list and cached
during the last \code{\link[=make]{make()}}.
That means you need to have already called \code{\link[=make]{make()}}
if you set \code{deps} to \code{TRUE}.}

\item{lazy}{logical, whether to lazy load with
\code{\link[=delayedAssign]{delayedAssign()}} rather than the more eager
\code{\link[=assign]{assign()}}.}

\item{graph}{optional igraph object, representation
of the workflow network for getting dependencies
if \code{deps} is \code{TRUE}. If none is supplied,
it will be read from the cache.}

\item{replace}{logical. If \code{FALSE},
items already in your environment
will not be replaced.}
}
\value{
\code{NULL}
}
\description{
Loads the object(s) into the
current workspace (or \code{envir} if given). Defaults
to loading the whole cache if arguments \code{...}
and \code{list} are not set
(or all the imported objects if in addition
imported_only is \code{TRUE}).
}
\examples{
\dontrun{
test_with_dir("Quarantine side effects.", {
load_basic_example() # Get the code with drake_example("basic").
make(my_plan) # Run the projects, build the targets.
loadd(small) # Load target 'small' into your workspace.
small
# For many targets, you can parallelize loadd()
# using the 'jobs' argument.
loadd(list = c("small", "large"), jobs = 2)
# Load the dependencies of the target, coef_regression2_small
loadd(coef_regression2_small, deps = TRUE)
# Load all the imported objects/functions.
loadd(imported_only = TRUE)
# Load everything, including built targets.
# Be sure your computer has enough memory.
loadd()
})
}
}
\seealso{
\code{\link[=cached]{cached()}}, \code{\link[=built]{built()}},
\code{\link[=imported]{imported()}}, \code{\link[=drake_plan]{drake_plan()}}, \code{\link[=make]{make()}},
}
