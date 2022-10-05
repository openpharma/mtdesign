#' @useDynLib mtdesign
#' @importFrom Rcpp sourceCpp

.onLoad <- function(libname, pkgname) {
  logger::log_layout(
    logger::layout_glue_generator(format = "{namespace} {time} {level} {fn}: {msg}"),
    namespace = "mtdesign"
  )
}

.onUnload <- function(libpath) {
  library.dynam.unload("mtdesign", libpath)
}

isBasicGrid <- function(grid) {
  logger::log_debug("Entry")
  if (!methods::is(grid, "data.frame")) {
    return(FALSE)
  }
  columnsRequired <- c("p0", "p1", "nStage1", "nTotal", "rFutility", "rTotal")
  rv <- length(intersect(names(grid), columnsRequired)) == length(columnsRequired)
  logger::log_debug("Exit")
  return(rv)
}

isManderGrid <- function(grid) {
  logger::log_debug("Entry")
  columnsRequired <- c("rSuccess")
  rv <- isBasicGrid(grid) & length(intersect(names(grid), columnsRequired)) == length(columnsRequired)
  logger::log_debug("Exit")
  return(rv)
}

isAugmented <- function(grid) {
  logger::log_debug("Entry")
  if (!isBasicGrid(grid)) {
    return(NA)
  }
  columnsRequired <- c(
    "Type1", "Type2", "PETNull", "PETAlt", "AveSizeNull",
    "AveSizeAlt"
  )
  logger::log_debug("Exit")
  rv <- length(intersect(names(grid), columnsRequired)) == length(columnsRequired)
  return(rv)
}

#' Create a grid of candidate designs
#'
#' @param p0 the response rate under the null hypothesis
#' @param p1 the response rate under the alternate hypothesis
#' @param alpha the desired (one-sided) type 1 error rate
#' @param beta the desired type 2 error rate
#' @param power an alternative to \code{beta}
#' @param nMin the lower bound for the search grid.  If \code{NA},
#' \code{searchBounds} is called to provide an appropriate value
#' @param nMax the lower bound for the search grid.  If \code{NA},
#' \code{searchBounds} is called to provide an appropriate value
#' @param mander is a Mander & Thompson or a Simon's design required?
#' @return a tibble.  See Usage notes for a list and description of columns.
#' @examples
#' # Standard use for a Simon's 2-stage design
#' x <- createGrid(p0 = 0.1, p1 = 0.5, alpha = 0.1, beta = 0.1, mander = FALSE)
#' # Custom search bounds for a Mander & Thompson design
#' y <- createGrid(p0 = 0.1, p1 = 0.4, alpha = 0.1, beta = 0.1, nMin = 20, nMax = 30)
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export
createGrid <- function(p0,
                       p1,
                       alpha = 0.1,
                       beta = NA,
                       power = ifelse(is.na(beta), 0.9, 1 - beta),
                       nMin = NA,
                       nMax = NA,
                       mander = TRUE) {
  logger::log_debug("Entry")
  # Validation
  if (is.na(p0)) stop("You must provide a value for p0")
  if (is.na(p1)) stop("You must provide a value for p1")
  if (p0 < 0 || p0 > 1) stop("p0 must be between 0 and 1")
  if (p1 < 0 || p1 > 1) stop("p1 must be between 0 and 1")
  if (p1 <= p0) stop("p1 must be strictly greater than p0")
  if (alpha < 0 || alpha > 1) stop("alpha must be between 0 and 1")
  if (!is.na(power)) {
    if (power < 0 || power > 1) {
      stop("power must be between 0 and 1")
    }
  }
  if (!is.na(beta)) {
    if (beta < 0 || beta > 1) {
      stop("beta must be between 0 and 1")
    }
  }
  if (!is.na(power) & !is.na(beta) & !isTRUE(all.equal(beta, (1 - power)))) {
    stop("Inconsistent values for beta and power")
  }
  if (is.na(power) & is.na(beta)) {
    stop("Both beta and power are null.  At least one must be not null.")
  }

  # Initialise
  if (is.na(beta)) beta <- 1 - power
  if (is.na(nMin) | is.na(nMax)) {
    bounds <- searchBounds(p0, p1, alpha, beta, twoSided = FALSE)
  }
  if (is.na(nMin)) {
    nMin <- bounds["min"]
    logger::log_debug(paste0("Using default value for nMin: ", nMin))
  }
  if (is.na(nMax)) {
    nMax <- bounds["max"]
    logger::log_debug(paste0("Using default value for nMax: ", nMax))
  }
  logger::log_debug(
    paste0(
      "One stage sample size is ",
      bounds["n"],
      ".  Search bounds are ",
      nMin,
      " to ",
      nMax,
      "."
    )
  )
  if (nMax <= nMin) stop("nMax must be strictly greater than nMin.")

  # Begin
  nTotal <- nMin:nMax
  nStage1 <- 1:(nMax - 1)
  rTotal <- 0:(nMax - 1)
  rFutility <- 0:(nMax - 1)
  if (mander) {
    rSuccess <- 0:nMax
  } else {
    rSuccess <- nMax
  }
  # It's easy to come up with a grid with over 1.5 billion combinations using
  # a brute-force solution starting with
  # d <- tibble::tibble() %>%
  #        dplyr::expand(nTotal, nStage1, rTotal, rFutility, rSuccess)
  # so build up and filter in stages.
  d <- tibble::tibble() %>% tidyr::expand(nTotal, nStage1)
  d <- d %>% dplyr::filter(nTotal >= nMin, nStage1 < nTotal)
  logger::log_trace(paste0("Building grid - nTotal, nStage1: ", nrow(d)))

  d <- d %>% tidyr::expand(tidyr::nesting(nTotal, nStage1), rTotal)
  d <- d %>% dplyr::filter(rTotal < nTotal)
  logger::log_trace(paste0("Building grid - nTotal, nStage1, rTotal: ", nrow(d)))

  d <- d %>% tidyr::expand(tidyr::nesting(nTotal, nStage1, rTotal), rFutility)
  d <- d %>% dplyr::filter(
    rFutility < nTotal,
    rFutility < rTotal,
    rFutility < nStage1,
    (rTotal - rFutility) < (nTotal - nStage1)
  )
  logger::log_trace(
    paste0(
      "Building grid - nTotal, nStage1, rTotal, rFutility: ",
      nrow(d)
    )
  )

  d <- d %>% tidyr::expand(tidyr::nesting(nTotal, nStage1, rTotal, rFutility), rSuccess)

  if (mander) {
    d <- d %>%
      dplyr::filter(
        rSuccess < nTotal,
        rSuccess <= rTotal,
        rSuccess > rFutility,
        rSuccess <= nStage1
      )
    logger::log_trace(
      paste0(
        "Building grid - nTotal, nStage1, rTotal, rFutility, rSuccess: ",
        nrow(d)
      )
    )
  }
  d <- d %>%
    dplyr::mutate(
      p0 = p0,
      p1 = p1,
      Alpha = alpha,
      Beta = beta
    )
  if (!mander) {
    d <- d %>% dplyr::select(-rSuccess)
  }
  logger::log_trace(paste0("Grid has ", nrow(d), " rows."))
  logger::log_debug("Exit")
  return(d)
}

#' Obtain default bounds for the construction of the search grid.
#'
#' The formula used is the continuity corrected Normal approximation from
#' Fleiss et al (2003).
#' @param p0 the response rate under the null hypothesis
#' @param p1 the response rate under the alternate hypothesis
#' @param alpha the desired (one-sided) type 1 error rate
#' @param beta the desired type 2 error rate
#' @param twoSided two- or one-sided significance level?
#' @return a list with three elements: "n" - the single stage sample size from
#' Fleiss et al; "min" - the lower bound, 0.8*n; "max" - the upper bound, 2*n.
#' \code{floor()} and \code{ceiling()} are applied as appropriate.
#' @export
searchBounds <- function(p0, p1, alpha = 0.05, beta = 0.2, twoSided = TRUE) {
  logger::log_debug("Entry")
  if (twoSided) alpha <- alpha / 2

  # Sample size formula based on Fleiss JL, Levin B and Paik MC (2003).
  # Statistical Methods for Rates and Proportions, Third Edition,
  # John Wiley & Sons, New York
  n <- ((stats::qnorm(1 - alpha) * sqrt(p0 * (1 - p0)) +
    stats::qnorm(1 - beta) * sqrt(p1 * (1 - p1))) /
    (p0 - p1))^2
  # Continuity correction from Fleiss et al
  n <- n + 1 / abs(p0 - p1)
  # Bounds based on ????
  rv <- c("n" = ceiling(n), "min" = floor(n * 0.8), "max" = ceiling(n * 2))
  logger::log_debug("Exit")
  return(rv)
}

#' Augment a grid of candidate designs with type 1 and type 2 error
#' probabilities, expected sample sizes and probabilities of early termination
#' @param d a tibble created by `createGrid`
#' @param parallel use parallelisation if available
#' @param cores the number of cores to use when parallelising.  If
#' <code>NA</code>, all available cores are requested
#' @param minChunkSize  The minimum size of the grid before paralellisation is
#' attempted
#' @return an augmented grid tibble
#' @section Usage Notes:
#' Regardless of the value of `parallel`, parallelisation is only
#' used if the size of the grid is greater than <code>chunkSize</code>.  If
#' paralellisation is requested and needed, an exception is thrown if the
#' parallel package is not available.
#' @examples
#' x <- createGrid(p0 = 0.1, p1 = 0.30, alpha = 0.1, beta = 0.1, nMin = 24, nMax = 32) %>%
#'   augmentGrid(parallel = FALSE)
#' @export
augmentGrid <- function(d, parallel = TRUE, cores = NA, minChunkSize = 100000) {
  logger::log_debug("Entry")
  k <- d %>% nrow()
  if (parallel) {
    if (k < minChunkSize) {
      logger::log_info(
        paste0(
          "Parallelisation has been requested, but the grid size [", k,
          "] is less than the minimum chunk size [", minChunkSize,
          "].  Parallelisation will not occur"
        ),
        namespace = "mtdesign"
      )
      parallel <- FALSE
    }
  }
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Parallelisation has been requested but the parallel package is not available.  Unable to continue.")
    }
    if (is.na(cores)) {
      cores <- parallel::detectCores()
    }
    logger::log_trace("Starting parallelisation")
    logger::log_trace(paste0("Requesting ", cores, " cores"))
    logger::log_trace(paste0("k is ", k))
    chunkSize <- ceiling(k / cores)
    logger::log_trace(paste0("Creating chunk list.  Chunk size is ", chunkSize))
    tmp <- d %>% dplyr::mutate(Chunk = ceiling(dplyr::row_number() / chunkSize))
    parallelList <- tmp %>%
      dplyr::group_by(Chunk) %>%
      dplyr::group_map(function(.x, .y) .x)
    logger::log_trace("Creating cluster")
    cluster <- parallel::makeCluster(cores)
    logger::log_trace("Initialising nodes")
    parallel::clusterEvalQ(cluster, {
      library(parallel)
    })
    logger::log_trace("Running parLapply")
    d <- parallel::parLapply(
      cluster,
      parallelList,
      augmentGrid,
      parallel = FALSE
    ) %>%
      dplyr::bind_rows()
    logger::log_trace("Stopping cluster")
    suppressWarnings(parallel::stopCluster(cluster))
  } else {
    if (k >= 1e6) {
      logger::log_info(
        paste0(
          "The grid contains ",
          k,
          " rows.  This may take some time..."
        )
      )
    }
    # The return type of a data frame from Rcpp is unreliable, so wrap it here
    # to make sure all is good
    cls <- class(d)
    d <- tibble::as_tibble(augmentGridC(d))
    class(d) <- cls
  }
  # Bug fix
  if (isManderGrid(d)) {
    d <- d %>% dplyr::mutate(rSuccess = as.integer(rSuccess))
  }
  logger::log_debug("Exit")
  return(d)
}


#' Finds optimal and minimax designs for either Simon's 2-stage or Mander &
#' Thompson studies
#'
#' \code{obtainDesign} is essentially a wrapper for calls to
#' \code{\link{createGrid}} and \code{\link{augmentGrid}} followed by some
#' simple filtering of the candidate designs to identify the optimal and
#' minimax designs.
#'
#' @param grid Optional. A tibble created by \code{createGrid}.  If \code{NULL},
#'  then \code{p0}, \code{p1}, \code{alpha} and \code{beta} must be specified
#'  and \code{createGrid} is called to generate the required grid.  If not
#'  \code{NULL} then \code{p0}, \code{p1}, \code{alpha} and \code{beta} are
#'  ignored
#' @param p0 the response rate under the null hypothesis
#' @param p1 the response rate under the alternate hypothesis
#' @param alpha the desired (one-sided) type 1 error rate
#' @param beta the desired type 2 error rate
#' @param fullGrid should the full grid of all possible designs be returned, or
#'  simply the optimal and minimax solutions?  For a Mander and Thompson design,
#'  optimal and minimax designs are returned for both the null and alternate
#'  hypotheses.  See Usage Notes below.
#' @param ... passed to `createGrid` or `augmentGrid`.  In particular
#'  \code{mander=TRUE} for a Mander & Thompson design or \code{mander=FALSE} for
#'  a Simon's 2-stage design.
#' @return a tibble created by \code{\link{createGrid}}.  If
#'  \code{fullGrid == FALSE} the table contains an additional column,
#'  \code{Criterion} indicating the type of design.  Possible values for
#'  \code{Criterion} are "optimal" and "minimax" for Simon's designs and
#'  "optimalNull", "optimalAlt", "minimaxNull" and "minimaxAlt" for Mander &
#'  Thompson designs.
#' @section Usage notes:
#' If \code{grid} is not \code{NULL} it is possible that none of the candidate
#' designs are acceptable (that is, satisfy both the significance level and
#' power requirements).  If this is the case and \code{fullGrid == FALSE}, then
#' an empty tibble is returned.  If \code{versbose == TRUE} a warning message is
#'  also printed.
#' If \code{fullGrid == TRUE} the full grid of all designs considered is
#' returned.  This can then be further interrogated to find optimal designs
#'  under constraints - for example with fixed stage sizes.
#' @examples
#' \donttest{
#' # Standard use (Simon's 2-stage design)
#' createGrid(p0 = 0.05, p1 = 0.25, alpha = 0.05, beta = 0.2, mander = FALSE) %>%
#'   augmentGrid(parallel = FALSE) %>%
#'   obtainDesign()
#' # Constrained stage sizes
#' createGrid(p0 = 0.25, p1 = 0.45, alpha = 0.05, beta = 0.2) %>%
#'   dplyr::filter(nStage1 == 8) %>%
#'   augmentGrid(parallel = FALSE) %>%
#'   obtainDesign()
#' }
#' @export
obtainDesign <- function(grid = NULL,
                         p0 = NA,
                         p1 = NA,
                         alpha = ifelse(is.null(grid), 0.05, NA),
                         beta = ifelse(is.null(grid), 0.1, NA),
                         fullGrid = FALSE,
                         ...) {
  logger::log_debug("Entry")
  # Initialise
  dots <- list(...)
  # Validate
  if (is.null(grid)) {
    if (is.na(p0) | is.na(p1) | is.na(alpha) | is.na(beta)) {
      stop("If you do not supply a grid, then you must supply all of p0, p1, alpha and beta.")
    }
    if (p0 <= 0 | p0 >= 1) stop("p0 must be between 0 and 1")
    if (p1 <= 0 | p1 >= 1) stop("p1 must be between 0 and 1")
    if (alpha <= 0 | alpha >= 1) stop("alpha must be between 0 and 1")
    if (beta <= 0 | beta >= 1) stop("beta must be between 0 and 1")
  } else {
    if (!(grid %>% isBasicGrid())) {
      stop("Grid must be a tibble created by createGrid()")
    }
    if (!is.na(p0)) {
      logger::log_warn("A valid grid has been supplied.  Ignoring p0...")
    }
    if (!is.na(p1)) {
      logger::log_warn("A valid grid has been supplied.  Ignoring p1...")
    }
    if (!is.na(alpha)) {
      logger::log_warn("A valid grid has been supplied.  Ignoring alpha...")
    }
    if (!is.na(beta)) {
      logger::log_warn("A valid grid has been supplied.  Ignoring beta...")
    }
  }

  # Begin
  # Calls to do.call are required to separate the dot list appropriately
  # Function name as string as workaround for https://github.com/daroczig/logger/issues/114
  if (is.null(grid)) {
    grid <-
      do.call(
        "createGrid",
        c(
          list(p0 = p0, p1 = p1, alpha = alpha, beta = beta),
          dots[names(dots) %in% names(formals(mtdesign::createGrid))]
        )
      )
  }
  if (!(grid %>% isAugmented())) {
    grid <-
      do.call(
        "augmentGrid",
        c(
          list(d = grid),
          dots[names(dots) %in% names(formals(mtdesign::augmentGrid))]
        )
      )
  }
  if (fullGrid) {
    return(grid)
  } else {
    acceptableGrid <- grid %>%
      dplyr::filter(
        Type1 <= Alpha,
        Type2 <= Beta
      )
    if (nrow(acceptableGrid) == 0) {
      rlang::warn("No acceptable designs were found.")
      logger::log_warn("No acceptable designs were found.")
    }

    rv <- vector("list", 4)
    rv[[1]] <- acceptableGrid %>%
      dplyr::slice(which.min(AveSizeNull)) %>%
      dplyr::mutate(
        Criterion = ifelse(
          acceptableGrid %>% isManderGrid(),
          "optimalNull",
          "optimal"
        )
      )
    rv[[2]] <- acceptableGrid %>%
      dplyr::slice_min(nTotal) %>%
      dplyr::slice_min(AveSizeNull) %>%
      dplyr::mutate(
        Criterion = ifelse(
          acceptableGrid %>% isManderGrid(),
          "minimaxNull",
          "minimax"
        )
      )
    if (isManderGrid(grid)) {
      rv[[3]] <- acceptableGrid %>%
        dplyr::slice_min(AveSizeAlt) %>%
        dplyr::mutate(Criterion = "optimalAlt")
      rv[[4]] <- acceptableGrid %>%
        dplyr::slice_min(nTotal) %>%
        dplyr::slice_min(AveSizeAlt) %>%
        dplyr::mutate(Criterion = "minimaxAlt")
    }
    rv <- dplyr::bind_rows(rv)
    class(rv) <- class(grid)
    logger::log_debug("Exit")
    return(rv)
  }
}

#' Plot the power curve(s) for the given design(s)
#'
#' @param grid the tibble containing the designs to be
#'  plotted
#' @param probs the response rates for which the rejection probabilities are to
#' be plotted
#' @return the ggplot object containing the power curve(s)
#' @examples
#' createGrid(p0 = 0.05, p1 = 0.25, alpha = 0.05, beta = 0.2, mander = FALSE) %>%
#'   augmentGrid(cores = 2) %>%
#'   obtainDesign() %>%
#'   powerPlot(probs = seq(0, 0.5, 0.025))
#' @export
powerPlot <- function(grid, probs = seq(0, 1, 0.01)) {
  logger::log_debug("Entry")
  if (is.null(grid)) stop("grid cannot be null")
  if (!isBasicGrid(grid)) stop("Grid must be a tibble created by createGrid()")

  if (isManderGrid(grid)) {
    plotData <- grid %>%
      dplyr::mutate(Design = c(seq_len(nrow(grid)))) %>%
      dplyr::group_by(Design) %>%
      dplyr::mutate(
        Label = paste0(
          "(",
          rFutility,
          " ",
          rSuccess,
          ")/",
          nStage1,
          " ",
          rTotal,
          "/",
          nTotal
        )
      ) %>%
      dplyr::select(
        Design,
        Label,
        nStage1,
        rFutility,
        rSuccess,
        nTotal,
        rTotal,
        Alpha,
        Beta,
        p0,
        p1
      ) %>%
      tidyr::expand(
        tidyr::nesting(
          Design,
          Label,
          nStage1,
          rFutility,
          rSuccess,
          nTotal,
          rTotal,
          Alpha,
          Beta,
          p0,
          p1
        ),
        pResponse = probs
      ) %>%
      dplyr::group_by(Design, pResponse) %>%
      dplyr::mutate(
        pReject = 1 - manderProb(pResponse, nStage1, rFutility, rSuccess, nTotal, rTotal)
      ) %>%
      dplyr::ungroup()
  } else {
    plotData <- grid %>%
      dplyr::mutate(Design = seq_len(nrow(grid))) %>%
      dplyr::group_by(Design) %>%
      dplyr::mutate(
        Label = paste0(
          rFutility,
          "/",
          nStage1,
          " ",
          rTotal,
          "/",
          nTotal
        )
      ) %>%
      dplyr::select(
        Design,
        Label,
        nStage1,
        rFutility,
        nTotal,
        rTotal,
        Alpha,
        Beta,
        p0,
        p1
      ) %>%
      tidyr::expand(
        tidyr::nesting(
          Design,
          Label,
          nStage1,
          rFutility,
          nTotal,
          rTotal,
          Alpha,
          Beta,
          p0,
          p1
        ),
        pResponse = probs
      ) %>%
      dplyr::group_by(Design, pResponse) %>%
      dplyr::mutate(
        pReject = 1 - simonProb(pResponse, nStage1, rFutility, nTotal, rTotal)
      ) %>%
      dplyr::ungroup()
  }
  plot <- plotData %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x = pResponse,
        y = pReject,
        colour = Label
      )
    ) +
    ggplot2::labs(
      x = "True response rate",
      y = "p(Signal detected)"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  logger::log_debug("Exit")
  return(plot)
}
