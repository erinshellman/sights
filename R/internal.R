## @title Processor
##
## @description Data processing and error checks for all SIGHTS functions
##
## @param m A numeric matrix.
## @param type Type of function.
## @param mr,mc Matrix rows and cols.
## @param pr,pc Plate rows and cols.
## @param bm Bias matrix.
## @param bc Bias cols.
## @param ri Replicate index.
## @param cn Sample names.
## @param tf Trim factor.
## @param ts Test side.
## @param cm Control method.
##
## @details Checks for all errors and converts the data matrices into the required formats. It provides useful error messages.
##
## @keywords internal
## @return Numeric matrix
##
sightsCheck <- function(m, type, mr = NULL, mc = NULL, pr = NULL, pc = NULL, bm = NULL, bc = NULL, ri = NULL,
    cn = NULL, tf = NULL, ts = NULL, cm = NULL) {
    if (is.null(ncol(m))) {
        m <- matrix(m, ncol = 1, dimnames = dimnames(m))
    }
    if (!is.null(mr)) {
        if (is.vector(mr) & all(mr <= nrow(m))) {
            m <- m[mr, , drop = FALSE]
        } else {
            stop(paste(type, "Rows exceed the number of plate wells in the ", type, "Matrix \n  Please select well numbers between 1 and ",
                nrow(m), sep = ""))
        }
    }
    if (!is.null(pr) & !is.null(pc)) {
        if (pr * pc != nrow(m)) {
            stop(paste(type, "Rows (", nrow(m), ") is not equal to the product of plateRows (", pr, ") and plateCols (",
                pc, ")", sep = ""))
        }
    }
    if (!is.null(mc)) {
        if (is.vector(mc) & all(mc <= ncol(m))) {
            m <- m[, mc, drop = FALSE]
        } else {
            stop(paste(type, "Cols exceed the number of plates in the ", type, "Matrix \n  Please select plate numbers between 1 and ",
                ncol(m), sep = ""))
        }
    }
    if (!is.null(bm)) {
        if (nrow(bm) != nrow(m)) {
            stop("dataRows is not equal to the number of plate wells in the biasMatrix")
        }
    }
    if (!is.null(bc)) {
        if (!(is.vector(bc) & all(bc <= ncol(m)))) {
            stop("biasCols exceed the number of plates in the dataMatrix")
        }
    }
    if (is.vector(ri) & length(ri) != ncol(m)) {
        stop(paste(type, "Cols is not equal to the number of plates in the repIndex", sep = ""))
    } else if (!is.null(ri)) {
        if (!any(duplicated(ri))) {
            stop("Please provide at least two replicates per sample")
        } else if (length(unique(table(ri))) != 1) {
            stop("Please provide the same number of replicates per sample")
        }
    }
    if (is.vector(cn) & length(cn) != ncol(m)) {
        stop(paste(type, "Cols is not equal to the number of sample names in the colNames", sep = ""))
    }
    if (!is.null(tf)) {
        if (!is.numeric(tf) | tf < 0 | tf > 0.5) {
            stop("trimFactor is not a number between 0 and 0.5")
        }
    }
    if (!is.null(ts)) {
        if (!any(ts == c("less", "greater", "two.sided"))) {
            stop("testSide can only take one of these values: 'less', 'greater' or 'two.sided'")
        }
    }
    if (type == "fdr") {
        if (dim(m)[2]%%5 != 0) {
            stop("testMatrix should consist of 5 columns each for 1 sample, where the 5th column should contain p-values")
        }
        if (!(is.character(cm)) | length(cm) != 1) {
            stop(paste("Please provide a valid method name. Select one from", dQuote("smoother"), "or", dQuote("bootstrap"),
                ". Note: names are case-sensitive."))
        }
        if (cm != "smoother" & cm != "bootstrap") {
            stop(paste(dQuote(cm), "is not a valid method name. Select one from", dQuote("smoother"), "or",
                dQuote("bootstrap"), ". Note: names are case-sensitive."))
        }
    }
    if (type == "data") {
        if (is.data.frame(m) == FALSE) {
            m <- as.data.frame(m)
        }
    } else {
        # if plotData is a dataframe, turn it into a matrix
        if (is.data.frame(m))
            m <- as.matrix(m)

        # if plotData is a vector, turn it into a matrix with one column
        if (is.null(dim(m)[[2]])) {
            m <- matrix(m, ncol = 1)
        }
    }
    return(m)
}

##
## @title Z-score
##
## @description Calculates Z-score
##
## @param x A numeric matrix.
##
## @details Used in normZ.
##
## @keywords internal
## @return Numeric matrix
##
myZ <- function(x) {
    return((x - base::mean(x, na.rm = TRUE))/stats::sd(x, na.rm = TRUE))
}
##
## @title Robust Z-score
##
## @description Calculates robust Z-score
##
## @param x A numeric matrix.
##
## @details Used in normRobZ, normLoess and normMedFil.
##
## @keywords internal
## @return Numeric matrix
##
myRobZ <- function(x) {
    return((x - stats::median(x, na.rm = TRUE))/stats::mad(x, na.rm = TRUE))
}
##
## @title Loess score
##
## @description Calculates loess score
##
## @param plate A single plate.
## @param pr,pc Plate rows and cols.
##
## @details Used in normLoess.
##
## @keywords internal
## @return Numeric matrix
##
loessMe <- function(plate, pr, pc) {
    coll <- 1:pc
    roww <- 1:pr
    tmp <- matrix(plate, nrow = pr, ncol = pc, byrow = FALSE)
    rloess <- cloess <- NULL
    # get row loess fit
    for (i in 1:pr) {
        nas <- rep(NA, pc)
        isna <- is.na(tmp[i, ])
        naInd <- coll[!is.na(tmp[i, ])]
        x <- tmp[i, ]
        if (sum(isna) != length(x)) {
            tmp1 <- stats::loess(x ~ coll, family = "symmetric")$fitted
            nas[naInd] <- tmp1
        }
        rloess <- rbind(rloess, nas)
    }
    # get column loess fit
    for (i in 1:pc) {
        nas <- rep(NA, pr)
        isna <- is.na(tmp[, i])
        naInd <- roww[!is.na(tmp[, i])]
        x <- tmp[, i]
        if (sum(isna) != length(x)) {
            tmp1 <- stats::loess(x ~ roww, family = "symmetric")$fitted
            nas[naInd] <- tmp1
        }
        cloess <- cbind(cloess, nas)
    }
    # get row and column averages of smoothers
    rmeans <- apply(rloess, 1, base::mean, na.rm = TRUE)
    cmeans <- apply(cloess, 2, base::mean, na.rm = TRUE)
    # calculate normalized scores in inefficient fashion
    outMat <- matrix(NA, nrow = pr, ncol = pc)
    for (i in 1:pr) {
        for (j in 1:pc) {
            adj <- (rmeans[i]/rloess[i, j]) * (cmeans[j]/cloess[i, j])
            outMat[i, j] <- tmp[i, j] * adj
        }
    }
    return(c(outMat))
}
##
## @title MedFil score
##
## @description Calculates median filter score
##
## @param plate A single plate.
## @param buds Buddies.
##
## @details Used in normMedFil.
##
## @keywords internal
## @return Numeric matrix
##
applyMedFilter <- function(plate, buds) {

    grandMed <- stats::median(plate, na.rm = TRUE)
    outVec <- NULL
    for (i in 1:length(plate)) {
        hood <- buds[i, !is.na(buds[i, ])]
        medd <- stats::median(plate[hood], na.rm = TRUE)
        outVec <- c(outVec, plate[i] * (grandMed/medd))
    }
    return(outVec)
}
## @title Create Neighbourhood Matrix
##
## @description Creates neighbourhood list for matrix of data.
##
## @param nrows Number of rows in matrix.
## @param ncols  Number of columns in matrix.
## @param wind Window size. Defines max distance of neighbours. Value of 2 will create a 5x5 neighbourhood.
##
## @keywords internal
## @return Numeric matrix
##
## @details Assumes data will be sequentially ordered first by column, then row.  Each row defines neighbours for that well.  Index values define which data points are neighbours in sequential data.  The neighboorhood includes the middle well. Used in normMedFil.
##
createNeighbourhoodMatrix <- function(ncols = 12, nrows = 8, wind = 2) {
    # total number of wells in matrix
    numWells <- nrows * ncols
    # create wasteful output matrix for max neighbourhood
    outMat <- matrix(NA, nrow = numWells, ncol = numWells)
    # reference matrix to identify sequential index of each well in matrix
    indMat <- matrix(1:numWells, nrow = nrows, ncol = ncols)
    # keeps track of where we are in sequential vector of matrix
    seqWellInd <- 0
    # keeps track of maximum number of buddies a well can have
    maxBuddies <- 0
    # iterate through each well location and find neighbours
    for (coll in 1:ncols) {
        for (roww in 1:nrows) {
            # cat('Finding buddies for', roww, ' ', coll, '\n')
            seqWellInd <- seqWellInd + 1
            # vector to store neighbours
            buddies <- NULL
            # iterate through neighbourood , first by row, then column
            for (i in (roww - wind):(roww + wind)) {
                for (j in (coll - wind):(coll + wind)) {
                  # only keep valid neighbours (within range of matrix)
                  if ((i >= 1) & (i <= nrows) & (j >= 1) & (j <= ncols)) {
                    buddies <- c(buddies, indMat[i, j])
                  }
                }
            }
            # assign neighbours to storage matrix
            outMat[seqWellInd, 1:length(buddies)] <- sort(buddies)
            # check if number of neighbours exceeds max neighbours yet encountered
            if (length(buddies) > maxBuddies) {
                maxBuddies <- length(buddies)
            }
        }
    }
    # remove excess columns
    outMat <- outMat[, 1:maxBuddies]
    return(outMat)
}
## @title Create Sequential Neighbourhood Matrix
##
## @description Creates a row neighbourhood list for matrix of data.
##
## @param nrows Number of rows in matrix.
## @param ncols  Number of columns in matrix.
## @param wind Window size. Defines max distance of neighbours. Value of 2 will create a 5x5 neighbourhood.
##
## @keywords internal
## @return Numeric matrix
##
## @details Assumes data will be sequentially ordered first by column, then row.  Each row defines neighbours for that well.  Index values define which data points are neighbours in sequential data.  The neighboorhood includes the middle well. Used in normMedFil.
##
createSequentialNeighbourhoodMatrix <- function(ncols = 12, nrows = 8, wind = 2) {
    # createSequentialNeighbourhoodMatrix(namey='E://projects//HTS//Inglese//spatial//data//IngleseConNeighboursRowWindow3.txt',
    # ncols=40, nrows=32, wind=3) total number of wells in matrix
    numWells <- nrows * ncols
    # create wasteful output matrix for max neighbourhood
    outMat <- matrix(NA, nrow = numWells, ncol = numWells)
    # reference matrix to identify sequential index of each well in matrix
    indMat <- matrix(1:numWells, nrow = nrows, ncol = ncols)
    # keeps track of where we are in sequential vector of matrix
    seqWellInd <- 0
    # keeps track of maximum number of buddies a well can have
    maxBuddies <- 0
    # iterate through each well location and find neighbours
    for (coll in 1:ncols) {
        for (roww in 1:nrows) {
            # cat('Finding buddies for', roww, ' ', coll, '\n')
            seqWellInd <- seqWellInd + 1
            # vector to store neighbours
            buddies <- NULL
            # iterate through neighbourood by row
            for (j in (coll - wind):(coll + wind)) {
                # only keep valid neighbours (within range of matrix)
                if ((j >= 1) & (j <= ncols))
                  {
                    # cat(roww, ' ', j, '\n')
                    buddies <- c(buddies, indMat[roww, j])
                  }  # end if i
            }  # end for j
            # assign neighbours to storage matrix
            outMat[seqWellInd, 1:length(buddies)] <- sort(buddies)
            # check if number of neighbours exceeds max neighbours yet encountered
            if (length(buddies) > maxBuddies) {
                maxBuddies <- length(buddies)
            }
        }  # end for roww
    }  # end for coll
    # remove excess columns
    outMat <- outMat[, 1:maxBuddies]
    return(outMat)
}
## @title Median Normalization
## @description Calculates median normalized results
## @param x Data matrix input.
## @details Used in normR.
## @return Numeric matrix
## @keywords internal
##
medMe <- function(x) {
    return(x - stats::median(x, na.rm = TRUE))
}
##
## @title R-score
## @description Calculates robust regression score
## @param x A single plate.
## @param rows,cols Plate rows and cols.
## @keywords internal
## @return Numeric matrix
## @details Apply rlm model to x with autoregressive coefficient params rows and cols.  Normalize by dividing by variance estimate. Used in normR.
##
rlmRConly <- function(x, rows, cols) {
    tmp <- rep(NA, length(x))
    naInd <- is.na(x)
    ret <- MASS::rlm(x ~ rows + cols)
    # tmp <- ret$residuals
    tmp[!naInd] <- ret$residuals
    return(tmp/ret$s)
}
##
## @title SPAWN score
## @description Calculates spatial and well normalization score
## @param mat Entire data matrix.
## @param pr,pc Plate rows and cols.
## @param tf Trim factor.
## @details Used in normSPAWN.
## @keywords internal
## @return Numeric matrix
##
spawning <- function(mat, pr, pc, tf) {
    len <- dim(mat)[[2]]
    # calculate trimmed mean polish
    mt <- NULL
    for (i in 1:len) {
        tmp <- c(t(trimPolish(matrix(mat[, i], nrow = pr, ncol = pc, byrow = TRUE), trace.iter = FALSE, trim = tf)$residuals))
        mt <- cbind(mt, tmp/stats::mad(tmp, na.rm = TRUE))
    }
    return(mt)
}
## @title Trimmed polish
## @description Modified from median polish code of B-score
##
## @param x A numeric matrix.
## @param eps Tolerance for convergence. Should be a real number greater than 0.
## @param maxiter The maximum number of iterations.
## @param trace.iter Logical. Should progress in convergence be reported?
## @param na.rm Logical. Should missing values be removed?
## @param trim Trim value when applying trimmed mean.
##
## @details Used in normSPAWN.
##
## @keywords internal
## @return Numeric matrix
##
trimPolish <- function(x, eps = 0.01, maxiter = 10, trace.iter = FALSE, na.rm = TRUE, trim) {
    z <- as.matrix(x)
    nr <- nrow(z)
    nc <- ncol(z)
    t <- 0
    r <- numeric(nr)
    c <- numeric(nc)
    oldsum <- 0
    for (iter in 1:maxiter) {
        rdelta <- apply(z, 1, base::mean, na.rm = na.rm, trim)
        z <- z - matrix(rdelta, nrow = nr, ncol = nc, byrow = FALSE)
        r <- r + rdelta
        delta <- base::mean(c, na.rm = na.rm, trim)
        c <- c - delta
        t <- t + delta
        cdelta <- apply(z, 2, base::mean, na.rm = na.rm, trim)
        z <- z - matrix(cdelta, nrow = nr, ncol = nc, byrow = TRUE)
        c <- c + cdelta
        delta <- mean(r, na.rm = na.rm, trim)
        r <- r - delta
        t <- t + delta
        newsum <- sum(abs(z), na.rm = na.rm)
        converged <- newsum == 0 || abs(newsum - oldsum) < eps * newsum
        if (converged)
            break
        oldsum <- newsum
        if (trace.iter)
            cat(iter, ":", newsum, "\n")
    }
    if (converged) {
        if (trace.iter)
            cat("Final:", newsum, "\n")
    } else warning(gettextf("medpolish() did not converge in %d iterations", maxiter), domain = NA)
    names(r) <- rownames(z)
    names(c) <- colnames(z)
    ans <- list(overall = t, row = r, col = c, residuals = z, name = deparse(substitute(x)))
    class(ans) <- "medpolish"
    ans
}
## @title Empirical probability
## @description Generate empirical probability density function for data.
## @param data - vector of data points
## @param  q - trimming value.
## @details Remove 1-q points as outliers from greater tail. Used in plotIGFit.
## @keywords internal
## @return Numeric vector
##
myca <- function(data, q = 0.9) {
    # x <- seq(0.001,3.5,0.001)
    len <- getLen(data, quan = q)
    # len <- 1000
    maxi <- sort(data)[len]
    x <- seq(min(data), maxi, length = len)
    # x <- quantile(data[1:len], ppoints(100))
    p <- rep(0, len - 1)
    lenny <- length(data)
    for (i in 1:len) p[i] <- (sum(data < x[i]))/lenny
    return(cbind(x, p))
}
## @title Get length
## @description Get rid of the very large data points so graphs scale better.
## @param data - vector of data points
## @param  quan - trimming quantile.
## @details Used in plotIGFit (myca).
## @return Numeric vector
## @keywords internal
##
getLen <- function(data, quan = 0.9) {
    return(trunc(quan * length(data)))
}
## @title t-test
## @description Do one-sample t-test
## @param x A numeric matrix
## @param testSide Test side
## @param trueMean True mean
## @details Used in statT.
## @keywords internal
## @return Numeric matrix
myT <- function(x, testSide, trueMean) {

    # handle all NA situation
    if (sum(is.na(x)) == length(x)) {
        return(rep(NA, 5))
    }
    tmp <- stats::t.test(x, alternative = testSide, mu = trueMean)
    return(c(tmp$statistic, base::mean(x, na.rm = TRUE) - trueMean, (1/sqrt(tmp$parameter + 1)) * stats::sd(x,
        na.rm = TRUE), tmp$parameter, tmp$p.value))
}
## @title checking for NAs
## @param x A numeric matrix
## @return Numeric matrix
## @description set return values to NA if initial data were all NA
## @details Used in statRVM.
## @keywords internal
##
checkNA <- function(x) {
    if (is.na(x[1])) {
        outie <- rep(NA, 5)
    } else {
        outie <- x
    }
    return(outie)
}
## @title RVM test
## @param x A numeric matrix
## @param side Test side
## @return Numeric matrix
## @description apply RVM test and return p-value
## @details Used in statRVM.
## @keywords internal
##
myRVM <- function(x, side = "less") {

    if (side == "two.sided") {
        res <- RVT1(x)
    } else {
        res <- RVT1Sided(x, side)
    }
    return(c(res$v, res$mn, sqrt(1/res$vvr), res$n - 1 + 2 * res$a, res$vp))
    # return(list(pval=res$vp, ab=cbind(res$a[1], res$b[1])))
}
## @title RVM two-sided test
##
## @description Apply RVM test to data
##
## @param data Data has genes as rows and arrays as columns.
##
## @return  A data frame with a row for each gene and a column for each parameter:
## \item{mn}{Mean value}
## \item{vr}{Unadjusted variance}
## \item{n}{Number of non-missing samples}
## \item{t}{Unadjusted t-statistic}
## \item{tp}{Unadjusted t p-value}
## \item{v}{RVM model statistic}
## \item{vp}{RVM model p-value}
## \item{vvr}{Adjusted variance}
##
## @details used in statRVM (myRVM).
##
## @keywords internal
## @return Numeric matrix
##
RVT1 <- function(data) {
    vr <- rowVars(data, na.rm = TRUE)
    n <- rowSums(!is.na(data))
    mn <- rowMeans(data, na.rm = TRUE)

    a <- getab(vr, n - 1)
    b <- a[2]
    a <- a[1]
    cat("alpha=", a, "beta=", b, "\n")
    t <- mn/sqrt(vr/n)
    tp <- 2 * (1 - stats::pt(abs(t), df = n - 1))
    vvr <- n * (1 + 2 * a/(n - 1))/(vr + 2/((n - 1) * b))
    v <- mn * sqrt(vvr)
    degf = n - 1 + 2 * a
    vp <- 2 * (1 - stats::pt(abs(v), df = degf))

    return(data.frame(n, mn, vr, t, tp, vvr, v, vp, a, b))
}
## @title RVM one-sided test
##
## @description Apply a one sided one sample test (greater/less than 0)
##
## @param data Data has genes as rows and arrays as columns.
## @param side Optional. Default value is 'less'.
##
## @return  A data frame with a row for each gene and a column for each parameter:
## \item{mn}{Mean value}
## \item{vr}{Unadjusted variance}
## \item{n}{Number of non-missing samples}
## \item{t}{Unadjusted t-statistic}
## \item{tp}{Unadjusted t p-value}
## \item{v}{RVM model statistic}
## \item{vp}{RVM model p-value}
## \item{vvr}{Adjusted variance}
##
## @details Used in statRVM (myRVM).
##
## @keywords internal
## @return Numeric matrix
##
RVT1Sided <- function(data, side = "less") {
    vr <- rowVars(data, na.rm = TRUE)
    n <- rowSums(!is.na(data))
    mn <- rowMeans(data, na.rm = TRUE)

    a <- getab(vr, n - 1)
    b <- a[2]
    a <- a[1]
    cat("alpha=", a, "beta=", b, "\n")
    t <- mn/sqrt(vr/n)
    tp <- 1 - stats::pt(t, df = n - 1)
    vvr <- n * (1 + 2 * a/(n - 1))/(vr + 2/((n - 1) * b))
    v <- mn * sqrt(vvr)
    degf = n - 1 + 2 * a

    if (side == "less") {
        tp <- stats::pt(t, df = n - 1)
        vp <- stats::pt(v, df = degf)

    } else if (side == "greater") {
        tp <- 1 - stats::pt(t, df = n - 1)
        vp <- 1 - stats::pt(v, df = degf)
    }

    return(data.frame(n, mn, vr, t, tp, vvr, v, vp, a, b))
}
## @title Get parameters for RVM
## @description Calculates parameters of prior distribution
## @param sig sig
## @param n n
## @return Estimates of a and b
## @keywords internal
## @details Used in plotIGFit.
## @return Numeric vector
##
getab <- function(sig, n) {
    set <- (!is.na(sig) & n > 0 & sig > 0)
    sig <- sig[set]
    n <- n[set]
    set <- n > 4
    if (sum(set) > 0) {
        m1 <- (n[set] - 2)/((n[set]) * sig[set])
        m2 <- (n[set] - 2) * (n[set] - 4)/((n[set]) * sig[set])^2
        m1 <- base::mean(m1, na.rm = TRUE)
        m2 <- base::mean(m2, na.rm = TRUE)
        b <- m2/m1 - m1
        a <- m1^2/(m2 - m1^2)
    } else {
        a <- b <- 1
    }
    strt <- c(a, b)
    g <- function(p, yunq) flik(p, yunq)
    a <- stats::nlm(g, strt, yunq = c(sig, n))
    a$estimate <- abs(a$estimate)
}
## @title Get log likelihood
## @description Calculates log likelihood for a*b*x from an F distribution with m and 2*a degrees of freedom
## @param p Contains a and b.
## @param y Vector containing data and the m values.
## @keywords internal
## @details Used in plotIGFit (getab).
## @return Numeric vector
##
flik <- function(p, y) {
    x <- y[1:(length(y)/2)]
    m <- y[(length(y)/2 + 1):length(y)]
    p <- abs(p)
    a <- p[1]
    b <- p[2]
    x <- x * (a * b)
    n <- 2 * a
    out <- log(stats::df(x, m, n)) + log(a * b)
    sum(-out)
}
## @title Row Variance
## @description Calculates variance of elements in a row
## @param x Data matrix.
## @keywords internal
## @details Used in plotIGFit.
## @return Numeric vector
##
rowVars <- function(x, ...) {
    apply(x, 1, stats::var, ...)
}
