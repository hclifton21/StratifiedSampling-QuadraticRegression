confint.t <- function (object, tdf, parm, level = 0.95, tails = 'both')
{
    cf <- coef(object)
    pnames <- names(cf)
    if ( missing(parm) ) {
      parm <- pnames
    }
    else if ( is.numeric(parm) ) {
      parm <- pnames[parm]
    }

    ses <- sqrt(diag(vcov(object)))[parm]

    if ( tails == 'both' ) {
      a <- (1 - level)/2
      al <- (1 - level)
      tail_str <- 'Two-Tailed'
      cil <- c(paste("   ",a*100,"%   "), paste("   ",(1-a)*100,"%"))
      cif <- c(-1, 1)
      cif <- cif*ses*qt((1-a),tdf)
      ci <- sprintf("%.5f", (cf[parm] + cif))
    }
    else if ( tails == 'lower' ) {
      a <- (1 - level)
      al <- (1 - level)
      tail_str <- 'One-Tailed (Lower)'
      cil <- c(paste("    ",a*100,"%   "), "    upper")
      ci <- c(sprintf("%.5f", (cf[parm] - ses*qt((1-a),tdf))), "infinity")
    }
    else if ( tails == 'upper' ) {
      a <- (1 - level)
      al <- (1 - level)
      tail_str <- 'One-Tailed (upper)'
      cil <- c("    lower   ", paste("    ",(1-a)*100,"%"))
      ci <- c("-infinity", sprintf("%.5f", (cf[parm] + ses*qt((1-a),tdf))))
    }

   
cat("-------------------------------------------------------------------\n")
    cat(paste("mean(",parm, ") =",sprintf("%.5f",cf[parm]),"\n"))
    cat(paste("SE(",parm, ") =",sprintf("%.5f",ses),"\n"))
    cat(paste(tail_str, "CI for", parm, "where alpha =", al,
"with", tdf, "df\n"))
    cat(paste(cil[1],cil[2],"\n"))
    cat(paste(" ", ci[1], "   ", ci[2], "     \n"))
   
cat("-------------------------------------------------------------------\n")
}
