#make a formula. Adapted from biomod2 package

makeFormula <- function (respName, explVar, type = "simple", interaction.level = 0, ...) {
  sup_args <- list(...)
  availableTypes = c("simple", "quadratic", "polynomial", "s_smoother")
  if (!is.character(respName) || length(respName) != 1) {
    stop("You must give a unique response variable name")
  }
  if (!is.data.frame(explVar) && !is.matrix(explVar)) {
    stop("You must give explanatory variable table")
  }
  if (!(type %in% availableTypes)) {
    stop(paste("Formuula type must be one of : ", toString(availableTypes), sep = ""))
  }
  explVarNames <- colnames(explVar)
  if (respName %in% explVarNames) {
    explVar <- explVar[, -which(explVarNames == respName), drop = FALSE]
    explVarNames <- colnames(explVar)
  }
  interaction.level <- min(interaction.level, ncol(explVar))
  junk <- c()
  switch(EXPR = type, simple = {
    junk <- c(junk, explVarNames)
  }, quadratic = {
    for (v in 1:ncol(explVar)) {
      if (is.numeric(explVar[, v])) {
        junk <- c(junk, paste(explVarNames[v], "+I(", explVarNames[v], "^2)", sep = ""))
      } else {
        junk <- c(junk, explVarNames[v])
      }
    }
  }, polynomial = {
    for (v in 1:ncol(explVar)) {
      if (is.numeric(explVar[, v])) {
        junk <- c(junk, paste(explVarNames[v], "+I(", explVarNames[v], "^2)+I(", explVarNames[v], "^3)", sep = ""))
      } else {
        junk <- c(junk, explVarNames[v])
      }
    }
  }, s_smoother = {
    for (v in 1:ncol(explVar)) {
      if (is.numeric(explVar[, v])) {
        if (is.null(sup_args$k)) {
          junk <- c(junk, paste("s(", explVarNames[v], ")", sep = ""))
        } else {
          junk <- c(junk, paste("s(", explVarNames[v], ",k=", sup_args$k, ")", sep = ""))
        }
      } else {
        junk <- c(junk, explVarNames[v])
      }
    }
  })
  junk.inter <- NULL
  if (interaction.level > 0) {
    junk.inter <- unlist(strsplit(junk, "+", fixed = TRUE))
    eval(parse(text = paste("junk.inter <- levels(interaction(", toString(rep("junk.inter", interaction.level + 1)), ",sep=':'))", sep = "")))
  }
  junk <- gsub(", ", " + ", toString(c(junk, junk.inter)))
  return(as.formula(paste(respName, " ~ ", junk, sep = "")))
}
