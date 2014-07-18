## 6 Aug 2013: Write ergm term for bipartite network.

InitErgmTerm.biabsdiff <- function(nw, arglist, ...){

  a <- check.ErgmTerm(nw, arglist, directed=TRUE, bipartite=FALSE,
                      varnames = c("attrname", "pow"),
                      vartype=c("character", "numeric"),
                      defaultvalues=list(NULL, 1),
                      required=c(TRUE, FALSE)
                      )

  ## Process the arguments
  nodecov <- get.node.attr(nw, a$attrname)
  ## Construct the list to return
  list(name="bi.absdiff",
       coef.names=paste(paste("bi.absdiff", if(a$pow != 1) a$pow else "",
         sep=""),a$attrname. sep="."),
       pkgname = "ergm.userterms",
       input = c(a$pow, nodecov),
       dependence = FALSE,
       emptynwstats = network.size(nw)
       )
}
