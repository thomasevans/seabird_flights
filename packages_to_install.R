packages_installed <- installed.packages(lib.loc = NULL, priority = NULL,
                   noCache = FALSE, fields = NULL,
                   subarch = .Platform$r_arch)
str(packages_installed)

package_names <- packages_installed[,1]
save(package_names, packages_installed, file = "packages_20160428.RData")
# ?save


lapply(paste("package:", package_names, sep = ""), detach,
       character.only = TRUE, unload = TRUE)
install.packages(package_names)



sess.pkgs <- function (package = NULL) 
{   z <- list()
if (is.null(package)) {
  package <- grep("^package:", search(), value = TRUE)
  keep <- sapply(package, function(x) x == "package:base" || 
                   !is.null(attr(as.environment(x), "path")))
  package <- sub("^package:", "", package[keep])
}
pkgDesc <- lapply(package, packageDescription)
if (length(package) == 0) 
  stop("no valid packages were specified")
basePkgs <- sapply(pkgDesc, function(x) !is.null(x$Priority) && 
                     x$Priority == "base")
z$basePkgs <- package[basePkgs]
if (any(!basePkgs)) {
  z$otherPkgs <-  package[!basePkgs]
}
z
}

lapply(paste("package:",sess.pkgs()$otherPkgs, sep=""), detach, 
       character.only = TRUE, unload = TRUE)