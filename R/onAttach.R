

.onAttach <- function(libname, pkgname) {
  version <- try(utils::packageVersion('mcmcOutput'), silent=TRUE)
  if(!inherits(version, "try-error"))
    packageStartupMessage("This is mcmcOutput ", version,
      ". For overview type ?mcmcOutput.")
}
