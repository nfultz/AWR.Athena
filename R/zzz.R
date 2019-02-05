#' @importFrom utils packageVersion download.file
#' @importFrom rJava .jpackage
.onLoad <- function(libname, pkgname) {

    version <- packageVersion(pkgname)[1,1:3] # drop internal releases eg 1.1.0-1 -> 1.1.0
  
    ## path to the JDBC driver
    file <- sprintf('AthenaJDBC41_%s.jar', version)
    path <- file.path(system.file('java', package = pkgname), file)

    ## check if the jar is available and install if needed (on first load)
    if (!file.exists(path)) {

        url <- paste0('https://s3.amazonaws.com/athena-downloads/drivers/JDBC/SimbaAthenaJDBC_', version, '/', file)

        ## download the jar file from AWS
        try(download.file(url = url, destfile = path, mode = 'wb'),
            silent = TRUE)

    }

    ## add the RJDBC driver and the log4j properties file to classpath
    rJava::.jpackage(pkgname, lib.loc = libname)

}

.onAttach <- function(libname, pkgname) {

    ## let the user know if the automatic JDBC driver installation failed
    path <- system.file('java', package = pkgname)
    if (length(list.files(path, '^AthenaJDBC41_[0-9.]*jar$')) == 0) {
        packageStartupMessage(
            'The automatic installation of the Athena JDBC driver seems to have failed.\n',
            'Please check your Internet connection and if the current user can write to ', path, '\n',
            'If still having issues, install the jar file manually from:\n',
            'https://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html\n')
    }

}
