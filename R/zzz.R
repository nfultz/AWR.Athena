driver_path <- NULL

.onLoad <- function(libname, pkgname) {

    ## path to the JDBC driver
    file <- sprintf('AthenaJDBC41-%s.jar', packageVersion(pkgname))
    path <- file.path(system.file('', package = pkgname), file.path('java', file))

    ## check if the jar is available and install if needed (on first load)
    if (!file.exists(path)) {

        ## create the java folder in the package install folder if not yet available
        if (!dir.exists(dirname(path))) {
            dir.create(dirname(path), recursive = TRUE)
        }

        ## download the jar file from AWS
        try(download.file(
            url = sprintf(
                'https://s3.amazonaws.com/athena-downloads/drivers/AthenaJDBC41-%s.jar',
                packageVersion(pkgname)),
            destfile = path, mode = 'wb'),
            silent = TRUE)

    }

    ## update path to the driver
    utils::assignInMyNamespace('driver_path', path)

}

.onAttach <- function(libname, pkgname) {

    ## let the user know if the automatic JDBC driver installation failed
    path <- file.path(system.file('', package = pkgname), 'java')
    if (length(list.files(path)) == 0) {
        packageStartupMessage(
            'The automatic installation of the Athena JDBC driver seems to have failed.\n',
            'Please check your Internet connection and if the current user can write to ', path, '\n',
            'If still having issues, install the jar file manually from:\n',
            'https://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html\n')
    }

}
