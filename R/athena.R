
#' Athena driver class.
#'
#' @keywords internal
#' @export
#' @import RJDBC
#' @import methods
#' @importClassesFrom RJDBC JDBCDriver
setClass("AthenaDriver", contains = "JDBCDriver")

#' Athena DBI wrapper
#'
#' @export
Athena <- function() {
  new("AthenaDriver")
}

#' @export
setMethod(initialize, "AthenaDriver",
   function(.Object, ...)
{
    # passed to parent builder, than unboxed, yuck
    # should ping RJDBC maintainers, and have them implement initialize methods instead
    jdbc <- JDBC(driverClass="com.amazonaws.athena.jdbc.AthenaDriver",
                 classPath=driver_path,
                 identifier.quote="'")

    .Object@jdrv = jdbc@jdrv
    .Object@identifier.quote = jdbc@identifier.quote
    .Object
})

#' Athena connection class.
#'
#' @export
#' @importClassesFrom RJDBC JDBCConnection
#' @keywords internal
setClass("AthenaConnection",
  contains = "JDBCConnection",
  slots = list(
    region = "character",
    s3_staging_dir = "character",
    schema_name = "character"
  )
)

#' @param drv An object created by \code{Kazam()}
#' @rdname Athena
#' @export
#' @examples
#' \dontrun{
#' require(DBI)
#' con <- dbConnect(AWR.Athena::Athena(), region='us-west-2', s3_staging_dir='s3://nfultz-athena-staging', schema_name='default')
#' dbListTables(con)
#' }
setMethod("dbConnect", "AthenaDriver",
          function(drv, region, s3_staging_dir, schema_name, ...) {

  con <- callNextMethod(drv, url=sprintf('jdbc:awsathena://athena.%s.amazonaws.com:443/', region),
                   s3_staging_dir=s3_staging_dir,
                   schema_name=schema_name,
                   aws_credentials_provider_class="com.amazonaws.athena.jdbc.shaded.com.amazonaws.auth.DefaultAWSCredentialsProviderChain")

  new("AthenaConnection", jc = con@jc, identifier.quote = drv@identifier.quote, region=region, s3_staging_dir=s3_staging_dir, schema_name=schema_name)
})
