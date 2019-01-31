
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

#' Constructor of AthenaDriver
#' 
#' @name AthenaDriver
#' @rdname AthenaDriver-class
setMethod(initialize, "AthenaDriver",
   function(.Object, ...)
{
    # passed to parent builder, than unboxed, yuck
    # should ping RJDBC maintainers, and have them implement initialize methods instead
    jdbc <- JDBC(driverClass="com.simba.athena.jdbc.Driver",
                 identifier.quote="'")

    .Object@jdrv = jdbc@jdrv
    .Object@identifier.quote = jdbc@identifier.quote
    .Object
})

#' Athena connection class.
#'
#' Class which represents the Athena connections.
#'
#' @export
#' @importClassesFrom RJDBC JDBCConnection
#' @keywords internal
setClass("AthenaConnection",
  contains = "JDBCConnection",
  slots = list(
    region = "character",
    S3OutputLocation = "character",
    Schema = "character"
  )
)

#' Authentication credentials are read from the DefaultAWSCredentialsProviderChain, which includes the .aws folder and
#' environment variables.
#'
#' @param drv An object created by \code{Athena()}
#' @param region the AWS region
#' @param S3OutputLocation S3 bucket where results will be saved to
#' @param Schema Athena schema to use
#' @param ... Other options
#' @rdname Athena
#' @seealso \href{http://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html#jdbc-options}{Athena Manual} for more connections options.
#' @export
#' @examples
#' \dontrun{
#' require(DBI)
#' con <- dbConnect(AWR.Athena::Athena(), region='us-west-2', 
#'                  S3OutputLocation='s3://nfultz-athena-staging', 
#'                  Schema='default')
#' dbListTables(con)
#' dbGetQuery(con, "Select count(*) from sampledb.elb_logs")
#' }
setMethod("dbConnect", "AthenaDriver",
          function(drv, region, S3OutputLocation, Schema, ...) {

  con <- callNextMethod(drv, url=sprintf('jdbc:awsathena://athena.%s.amazonaws.com:443/', region),
                   S3OutputLocation=S3OutputLocation,
                   Schema=Schema,
                   AWSCredentialsProviderClass="com.simba.athena.amazonaws.auth.DefaultAWSCredentialsProviderChain", ...)

  new("AthenaConnection", jc = con@jc, identifier.quote = drv@identifier.quote, region=region,S3OutputLocation=S3OutputLocation, Schema=Schema)
})