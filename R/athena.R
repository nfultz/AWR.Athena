
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
    jdbc <- JDBC(driverClass="com.amazonaws.athena.jdbc.AthenaDriver",
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
    s3_staging_dir = "character",
    schema_name = "character"
  )
)

#' Authentication credentials are read from the DefaultAWSCredentialsProviderChain, which includes the .aws folder and
#' environment variables.
#'
#' @param drv An object created by \code{Athena()}
#' @param region the AWS region
#' @param s3_staging_dir S3 bucket where results will be saved to
#' @param schema_name Athena schema to use
#' @param ... Other options
#' @rdname Athena
#' @seealso \href{http://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html#jdbc-options}{Athena Manual} for more connections options.
#' @export
#' @examples
#' \dontrun{
#' require(DBI)
#' con <- dbConnect(AWR.Athena::Athena(), region='us-west-2',
#'                  s3_staging_dir='s3://nfultz-athena-staging',
#'                  schema_name='default')
#' dbListTables(con)
#' dbGetQuery(con, "Select count(*) from sampledb.elb_logs")
#' }
setMethod("dbConnect", "AthenaDriver",
          function(drv, region, s3_staging_dir, schema_name, ...) {

  con <- callNextMethod(drv, url=sprintf('jdbc:awsathena://athena.%s.amazonaws.com:443/', region),
                   s3_staging_dir=s3_staging_dir,
                   schema_name=schema_name,
                   aws_credentials_provider_class="com.amazonaws.athena.jdbc.shaded.com.amazonaws.auth.DefaultAWSCredentialsProviderChain", ...)

  new("AthenaConnection", jc = con@jc, identifier.quote = drv@identifier.quote, region=region, s3_staging_dir=s3_staging_dir, schema_name=schema_name)

})

#' Send query and retrieve results
#' @export
setMethod("dbGetQuery", signature(conn = "AthenaConnection", statement = "character"),
          def = function(conn, statement, ...) {

  res <- dbSendQuery(conn, statement, ...)
  ## Athena can only pull 999 rows at a time
  dbFetch(res, n = -1, block = 999)

})
