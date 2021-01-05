#' connect_to_heroku_postgres
#'
#' connect to our Heroku Postgres instance
#' @return the connection
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#' @importFrom rjson fromJSON
#' @export
#'
connect_to_heroku_postgres <- function() {
  if (Sys.getenv('ENVIRONMENT') == 'production') {
    host <- Sys.getenv('HP_HOST')
    dbname <- Sys.getenv('HP_DBNAME')
    port <- Sys.getenv('HP_PORT')
    user <- Sys.getenv('HP_USER')
    pass <- Sys.getenv('HP_PASS')
  } else {
    x <- fromJSON(file = 'inst/keys/creds.json')[[1]]
    host <- x$host
    dbname <- x$dbname
    port <- x$port
    user <- x$user
    pass <- x$pass
  }

  dbConnect(Postgres(),
            dbname = dbname,
            host = host,
            port = port,
            user = user,
            password = pass,
            sslmode = "require"
  )
}
