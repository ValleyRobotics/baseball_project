library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "baseball_stats.db")


as.data.frame(dbListTables(con))

stats <- dbReadTable(con, 'bat_p')

dbDisconnect(con)

stats




dbConnector <- function(dbname) {
  require(RSQLite)
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}
conn=
dbConnector("data/baseball_stats.db")
dbGetData <- function(conn, tblname, month, day) {
  query <- paste("SELECT * FROM",
                 tblname,
                 "WHERE month =",
                 as.character(month),
                 "AND day =",
                 as.character(day))
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}