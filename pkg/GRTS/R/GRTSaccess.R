#' Perform a GRTS sampling on a access database table via ODBC
#' 
#' @param channel an ODBC channel to the database
#' @param tablename the name of the table in the database
#' @param grts.vars the name of the variabeles which define the GRTS sampling space. Typically an X and Y variable. However the only limitations are that all variables are numeric and the number of variables must be at least one.
#' @param cellsize the size of the smallest GRTS cell for each dimension.
#' @param samplesize the required sample size
#' @param availableLevels the number of available GRTS levels in the database. These must be named L01, L02, ... Currently available levels is limited to 99
#' @param reset logical value. If FALSE the current GRTS randomisation is completed. If TRUE a new GRTS randomisation will start.
#' @param prefix Prefix of the fields that store the randomisation
#' @param prob The name of the fields with the sampling probability
#' @importFrom RODBC odbcClose sqlTables sqlQuery
#' @export
GRTS.access <- function(channel, tablename, grts.vars, cellsize, samplesize, availableLevels = 20, reset = FALSE, prefix = "L", prob){
  if(!tablename %in% sqlTables(channel = channel)$TABLE_NAME){
    stop("table ", tablename, " does not exists.")
  }
  if(availableLevels > 99){
    stop("availableLevels is currently limited to 99. Contact the maintainer is you need more levels.")
  }
  
  
  
  GRTS.ODBC.Update <- function(thisSample, maxLevel, grts.vars, tablename, Low, Range, channel, prefix){
    Level <- 1 + maxLevel - max(which(is.na(thisSample)))
    Sign <- sample(c(" >= ", " < "), length(grts.vars), replace = TRUE)
    Index <- sample(length(grts.vars)) - 1
  
    whereStatement <- ifelse(
      is.na(thisSample), 
      paste("IsNull(", names(thisSample), ")", sep = ""), 
      paste(names(thisSample), "=", thisSample)
    )
    selectStatement <- paste("Avg(", grts.vars, ") AS M", grts.vars, sep = "")
    SQL <- paste("SELECT ", paste(selectStatement, collapse = ", "), " FROM ", tablename, " WHERE ", paste(whereStatement, collapse = " AND "), sep = "")
    Mean <- sqlQuery(channel = channel, query = SQL)
    Position <- ((Mean - Low) / Range) %/% 2 ^ (1 - Level)
    Mid <- Low + (Position + 0.5) * Range / 2 ^ (Level - 1)
    SQL <- paste(2 ^ Index, " * (", grts.vars, Sign, Mid, ")", sep = "")
    SQL <- paste(SQL, collapse = " - ")
    SQL <- paste("UPDATE", tablename, "SET", sprintf("%s%02i", prefix, Level), "= -", SQL, "WHERE", paste(whereStatement, collapse = " AND "))
    sqlQuery(channel = channel, query = SQL)
  }

  #calculate the central point
  SQL <- paste("(Min(", grts.vars, ") + Max(", grts.vars, ")) / 2 AS mid_", grts.vars, sep = "")
  SQL <- paste("SELECT", paste(SQL, collapse = ", "), "FROM", tablename)
  Mid <- sqlQuery(channel = channel, query = SQL)
  if(class(Mid) != "data.frame"){
    stop("Make sure that grts.vars points to existing columns in ",  tablename)
  }
  colnames(Mid) <- grts.vars

  #reset the GRTS sample
  if(reset){
    SQL <- paste("UPDATE", tablename, "SET", paste(sprintf("%s%02i = NULL", prefix, seq_len(availableLevels)), collapse = ", "))
    sqlQuery(channel = channel, query = SQL)
  }
  
  #estimate the range (number of GRTS cells per dimension)
  SQL <- paste("Max(", grts.vars, ") - Min(", grts.vars, ") AS range_", grts.vars, sep = "")
  SQL <- paste("SELECT\n", paste(SQL, collapse = ", "), "FROM", tablename)
  Range <- sqlQuery(channel = channel, query = SQL)
  maxLevel <- ceiling(log2(max(Range / cellsize)))
  Range <- cellsize * 2 ^ maxLevel
  
  #the origin
  Low <- Mid - Range / 2

  
  SQLsample1 <- paste(sprintf("%s%02i", prefix, rev(seq_len(maxLevel))), collapse = ", ")
  SQLsample2 <- paste(sprintf("iif(IsNull(%s%02i), 0, %s%02i)", prefix, rev(seq_len(maxLevel)), prefix, rev(seq_len(maxLevel))), collapse = ", ")
  if(missing(prob)){
    SQLsample <- paste("SELECT TOP", samplesize, SQLsample1, "FROM", tablename, "GROUP BY", SQLsample1, "ORDER BY", SQLsample2)
  } else {
    SQLsample <- paste("SELECT TOP", samplesize, "SUM(", prob, ") AS Prob, ", SQLsample1, "FROM", tablename, "WHERE", prob, "> 0 GROUP BY", SQLsample1, "ORDER BY", SQLsample2)
  }
  Sample <- sqlQuery(channel = channel, query = SQLsample)
  while(any(is.na(Sample))){
    if(missing(prob)){
      Sample <- Sample[is.na(Sample[, 1]), ]
      junk <- apply(Sample, 1, FUN = GRTS.ODBC.Update, maxLevel = maxLevel, grts.vars = grts.vars, tablename = tablename, channel = channel, Low = Low, Range = Range, prefix = prefix)
    } else {
      Sample <- Sample[is.na(Sample[, 2]), ]
      junk <- apply(Sample[, -1], 1, FUN = GRTS.ODBC.Update, maxLevel = maxLevel, grts.vars = grts.vars, tablename = tablename, channel = channel, Low = Low, Range = Range, prefix = prefix)
      if(sum(Sample$Prob) < samplesize){
        samplesize <- ceiling(samplesize * 1.1)
        SQLsample <- paste("SELECT TOP", samplesize, "SUM(", prob, ") AS Prob, ", SQLsample1, "FROM", tablename, "WHERE", prob, "> 0 GROUP BY", SQLsample1, "ORDER BY", SQLsample2)
      }
    }
    Sample <- sqlQuery(channel = channel, query = SQLsample)
    Sample
  }
  odbcClose(channel)
}
