#This inserts a job into the queue

required_packages <- c("DBI", "RPostgres", "curl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) install.packages(missing_packages)


postgres_host <- "192.168.68.65"
postgres_password <- "MouseDonkeyTree"

claim_job <- function(worker_id, Silent=FALSE, job=NA) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  
  on.exit(DBI::dbDisconnect(con))

  
  
  if(is.na(job)) {
    data <- DBI::dbGetQuery(con, "UPDATE jobs SET status = 'running', worker = $1, started_at = now(), attempts = attempts + 1  WHERE id = (SELECT id FROM jobs  WHERE status = 'queued' ORDER BY id FOR UPDATE SKIP LOCKED LIMIT 1) RETURNING id, payload, status, worker, started_at, attempts", params = list(worker_id))
    if(!Silent) print(paste("Jobs remaining:", DBI::dbGetQuery(con, "select count(*) as count from jobs where status = 'queued'")$count))
  } else {
    data <- DBI::dbGetQuery(con, "UPDATE jobs SET status = 'running', worker = $1, started_at = now(), attempts = attempts + 1  WHERE id = (SELECT id FROM jobs  WHERE status = 'queued' and job_name=$2 ORDER BY id FOR UPDATE SKIP LOCKED LIMIT 1) RETURNING id, payload, status, worker, started_at, attempts", params = list(worker_id, job))
    if(!Silent) print(paste("Jobs remaining:", DBI::dbGetQuery(con, "select count(*) as count from jobs where status = 'queued' and job_name=$1", params=list(job))$count))
  }
      
  return(data)
}


complete_job <- function(job_id, worker_id, quote_data) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)

  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbGetQuery(
    con,
    "UPDATE jobs SET status = 'completed', worker = $2, finished_at = now(), quote_data = $3
    WHERE id = $1 AND worker = $2 AND status = 'running'
    RETURNING id, status, worker, started_at, finished_at, attempts
    ",
    params = list(job_id, worker_id, quote_data)
  )
}


requeue_stale_jobs <- function(minutes_old = 10, Silent = FALSE) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  res <- DBI::dbGetQuery(
    con,
    "UPDATE jobs SET status = 'queued', worker = NULL, started_at = NULL WHERE status = 'running'  AND started_at < now() - ($1::text || ' minutes')::interval
    RETURNING id, payload, status, worker, started_at, attempts",
    params = list(as.character(minutes_old))
  )
  
  if (!Silent) print(paste("Jobs returned to queue:", nrow(res)))
  
  invisible(NULL)
}


retreive_job <- function(job_name) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  res <- DBI::dbGetQuery(con, "select * from jobs WHERE job_name = $1", params = list(job_name))
  
  res
}


requeue_jobs <- function(job_ids) {
  stopifnot(length(job_ids) > 0)
  stopifnot(all(!is.na(job_ids)))
  stopifnot(all(job_ids == as.integer(job_ids)))
  
  job_ids <- as.integer(job_ids)
  job_ids_sql <- paste(job_ids, collapse = ",")
  
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  res <- DBI::dbGetQuery(
    con,
    "UPDATE jobs SET status = 'queued', worker = NULL, started_at = NULL, quote_data = ''
     WHERE id = ANY(('{' || $1::text || '}')::int[])
     RETURNING id, payload, status, worker, started_at, attempts, quote_data",
    params = list(job_ids_sql)
  )
  
  print(paste("Jobs returned to queue:", nrow(res)))
  
  invisible(res)
}


insert_job <- function(payload, job, CloseOnExit=TRUE) {
  if(!exists("con")) {
    con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  }

  if(CloseOnExit) on.exit(DBI::dbDisconnect(con))
  
  if(payload !="") DBI::dbGetQuery(con, "INSERT INTO jobs (payload, job_name) VALUES ($1, $2) RETURNING id, status, payload, created_at", params = list(payload, job))
}


delete_job <- function(job_name) {
    if(!exists("con")) {
    con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  }

  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "DELETE FROM jobs WHERE job_name = $1", params = list(job_name))
}


list_job_names <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "select distinct job_name FROM jobs")
}


unqueue_jobs <- function(job) {
  
  con <- DBI::dbConnect(RPostgres::Postgres(), host = postgres_host, port = 5432, dbname = "queue_db", user = "postgres", password = postgres_password)
  
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  res <- DBI::dbExecute(
    con,
    "UPDATE jobs SET status = 'not_queued', worker = NULL, started_at = NULL
     WHERE job_name =$1", params = list(job)
  )
    
}
