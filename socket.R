# Start a separate R process with a script that launch a socket server on 8889
# and wait for the variable `done` in `.GlobalEnv` to finish
rscript <- Sys.which("Rscript")
system2(rscript, "--vanilla -e 'svSocket::start_socket_server(8889); while (!exists(\"done\")) Sys.sleep(1)'", wait = FALSE)





## connect to the server
con <- socketConnection(host = "localhost", port = 8889,
                        blocking = FALSE, timeout = 30)

library(svSocket)
eval_socket_server(con, '1 + 1')

##eval_socket_server(con, '\n<<<H>>>svSocket::par_socket_server(<<<s>>>, bare = FALSE)')
##eval_socket_server(con, '1 + 1')

run_socket_server <- function(con, code) {
  cat(code, "\n", file = con)
  res <- NULL
  while (!length(res)) {
    Sys.sleep(0.01)
    res <- readLines(con)
  }
  # Use this instruction to output results as if code was run at the prompt
  #cat(res, "\n")
  invisible(res)
}


(run_socket_server(con, '\n<<<H>>>svSocket::par_socket_server(<<<s>>>, bare = FALSE, echo=FALSE)'))
tmp <- run_socket_server(con, '1 + 1'); print(tmp)

eval_socket_server(con, 'done <- NULL') # The server will stop after this transaction
close(con)




