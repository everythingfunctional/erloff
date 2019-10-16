# erloff

[![pipeline status](https://gitlab.com/everythingfunctional/erloff/badges/master/pipeline.svg)](https://gitlab.com/everythingfunctional/erloff/commits/master)

Errors and logging for fortran

Have intent(out) message and/or error list from any procedure. The calling
procedure can then append those messages/errors to its own list(s), or
interogate the lists and deal with them directly. It is not safe to append
messages/errors to a single list concurrently, but it is safe run procedures
that return these lists concurrently.

This package makes logging and error handling safe for multithreaded programs.
