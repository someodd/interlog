# interlog: Interactive Log Phorum

A simple proof-of-concept, very Gopher-style forum.

this forum software is just a CLI tool. all it does is (1) append to a file (2) create the index you see (3) create new logs. it doesn't have a daemon or database. it just manages a directory that has "logs".

It’s designed to be as minimal as possible while keeping the feel of Gopher and staying very Unix-y. It’s a small tool that’s easy to integrate and run. No daemon — you can hook it in as a gateway. Pure CLI.

Threads are just text files, stored in this format:

```
interlogs/
  id_epoch_title.txt
```

All interlog does is provide an index view and interface for the `interlogs/` directory.

A log caps at 1 MB (`LOG_MAX_SIZE`).
The maximum append/add size is `APPEND_MAX_SIZE`.

## CLI Interface

* `interlog <interlogs/> append <id> "message goes here"`
  Appends to the log beginning with `interlog_posts/id_`. Exits with an error if there is a filesystem error, if the log exceeds `LOG_MAX_SIZE`, or if the message exceeds `APPEND_MAX_SIZE`.

* `interlog <interlogs/> index`
  Generates a gophermap of all logs in `interlogs/`, sorted by last updated file metadata. The epoch in the filename is the file creation date.
  Includes a link to create a new log and, per log, to append to it.
  Only the `FIRST_N_BYTES` and `LAST_N_BYTES` are shown (no overlap), along with the latest update/modify time.

* `interlog <interlogs/> new "message goes here"`
  Creates a new log, with a slug derived from the first line (content is stripped). Exits with an error if there is a filesystem error, the log would exceed `LOG_MAX_SIZE`, or the message is too large.

Errors are also raised if `\n` is present in the message (may be expanded in future).

## Options (helps with generating index)

* `--host` (e.g. `gopher.someodd.zip`) required
* Optional `--port`
* Optional `--base-selector` to prepend to everything when generating the index

## Administration

Just modify the filesystem directly.

## Venusia Gateway Setup

An example...

```
# FORUM/INTERLOG
################

[[files]]
selector = "/interlog/logs"
path = "/var/gopher/output/interlogs/"

[[gateway]]
selector = "/interlog"
search = false
wildcard = false
menu = false
command = "/usr/bin/interlog"
arguments = ["/var/gopher/output/interlogs/", "index", "--host=gopher.someodd.zip", "--base-selector=interlog", "--logs-selector=logs"]

[[gateway]]
selector = "/interlog/new"
search = true
wildcard = false
menu = true
command = "/usr/bin/interlog"
arguments = ["/var/gopher/output/interlogs/", "new", "$search"]

[[gateway]]
selector = "/interlog/append/*"
search = true
wildcard = true
menu = false
command = "/usr/bin/interlog"
arguments = ["/var/gopher/output/interlogs/", "append", "$wildcard", "$search"]

[[gateway]]
selector = "/interlog/log/*"
search = false
wildcard = true
menu = false
command = "cat"
arguments = ["/var/gopher/output/interlogs/$wildcard"]

# The root most files needs to be defined last.
#
# [[files]] defines a route that uses the built-in file server.
[[files]]
selector = ""
path = "/var/gopher/output"
```
