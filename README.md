# interlog: Interactive Log Phorum

Simple hyperminimal forum proof-of-concept. In the spirit of the Gopher protocol and Unix, used at: [gopher://gopher.someodd.zip/1/interlog](gopher://gopher.someodd.zip/1/interlog).

Just a CLI tool, no daemon, no DB. All it does is (1) append to logs (2) create a gophermap index (3) create new logs.

Threads are just text files, stored in this format:

```
interlogs/
  id_epoch_titleslug.txt
```

... replies are just lines appended to a log.

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

You can set up the Interlog CLI to be used as a gopherapp easily using my [Venusia Gopher daemon](https://github.com/someodd/venusia), editing `routes.toml`:

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
menu = false
command = "/usr/bin/interlog"
arguments = ["/var/gopher/output/interlogs/", "new", "$search", "--confirm-text=0Created %s. Do not refresh. Open me or go back!\t/interlog/logs/%s\tgopher.someodd.zip\t70"]

[[gateway]]
selector = "/interlog/append/*"
search = true
wildcard = true
menu = false
command = "/usr/bin/interlog"
arguments = ["/var/gopher/output/interlogs/", "append", "$wildcard", "$search", "--confirm-text=0Appended to %s. Do not refresh. Open me or go back!\t/interlog/logs/%s\tgopher.someodd.zip\t70"]

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
