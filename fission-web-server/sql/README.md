# Destructive Migrations

### Contents
This folder contains SQL files that should be run to get our database up to date. Each is prefixed with the unix time (in seconds) when it was created.

### Running the files

You can run them using `\i` in your postgres terminal.

```sql
\i 1576714308_selda_to_persistent.sql
```

Note that `\i` assumes a relative path.

If you are running postgres in a Docker container, connect to container locally then run the migration file

```sh
psql -U postgres -d web_api -h runfission.test -p 5432
```

This assumes a DNS setup where the postgres docker container is available at `runfission.test`. (See the [Fission web server README](../README.md).)

### Why do we have these files
Our database library `persistent` only performs non destructive updates.