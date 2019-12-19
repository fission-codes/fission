# Destructive Migrations

### Contents
This folder contains SQL files that should be run to get our database up to date. Each is prefixed with the unix time (in seconds) when it was created.

### Running the files
You can run them using `\i` in you postgres terminal.

```sql
\i 1576714308_selda_to_persistent.sql
```

### Why do we have these files
Our database library `persistent` only performs non destructive updates.