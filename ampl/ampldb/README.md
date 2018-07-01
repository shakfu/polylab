# Install ODBC slite3 and postgresql in Ubuntu Linux


## Install Packages

### Sqlite3

```
sudo apt install sqlite3
````

### Postgres


``
sudo apt install <postgres-version>`
```

After installation

```
sudo -u postgres createuser -s $USER
sudo -u postgres createdb $USER
```

This will give you local access via the /var/run/postgresql socket, which is quite usable.

However tt does **not** allow you yet to access localhost which some applications require. To do that:

1. Uncomment `listen_addresses = 'localhost'` in `/etc/postgresql/<postgres-version>/main/postgresql.conf`

2. Set the encrypted password for your user (what was in $USER in this example):
```
sudo -u postgres sql
ALTER USER <username> with encrypted password '<password>';
````` ``



### ODBC
````
libodbc1 - ODBC library for Unix
odbcinst - Helper program for accessing odbc ini files
odbcinst1debian2 - Support library for accessing odbc ini files
libsqliteodbc - ODBC driver for SQLite embedded database
odbc-postgresql - ODBC driver for PostgreSQL
unixodbc - Basic ODBC tools
```

## Configure ODBC

```
sa@zoo:~$ cat .odbc.ini 
[dietsql3]
Description = ampl sqlite3 db
Driver = SQLite3
Database = /home/sa/src/ampl/sqlite3/diet.db
Timeout = 2000

[dietpg]
Description = PostgreSQL connection to diet
Driver = PostgreSQL ANSI
Database = mydb
Servername = localhost
Port = 5432
UserName = joe
Password = mypass
sa@zoo:~$ 

```

### Test

```
isql <dsn>
```

for example

```
isql dietpg
```




