
void db_exit(PGconn * conn);
PGconn * db_connect(void);
int db_command(PGconn * conn, char * qstring);
PGresult * db_query(PGconn * conn, char * qstring);
void db_display(PGresult * res);
void db_analyze(PGresult * res);