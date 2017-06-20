/*
 * query.c
 *
 *  Test the C version of libpq, the PostgreSQL frontend library.
 */
#include <stdio.h>
#include <stdlib.h>
#include <libpq-fe.h>

#define COLOR_RESET     "\033[m"
#define COLOR_BOLD_RED  "\033[1;31m"
#define COLOR_BOLD_CYAN "\033[1;36m"


enum db_type {
    BOOLOID      = 16,
    CHAROID      = 18,
    INT2OID      = 21,
    INT4OID      = 23,
    INT8OID      = 20,
    TEXTOID      = 25,
    FLOAT4OID    = 700,
    FLOAT8OID    = 701,
    VARCHAROID   = 1043,
    DATEOID      = 1082,
    TIMEOID      = 1083,
    TIMESTAMPOID = 1114,
    NUMERICOID   = 1700
};

void db_exit(PGconn * conn)
{
    PQfinish(conn);
    exit(1);
}

PGconn * db_connect(void)
{
    const char *conninfo = "dbname = sa";
    PGconn * conn = PQconnectdb(conninfo);
    /* Check to see that the backend connection was successfully made */
    if (PQstatus(conn) != CONNECTION_OK) {
        fprintf(stderr, "Connection to database failed: %s",
            PQerrorMessage(conn));
        db_exit(conn);
    }
    return conn;
} 

// does not return anything
int db_command(PGconn * conn, char * qstring)
{
    PGresult *res = PQexec(conn, qstring);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "command failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        db_exit(conn);
    }
    return 0;
}

// returns tuple resutls
PGresult * db_query(PGconn * conn, char * qstring)
{
    PGresult *res = PQexec(conn, qstring);
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        fprintf(stderr, "query failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        db_exit(conn);
    }
    return res;
}

void db_display(PGresult * res)
{
    int n_fields;
    int i, j;

    /* first, print out the attribute names */
    n_fields = PQnfields(res);
    for (i = 0; i < n_fields; i++)
        printf(COLOR_BOLD_CYAN "%-15s" COLOR_RESET, PQfname(res, i));

    printf("\n--------------------------------------------------------------------------------------\n");

    /* next, print out the rows */
    for (i = 0; i < PQntuples(res); i++) {
        for (j = 0; j < n_fields; j++)
            printf("%-15s", PQgetvalue(res, i, j));
        printf("\n");
    }
    PQclear(res);
}

void db_analyze(PGresult * res)
{
    int n_fields;
    int row, col;

    n_fields = PQnfields(res);
    for (row = 0; row < PQntuples(res); row++) {
        for (col = 0; col < n_fields; col++) {
            if (PQgetisnull(res, row, col)) {
                // value is NULL, nothing more to do
                printf("field: %s -> NULL\n", PQfname(res, col));
            } 
            else {
                char * value  = PQgetvalue(res, row, col);
                //int    length = PQgetlength(res, row, col);

                switch (PQftype(res, col)) {

                    case INT2OID:
                    case INT4OID:
                    case INT8OID:
                        // process value as an integer
                        printf("int %s -> %s\n",
                            PQfname(res, col), value);
                        break;

                    case FLOAT4OID:
                    case FLOAT8OID:
                    case NUMERICOID:
                        // process value as an float
                        printf("float %s -> %s\n",
                            PQfname(res, col), value);
                        break;

                    case DATEOID:
                    case TIMEOID:
                    case TIMESTAMPOID:
                        // process value as an datetime obj
                        printf("datetime %s -> %s\n",
                            PQfname(res, col), value);
                        break;

                    default:
                        // just default to a text representation
                        printf("str %s -> %s\n",
                            PQfname(res, col), value);
                        break;
                }
            }
        }
    }
    PQclear(res);
}

int main(int argc, char **argv)
{
    char * qstring;

    if (argc > 1)
        qstring = argv[1];
    else
        qstring = "select * from user";

    PGconn  * conn = db_connect();
    PGresult * res = db_query(conn, qstring);
    db_display(res);
    //db_analyze(res);
    db_exit(conn);

    return 0;
}
