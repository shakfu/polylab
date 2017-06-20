#include <stdio.h>
#include <stdlib.h>

#include <libpq-fe.h>

static void exit_nicely(PGconn * conn)
{
    PQfinish(conn);
    exit(1);
}

void test_db(){
    PGconn *conn;
    PGresult *res;
    int nFields;
    int i, j;

    /* Make a connection to the database */
    conn = PQconnectdb("dbname=sa user=sa password=sa");
    /* Check to see that the backend connection was successfully made */
    if (PQstatus(conn) != CONNECTION_OK) {
        fprintf(stderr, "Connection to database failed: %s",
            PQerrorMessage(conn));
        exit_nicely(conn);
    } else {
        printf("connected to db\n");
    }

    /* exec a query */
    res = PQexec(conn, "select * from users");
    // printf("PQresultStatus: %d\nPGRES_COMMAND_OK: %d\n", 
    //     PQresultStatus(res), PGRES_COMMAND_OK);

    // if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    //     fprintf(stderr, "query failed: %s\n",
    //         PQerrorMessage(conn));
    //     PQclear(res);
    //     exit_nicely(conn);
    // }

    /* check fields */
    nFields = PQnfields(res);
    for (i = 0; i < nFields; i++)
        printf("%-15s", PQfname(res, i));
    printf("\n\n");

    /* next, print out the rows */
    for (i = 0; i < PQntuples(res); i++) {
        for (j = 0; j < nFields; j++)
            printf("%-15s", PQgetvalue(res, i, j));
        printf("\n");
    }

    /* close out */
    PQclear(res);
    exit_nicely(conn);
}

