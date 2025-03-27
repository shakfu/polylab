#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DATA 512
#define MAX_ROWS 100

typedef struct Contact {
    int id;
    int set;
    char name[MAX_DATA];
    char email[MAX_DATA];
} Contact;

typedef struct Database {
    Contact rows[MAX_ROWS];
} Database;

typedef struct Connection {
    FILE* file;
    Database* db;
} Connection;

void die(const char* message)
{
    if (errno) {
        perror(message);
    } else {
        printf("ERROR: %s\n", message);
    }

    exit(1);
}

void Contact_print(Contact* contact)
{
    printf("%d %s %s\n", contact->id, contact->name, contact->email);
}

void Database_load(Connection* conn)
{
    int rc = fread(conn->db, sizeof(Database), 1, conn->file);
    if (rc != 1)
        die("Failed to load database.");
}

Connection* Database_open(const char* filename, char mode)
{
    Connection* conn = malloc(sizeof(Connection));
    if (!conn)
        die("Memory error");

    conn->db = malloc(sizeof(Database));
    if (!conn->db)
        die("Memory error");

    if (mode == 'c') {
        conn->file = fopen(filename, "w");
    } else {
        conn->file = fopen(filename, "r+");

        if (conn->file) {
            Database_load(conn);
        }
    }

    if (!conn->file)
        die("Failed to open the file");

    return conn;
}

void Database_close(Connection* conn)
{
    if (conn) {
        if (conn->file)
            fclose(conn->file);
        if (conn->db)
            free(conn->db);
        free(conn);
    }
}

void Database_write(Connection* conn)
{
    rewind(conn->file);

    int rc = fwrite(conn->db, sizeof(Database), 1, conn->file);
    if (rc != 1)
        die("Failed to write database.");

    rc = fflush(conn->file);
    if (rc == -1)
        die("Cannot flush database.");
}

void Database_create(Connection* conn)
{
    int i = 0;

    for (i = 0; i < MAX_ROWS; i++) {
        // make a prototype to initialize it
        Contact contact = { .id = i, .set = 0 };
        // then just assign it
        conn->db->rows[i] = contact;
    }
}

void Database_set(Connection* conn, int id, const char* name,
                  const char* email)
{
    Contact* contact = &conn->db->rows[id];
    if (contact->set)
        die("Already set, delete it first");

    contact->set = 1;
    // WARNING: bug, read the "How To Break It" and fix this
    char* res = strncpy(contact->name, name, MAX_DATA);
    // demonstrate the strncpy bug
    if (!res)
        die("Name copy failed");

    res = strncpy(contact->email, email, MAX_DATA);
    if (!res)
        die("Email copy failed");
}

void Database_get(Connection* conn, int id)
{
    Contact* contact = &conn->db->rows[id];

    if (contact->set) {
        Contact_print(contact);
    } else {
        die("ID is not set");
    }
}

void Database_delete(Connection* conn, int id)
{
    Contact contact = { .id = id, .set = 0 };
    conn->db->rows[id] = contact;
}

void Database_list(Connection* conn)
{
    int i = 0;
    Database* db = conn->db;

    for (i = 0; i < MAX_ROWS; i++) {
        Contact* cur = &db->rows[i];

        if (cur->set) {
            Contact_print(cur);
        }
    }
}

int main(int argc, char* argv[])
{
    if (argc < 3)
        die("USAGE: ex17 <dbfile> <action> [action params]");

    char* filename = argv[1];
    char action = argv[2][0];
    Connection* conn = Database_open(filename, action);
    int id = 0;

    if (argc > 3)
        id = atoi(argv[3]);
    if (id >= MAX_ROWS)
        die("There's not that many records.");

    switch (action) {
    case 'c':
        Database_create(conn);
        Database_write(conn);
        break;

    case 'g':
        if (argc != 4)
            die("Need an id to get");

        Database_get(conn, id);
        break;

    case 's':
        if (argc != 6)
            die("Need id, name, email to set");

        Database_set(conn, id, argv[4], argv[5]);
        Database_write(conn);
        break;

    case 'd':
        if (argc != 4)
            die("Need id to delete");

        Database_delete(conn, id);
        Database_write(conn);
        break;

    case 'l':
        Database_list(conn);
        break;
    default:
        die("Invalid action, only: c=create, g=get, s=set, d=del, l=list");
    }

    Database_close(conn);

    return 0;
}