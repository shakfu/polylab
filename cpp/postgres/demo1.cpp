#include <iostream>
#include <string>
#include <sstream>
#include <postgresql/libpq-fe.h>
#include <locale.h>

using namespace std;

char *commaprint(unsigned long n)
{
	static int comma = '\0';
	static char retbuf[30];
	char *p = &retbuf[sizeof(retbuf)-1];
	int i = 0;

	if(comma == '\0') {
		struct lconv *lcp = localeconv();
		if(lcp != NULL) {
			if(lcp->thousands_sep != NULL &&
				*lcp->thousands_sep != '\0')
				comma = *lcp->thousands_sep;
			else	comma = ',';
		}
	}

	*p = '\0';

	do {
		if(i%3 == 0 && i != 0)
			*--p = comma;
		*--p = '0' + n % 10;
		n /= 10;
		i++;
	} while(n != 0);

	return p;
}

string intToString( long int n )
{
	std::ostringstream result;
	result << n;
	return result.str();
}

std::string commas(string num)
{
	string temp;
	int endstring = num.length(), i;
	for( i = endstring - 3; i >= 0; i -= 3) {
		if (i > 0) {
			temp = ","+ num.substr(i, 3) + temp;
		} else {
			temp = num.substr(i, 3) + temp;
		}		
	}
	if (i < 0) {
		temp = num.substr(0, 3+i) + temp;
	}
	return temp;
}

string thousands(long int n)
{
    return commas(intToString(n));
}

void test_pg() {
    PGconn      *conn;
    PGresult    *res;
    int         rec_count;
    int         row;
    int         col;
    double      salary = 0.0;
    
    conn = PQconnectdb("dbname=sa host=localhost user=sa password=sa");
    
    if (PQstatus(conn) == CONNECTION_BAD) {
        puts("We were unable to connect to the database");
        exit(0);
    }
    
    res = PQexec(conn, "select * from staff");
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        puts("We did not get any data!");
        exit(0);
    }
    
    rec_count = PQntuples(res);
    printf("We received %d records.\n", rec_count);
    
    for (row=0; row<rec_count; row++) {
        auto value = PQgetvalue(res, row, 18);
        salary += atof(value);
    }

    cout << "total salary / month: " << thousands(salary) << endl;

    PQclear(res);

    PQfinish(conn);
}


int main()
{
    test_pg();
    printf("number: %s\n", commaprint(10021212));

}

