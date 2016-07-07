#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <postgresql/libpq-fe.h>

using namespace std;

struct Person {
    string name;
    int age;
};

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
    
    puts("==========================");

    //~ for (row=0; row<rec_count; row++) {
        //~ for (col=0; col<67; col++) {
            //~ printf("%s\t", PQgetvalue(res, row, col));
        //~ }
        //~ puts("");
    //~ }
    
    for (row=0; row<rec_count; row++) {
        auto value = PQgetvalue(res, row, 18);
        // printf("%s\t", value);
        salary += atof(value);
        // puts("");
    }

    puts("==========================");
    cout << "total salary / month: " << thousands(salary) << endl;
    //printf("total salary is: %f\n", salary);

    PQclear(res);

    PQfinish(conn);
}




int main()
{
    int x = 3;
    auto y = x;    
    
    Person p{"sam", 17};

    vector<int> vec;
    vec.push_back(x);
    vec.push_back(10);

    for (int &i : vec)
    {
        cout << i << endl;
    }
    
    cout << p.name << endl;
    cout << p.age << endl;

    test_pg();
    
    //~ cout << "a number: " << commas(intToString(100212001)) << endl;

}

