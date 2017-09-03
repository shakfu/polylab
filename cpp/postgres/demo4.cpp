#include <iostream>
#include <pqxx/pqxx>

using namespace std;

//const char *connection_string = "dbname=sa host=localhost user=sa password=sa";

#define CONNECTION "dbname=sa host=localhost user=sa password=sa"

pqxx::result get_staff()
{
    pqxx::connection c(CONNECTION);
    pqxx::work txn(c);
    pqxx::result r = txn.exec("select * from staff");
    txn.commit();
    return r;
}

void check_employees(pqxx::result employees) {
    for(auto employee : employees)
        {
            cout
                << employee["id"].as<string>()   << "\t"
                << employee["salary"].as<int>()  << endl;
        }
}

int main(int, char *argv[])
{
    try
    {
        pqxx::result employees = get_staff();

        //check_employees(employees);

        cout << "# of employees: " << employees.size() << endl;
    }
    catch (const std::exception &e)
    {
        cerr << e.what() << endl;
        return 1;
    }

}
