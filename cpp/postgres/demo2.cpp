#include <iostream>
#include <pqxx/pqxx>

using namespace std;


pqxx::result get_staff()
{
    pqxx::connection c("dbname=sa host=localhost user=sa password=sa");
    pqxx::work txn(c);
    pqxx::result r = txn.exec("select * from staff");
    txn.commit();
    return r;
}


int main(int, char *argv[])
{
    try
    {
        pqxx::result employees = get_staff();

        if (employees.size() <= 1)
        {
            cerr << "Expected more than 1 employee " << endl;
            return 1;
        }

        cout << "# of employees: " << employees.size() << endl;

        for(pqxx::result::const_iterator row = employees.begin();
            row != employees.end();
            ++row)
        {
            cout
                << row["id"].as<string>()   << "\t"
                << row["salary"].as<int>()  << endl;
        }
    }
    catch (const std::exception &e)
    {
        cerr << e.what() << endl;
        return 1;
    }

}
