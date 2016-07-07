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
        
        cout << "# of employees: " << employees.size() << endl;
                    
        for(auto employee : employees)
        {
            cout 
                << employee["id"].as<string>()   << "\t"
                << employee["salary"].as<int>()  << endl;
        }    
    }
    catch (const std::exception &e)
    {
        cerr << e.what() << endl;
        return 1;
    }

}
