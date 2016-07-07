#include <iostream>
#include <pqxx/pqxx>

using namespace pqxx;

int main(int, char *argv[])
{
  connection c("dbname=sa user=sa password=sa");
  work txn(c);

  result r = txn.exec(
    "SELECT pass "
    "FROM users "
    "WHERE login =" + txn.quote(argv[1]));

  if (r.size() != 1)
  {
    std::cerr
      << "Expected 1 user with login " << argv[1] << ", "
      << "but found " << r.size() << std::endl;
    return 1;
  }

  std::cout << "password: " << r[0][0].c_str() << std::endl;

}