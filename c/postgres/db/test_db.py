from db import Query

q = Query("select * from user")
q.test()
q.close()

