CREATE FUNCTION add_one(integer) RETURNS integer
     AS '/home/sa/pvm/lang/c/pg_func/example', 'add_one'
     LANGUAGE C STRICT;

-- note overloading of SQL function name "add_one"
CREATE FUNCTION add_one(double precision) RETURNS double precision
     AS '/home/sa/pvm/lang/c/pg_func/example', 'add_one_float8'
     LANGUAGE C STRICT;

CREATE FUNCTION makepoint(point, point) RETURNS point
     AS '/home/sa/pvm/lang/c/pg_func/example', 'makepoint'
     LANGUAGE C STRICT;

CREATE FUNCTION copytext(text) RETURNS text
     AS '/home/sa/pvm/lang/c/pg_func/example', 'copytext'
     LANGUAGE C STRICT;

CREATE FUNCTION concat_text(text, text) RETURNS text
     AS '/home/sa/pvm/lang/c/pg_func/example', 'concat_text'
     LANGUAGE C STRICT;
