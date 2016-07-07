
drop table if exists person;

create table person (
    id serial primary key,
    name varchar,
    age int
);

insert into person(name, age) values ('sam', 10);

