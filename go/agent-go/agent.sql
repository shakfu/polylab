
drop table if exists mission cascade;
create table mission (
    id serial primary key,
    name text,
    keywords text
);

drop table if exists target cascade;
create table target (
    id serial primary key,
    mission_id int references mission(id),
    name text,
    description text,
    url text,
    keywords text,
    category text,
    relevance float
);

drop table if exists entry;
create table entry (
    id serial primary key,
    target_id int references target(id),
    link text,
    url text,
    category text,
    relevance float
);

insert into mission (id, name, keywords) values
    (1, 'mideast', 'syria, peace, turkey'),
    (2, 'it'     , 'apple, postgres');

insert into target (id, mission_id, name, url, keywords) values
    (1, 1, 'news.google.com' , 'http://news.google.com/?output=rss', 'saudi, syria'),
    (2, 1, 'guardian - saudi', 'http://www.theguardian.com/world/saudiarabia/rss', 'saudi, syria'),
    (3, 2, 'hacker news', 'http://news.ycombinator.com/rss', 'python, golang');

