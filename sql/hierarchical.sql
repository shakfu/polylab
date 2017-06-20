create extension if not exists ltree;

drop type if exists jobfamily cascade;
create type jobfamily as enum (
    'Senior Executive',
    'Executive',
    'Manager',
    'Engineer',
    'Professional',
    'Technician',
    'Administrator',
    'Skilled Labor',
    'Semi-skilled Labor',
    'Unskilled Labor',
    'Trainee'
);

drop table if exists employee cascade;
create table employee (
    id          integer primary key,
    name        text
);

drop table if exists job cascade;
create table job (
    id          text primary key,
    family      jobfamily not null,
    name        text not null
);

drop table if exists position cascade;
create table position (
    id          integer primary key,
    reports_to  integer references position(id),
    employee_id integer references employee(id),
    org_id      integer not null,
    jobcode     text references job(id) not null,
    name        text not null
);



drop table if exists org cascade;
create table org (
    id          integer primary key,
    parent_id   integer references org(id),
    shortname   text not null,
    parent_path LTREE,
    name        text unique,
    managed_by  integer references position(id)
);


create index org_parent_path_idx on org using gist (parent_path);
-- create index org_parent_id_idx on org (parent_id);
alter table position add constraint position_org_fk foreign key (org_id) references org;


insert into employee values(1, 'John Smith');
insert into employee values(2, 'Rajiv Patel');
insert into employee values(3, 'Khaled Hasan');
insert into employee values(4, 'Larry Anderson');

insert into job values('J1', 'Senior Executive', 'Chairman');
insert into job values('J2', 'Senior Executive', 'Director');
insert into job values('J3', 'Executive', 'GM');
insert into job values('J4', 'Executive', 'DGM');

insert into org values(1, 1, 'bg',  'bg',        'Business Group');
insert into org values(2, 1, 'v1',  'bg.v1',     'Industry Vertical Group');
insert into org values(3, 1, 'bu1', 'bg.v1.bu1', 'Business Unit');

insert into position values(1, 1, 1, 1, 'J1', 'Chairman of the Board');
insert into position values(2, 1, 2, 2, 'J2', 'Director, Industry Vertical Group');
insert into position values(3, 2, 3, 3, 'J3', 'General Manager');
insert into position values(4, 3, 4, 3, 'J4', 'Deputy General Manager');


-- metrics

drop table if exists metric;
create table metric (
    id          serial primary key,
    name        text unique,
    parent_id   integer references metric(id) default null
);

insert into metric values(
    default,
    'sa',
    default
);

create extension if not exists ltree;

drop table if exists person;
create table person (
    id          serial primary key,
    name        text unique,
    parent      LTREE
);

insert into person values(
    default,
    'sa',
    default
);

create extension if not exists ltree;

drop table if exists org cascade;
create table org (
    id          serial primary key,
    parent_id   integer references org(id) default null,
    name        text unique,
    parent_path LTREE
);

create index org_parent_path_idx on org using gist (parent_path);
create index org_parent_id_idx on org (parent_id);

create or replace function update_org_parent_path() returns trigger as $$
    declare
        path ltree;
    begin
        if new.parent_id is null then
            new.parent_path = 'root'::ltree;
        elseif tg_op = 'insert' or old.parent_id is null or old.parent_id != new.parent_id then
            select parent_path || id::text from org where id = new.parent_id into path;
            if path is null then
                raise exception 'Invalid parent_id %', new.parent_id;
            end if;
            new.parent_path = path;
        end if;
        return new;
    end;
$$ language plpgsql;

create trigger parent_path_tgr
    before insert or update on org
    for each row execute procedure update_org_parent_path();
