create table posts(id integer primary key, string_id text);
create table readers(
  id integer primary key, 
  ip_address text,
  user_agent text 
);
create table likes(
  id integer primary key, 
  reader_key__id integer, 
  post_key__id integer,
  foreign key(reader_key__id) references readers(id) on delete cascade,
  foreign key(post_key__id) references posts(id) on delete cascade
);


create unique index unique_posts on posts(string_id);
create unique index unique_readers on readers(ip_address, user_agent);
create index likes_post_index on likes(post_key__id);
create index likes_reader_index on likes(reader_key__id);
