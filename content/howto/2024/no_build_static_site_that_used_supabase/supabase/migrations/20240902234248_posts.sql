create table posts (
  id bigint generated by default as identity primary key,
  title text,
  body text,
  user_id uuid references auth.users,
  created_at timestamp with time zone default now(),
  updated_at timestamp with time zone default now()
);

-- 2. Enable RLS
alter table posts enable row level security;

-- select policy
create policy "Posts are visible to everyone."
on posts for select
to anon, authenticated -- the Postgres Role (recommended)
using ( true ); -- the actual Policy

-- insert policy
create policy "Users can create a post."
on posts for insert
to authenticated
with check ( auth.uid() is not null );
