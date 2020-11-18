create schema kip;
set search_path to kip;

create table workout (
    id serial primary key not null,
    start_dttm timestamp default current_timestamp not null,
    end_dttm timestamp null,
    workout_group_id int not null,
    program_id int not null
);

create table lift (
    id serial primary key not null,
    workout_id int not null,
    exercise_id int not null,
    constraint fk_workout foreign key(workout_id) references workout(id)
);

create table set (
    id serial primary key not null,
    lift_id int not null,
    set_number int null,
    repetitions int null,
    distance float4 null,
    duration float4 null,
    constraint fk_lift foreign key(lift_id) references lift(id)
);

create table muscle_group (
    id serial primary key not null,
    name varchar(500) not null
);

create table exercise (
    id serial primary key not null,
    name varchar(500) not null,
    muscle_group_id int not null,
    primary_lift bool not null,
    constraint fk_muscle_group foreign key(muscle_group_id) references muscle_group(id)
);

create table workout_group (
    id int not null,
    muscle_group_id int not null,
    constraint fk_muscle_group foreign key(muscle_group_id) references muscle_group(id)
);

create table program (
    id serial primary key not null,
    name varchar(500) not null
);

create table program_steps (
    id serial primary key not null,
    program_id int not null,
    exercise_id int not null,
    set_goal int not null,
    rep_goal int not null,
    constraint fk_program foreign key(program_id) references program(id),
    constraint fk_exercise foreign key(exercise_id) references exercise(id)
);

create table personal_records (
    id serial primary key not null,
    exercise_id int not null,
    record_dttm timestamp default current_timestamp,
    weight int not null,
    constraint fk_exercise foreign key(exercise_id) references exercise(id)
);

alter table lift add constraint fk_exercise foreign key(exercise_id) references exercise(id);

create table primary_lift_program (
    workout_number int not null,
    step int not null,
    sets int not null,
    reps int not null,
    pr_percentage float4 not null
);

insert into muscle_group (name)
values ('Chest'), ('Back'), ('Shoulders'), ('Biceps'), ('Triceps'), ('Legs'), ('Abs'), ('Cardio');

insert into workout_group (id, muscle_group_id)
values
(1, 1), (1, 5), (1, 7),
(2, 2), (2, 4), (2, 8),
(3, 3), (3, 6), (3, 7),
(4, 7), (4, 8);

insert into exercise (name, muscle_group_id, primary_lift)
values
('Bench Press', 1, true),
('Incline Bench Press', 1, false),
('DB Bench Press', 1, false),
('Incline DB Bench Press', 1, false),
('Band Flies', 1, false),
('Plate Squeeze N Press', 1, false),
('Overhead BB Extension', 5, false),
('DB Kick Backs', 5, false),
('Band Pull Downs', 5, false),
('Overhead 1 Arm DB Extensions', 5, false),
('Planks', 7, false),
('Crunches', 7, false),
('Weighted Decline Sit Ups', 7, false);

select * from exercise;



