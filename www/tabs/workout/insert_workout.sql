insert into kip.workout (start_dttm, workout_group_id, program_id) 
values ({start_dttm}, {workout_group_id}, {program_id})
returning id;

