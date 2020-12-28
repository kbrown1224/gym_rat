select *
from (
    select date(start_dttm) as workout_date, repetitions, weight, 'most_weight' as category
    from workout w
    left join lift l on w.id = l.workout_id
    left join set s on l.id = s.lift_id
    where exercise_id = {exercise_id}
    order by weight desc, start_dttm
    limit 1
) as most_weight_query

union

select *
from (
    select date(start_dttm) as workout_date, repetitions, weight, 'most_reps' as category
    from workout w
    left join lift l on w.id = l.workout_id
    left join set s on l.id = s.lift_id
    where exercise_id = {exercise_id}
    order by repetitions desc, start_dttm
    limit 1
) as most_reps_query;