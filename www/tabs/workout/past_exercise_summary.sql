select
    l.exercise_id,
    repetitions,
    max(weight) as max_weight,
    min(weight) as min_weight
from set s
left join lift l on l.id = s.lift_id
where l.exercise_id in  {exercise_ids}
group by l.exercise_id, repetitions;