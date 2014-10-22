select * from bookings a
join tours b
on a.tour_id = b.id
join users c
on a.tourist_id = c.id
join booking_slots d
on a.booking_slot_id = d.id
where a.state = 'Taken';


