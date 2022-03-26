-module('Tianjiupai.Constants').
-export(['disconnection_timeout'/0, 'maximum_num_innings'/0, 'room_expiration_timeout'/0, 'user_expiration_timeout'/0, 'maximum_num_rooms'/0, 'maximum_num_users'/0, 'maximum_num_rooms_per_user'/0, 'metrics_interval'/0]).
'disconnection_timeout'() -> 30000.
'maximum_num_innings'() -> 8.
'room_expiration_timeout'() -> (1000 * (60 * 60)).
'user_expiration_timeout'() -> (1000 * (60 * 60)).
'maximum_num_rooms'() -> 1000.
'maximum_num_users'() -> 1000.
'maximum_num_rooms_per_user'() -> 10.
'metrics_interval'() -> (1000 * 60).
