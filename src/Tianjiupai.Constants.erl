-module('Tianjiupai.Constants').
-export(['disconnection_timeout'/0, 'maximum_num_innings'/0, 'room_expiration_timeout'/0, 'user_expiration_timeout'/0]).
'disconnection_timeout'() -> 30000.
'maximum_num_innings'() -> 8.
'room_expiration_timeout'() -> (1000 * (60 * (60 * 24))).
'user_expiration_timeout'() -> (1000 * (60 * (60 * 24))).
