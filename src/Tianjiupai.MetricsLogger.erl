-module('Tianjiupai.MetricsLogger').
-export(['put_impl'/1, 'put'/3]).
    put_impl(Msg) ->
        logger:log(info, Msg, [], #{ message_only => "yes", file => "", line => 0 }).
  
'put'(S472NumRooms, S473NumUsers, S471Timestamp) -> begin S474Data = sesterl_internal_prim:'format'({"{\"_aws\": {\"Timestamp\": ~p, \"CloudWatchMetrics\": [{\"Namespace\": \"game-server-metrics\", \"Dimensions\": [[\"functionVersion\"]], \"Metrics\": [{\"Name\": \"num_rooms\": \"Unit\": \"Count\"}, {\"Name\": \"num_users\", \"Unit\": \"Count\"}]}]}, \"functionVersion\": \"$LATEST\", \"num_rooms\": ~p, \"num_users\": ~p}", 3}, {S471Timestamp, S472NumRooms, S473NumUsers}), 'Tianjiupai.MetricsLogger':'put_impl'(S474Data) end.
