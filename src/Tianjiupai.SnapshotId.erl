-module('Tianjiupai.SnapshotId').
-export(['generate'/0, 'equal'/2]).
    generate() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'equal'(S1076S1, S1077S2) -> 'SesterlStdlib.Binary':'equal'(S1076S1, S1077S2).
