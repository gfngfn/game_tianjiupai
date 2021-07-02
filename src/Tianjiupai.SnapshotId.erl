-module('Tianjiupai.SnapshotId').
-export(['generate'/0, 'equal'/2]).

    generate() ->
        Uuid = uuid:get_v4(),
        list_to_binary(uuid:uuid_to_string(Uuid)).
  
'equal'(S318S1, S319S2) -> 'SesterlStdlib.Binary':'equal'(S318S1, S319S2).
