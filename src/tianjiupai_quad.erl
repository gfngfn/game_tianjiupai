-module(tianjiupai_quad).

%%====================================================================================================
%% Exported API
%%====================================================================================================
-export_type([
    seat/0,
    quad/1
]).
-export([
    advance_seat/2,
    zip/3,
    map/2,
    find/2,
    access/2,
    update/3
]).

%%====================================================================================================
%% Macros & Types
%%====================================================================================================
-type seat() :: 0 | 1 | 2 | 3.

-type quad(X) :: {X, X, X, X}.

%%====================================================================================================
%% Exported Functions
%%====================================================================================================
-spec advance_seat(seat(), integer()) -> seat().
advance_seat(Seat, N) ->
    Rem = (Seat + N) rem 4,
    if
        Rem < 0 -> Rem + 4;
        true    -> Rem
    end.

-spec zip(F, quad(X), quad(Y)) -> quad(Z) when
    F :: fun((X, Y) -> Z),
    X :: term(),
    Y :: term(),
    Z :: term().
zip(F, {X0, X1, X2, X3}, {Y0, Y1, Y2, Y3}) ->
    {F(X0, Y0), F(X1, Y1), F(X2, Y2), F(X3, Y3)}.

-spec map(F, quad(X)) -> quad(Y) when
    F :: fun((X) -> Y),
    X :: term(),
    Y :: term().
map(F, {X0, X1, X2, X3}) ->
    {F(X0), F(X1), F(X2), F(X3)}.

-spec find(F, quad(X)) -> {ok, {X, seat()}} | error when
    F :: fun((X) -> boolean()),
    X :: term().
find(F, Quad) ->
    {X0, X1, X2, X3} = Quad,
    case {F(X0), F(X1), F(X2), F(X3)} of
        {true, _, _, _} -> {ok, {X0, 0}};
        {_, true, _, _} -> {ok, {X1, 1}};
        {_, _, true, _} -> {ok, {X2, 2}};
        {_, _, _, true} -> {ok, {X3, 3}};
        _               -> error
    end.

-spec access(seat(), quad(X)) -> X when
    X :: term().
access(Seat, {X0, X1, X2, X3}) ->
    case Seat of
        0 -> X0;
        1 -> X1;
        2 -> X2;
        3 -> X3
    end.

-spec update(seat(), X, quad(X)) -> quad(X) when
    X :: term().
update(Seat, XNew, {X0, X1, X2, X3}) ->
    case Seat of
        0 -> {XNew, X1, X2, X3};
        1 -> {X0, XNew, X2, X3};
        2 -> {X0, X1, XNew, X3};
        3 -> {X0, X1, X2, XNew}
    end.
