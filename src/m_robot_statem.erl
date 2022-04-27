-module(m_robot_statem).
-behaviour(gen_statem).

-export([start_link/1, set_origin/4, move/2, info/1, stop/1]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([origin/3, ready/3]).

-record(state, {x_pos = 0, y_pos = 0, orientation = 0, location_state = 'NOT_LOST'}).

start_link(RobotId) ->
    gen_statem:start_link({local, RobotId}, ?MODULE, [], []).

set_origin(RobotId, X, Y, Orientation) ->
    O_Degrees = orientation_to_degrees(Orientation),
    gen_statem:call(RobotId, {origin, X, Y, O_Degrees}).
move(RobotId, Command) ->
    gen_statem:call(RobotId, {move, Command}).
info(RobotId) ->
    gen_statem:call(RobotId, info).
stop(RobotId) ->
    gen_statem:stop(RobotId).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([]) ->
    StateM = origin,
    {ok, StateM, #state{}}.
callback_mode() -> state_functions.

%%% state callback(s)
origin({call,From}, {origin, X, Y, O_Degrees}, State = #state{location_state = LocState}) ->
    OriginState = 
        State#state{x_pos = X, y_pos = Y, orientation = O_Degrees},
    {next_state, ready , OriginState, [{reply, From, {X, Y, degrees_to_orientation(O_Degrees), LocState}}]};
origin(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

ready({call,From}, {move, Direction}, S = #state{x_pos = X, y_pos = Y, orientation = O_Degrees}) ->
    {ok, Grid = {_X_Grid, _Y_Grid}} = application:get_env(m_robot, m_robot_grid_size),
    {LocState, {X_New, Y_New, O_Degrees_New}} = calc_pos({X, Y}, Direction, O_Degrees, Grid),

    NewState = 
        S#state{x_pos = X_New, y_pos = Y_New, orientation = O_Degrees_New, location_state = LocState},

    {next_state, ready , NewState, [{reply, From, {X_New, Y_New, degrees_to_orientation(O_Degrees_New), LocState}}]};
ready({call,From}, {origin, X, Y, O_Degrees}, State = #state{location_state = LocState}) ->
    OriginState = 
        State#state{x_pos = X, y_pos = Y, orientation = O_Degrees},
    {next_state, ready , OriginState, [{reply, From, {X, Y, degrees_to_orientation(O_Degrees), LocState}}]};
ready(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, info, State = #state{x_pos = X, y_pos = Y, orientation = O_Degrees, location_state = LocState}) ->
    {keep_state, State, [{reply, From, {X, Y, degrees_to_orientation(O_Degrees), LocState}}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

orientation_to_degrees("N") -> 0;
orientation_to_degrees("E") -> 90;
orientation_to_degrees("W") -> 270;
orientation_to_degrees("S") -> 180.

degrees_to_orientation(0) -> "N";
degrees_to_orientation(90) -> "E";
degrees_to_orientation(270) -> "W";
degrees_to_orientation(180) -> "S".

calc_pos({X, Y}, "L", _O_Degrees = 0, _Grid) -> {'NOT_LOST', {X, Y, 270}};
calc_pos({X, Y}, "L", O_Degrees, _Grid) -> {'NOT_LOST', {X, Y, O_Degrees - 90}};
calc_pos({X, Y}, "R", _O_Degrees = 270, _Grid) -> {'NOT_LOST', {X, Y, 0}};
calc_pos({X, Y}, "R", O_Degrees, _Grid) -> {'NOT_LOST', {X, Y, O_Degrees + 90}};

calc_pos({X, Y}, "F", O_Degrees = 0, {_X_Grid, Y_Grid}) ->
    Y_Candidate = Y + 1,
    {State, Y_New} = check_grid(Y_Candidate, Y_Grid),
    {State, {X, Y_New, O_Degrees}};
calc_pos({X, Y}, "F", O_Degrees = 90, {X_Grid, _Y_Grid}) ->
    X_Candidate = X + 1,
    {State, X_New} = check_grid(X_Candidate, X_Grid),
    {State, {X_New, Y, O_Degrees}};
calc_pos({X, Y}, "F", O_Degrees = 270, {X_Grid, _Y_Grid}) ->
    X_Candidate = X - 1,
    {State, X_New} = check_grid(X_Candidate, X_Grid),
    {State, {X_New, Y, O_Degrees}};
calc_pos({X, Y}, "F", O_Degrees = 180, {_X_Grid, Y_Grid}) ->
    Y_Candidate = Y - 1,
    {State, Y_New} = check_grid(Y_Candidate, Y_Grid),
    {State, {X, Y_New, O_Degrees}}.

check_grid(X_or_Y, X_or_Y_Grid) ->
    if (X_or_Y > X_or_Y_Grid) -> {'LOST', X_or_Y_Grid};
       (X_or_Y < 0) -> {'LOST', 0};
       true -> {'NOT_LOST', X_or_Y}
    end.