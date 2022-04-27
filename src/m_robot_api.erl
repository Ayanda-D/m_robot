-module(m_robot_api).

-export([grid/2, move/5, list/0]).

grid(X, Y) ->
    application:set_env(m_robot, m_robot_grid_size, {X, Y}).

move(RobotId, _X, _Y, _Orientation, []) ->
    m_robot_statem:info(RobotId);
move(RobotId, X, Y, Orientation, Commands) when is_list(Commands) ->
    {ok, Speed} = application:get_env(m_robot, m_robot_speed),
    _Start = m_robot_statem:set_origin(RobotId, X, Y, Orientation),
    Results =
        [begin
            timer:sleep(Speed),
            m_robot_statem:move(RobotId, [Command])
        end || Command <- Commands],
    lists:last(Results).

list() -> supervisor:which_children(m_robot_sup).