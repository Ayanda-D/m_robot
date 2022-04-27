-module(m_robot_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, N_Robots} = application:get_env(m_robot, m_robot_total),
	Procs = [robot_procs(RobotId) || RobotId <- lists:seq(1, N_Robots)],
	{ok, {{one_for_one, 1, 5}, lists:flatten(Procs)}}.

robot_procs(Id) ->
	ProcId = lists:concat(["m_robot_",  integer_to_list(Id)]),
	[{ProcId,
		{m_robot_statem, start_link, [list_to_atom(ProcId)]},
		transient, 16#ffffffff, worker,
		[m_robot_statem]}].
