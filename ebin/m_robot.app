{application, 'm_robot', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['m_robot_api','m_robot_app','m_robot_statem','m_robot_sup']},
	{registered, [m_robot_sup]},
	{applications, [kernel,stdlib]},
	{mod, {m_robot_app, []}},
	{env, [
  {m_robot_total,  10},
  {m_robot_grid_size,  {5, 5}},
  {m_robot_current_position,  {0, 0}},
  {m_robot_speed,  500}
     ]}
]}.