PROJECT = m_robot
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

define PROJECT_ENV
[
  {m_robot_total,  10},
  {m_robot_grid_size,  {5, 5}},
  {m_robot_speed,  500}
     ]
endef

include erlang.mk
