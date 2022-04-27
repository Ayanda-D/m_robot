# Mars Robot

A Mars Robot Control simulator. Each robot is modelled as state machine which receives and processes different commands to control it's movements and direction orientation.

## Get started!

Install [Erlang 23](https://www.erlang.org/doc/index.html) or later.

Clone the project repo and run `make run` as follows:

```
git clone https://github.com/Ayanda-D/m_robot

cd m_robot

make run

```

This will start a node session:

```
Erlang/OTP 24 [erts-12.3.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V12.3.1  (abort with ^G)
(m_robot@127.0.0.1)1>
```
 
 
## Notes
 
- The program reads the input command, update the robots, and print out the final states of the robots

- Each robot has a position (x, y), and an orientation (N, E, S, W)
 
- Each robot can move forward one space (F), rotate left by 90 degrees (L), or rotate right by 90 degrees (R)

- If a robot moves off the grid, it is marked as `lost` and its last valid grid position and orientation is recorded

- Going from `x->x+1` is in the easterlydirection, and `y->y+1` is in the northerly direction. i.e. (0, 0) represents the south-west corner of the grid 


## Configuration

The application can be configured with the following configs:

- `m_robot_total` - Total number of robots. e.g. `100`
- `m_robot_grid_size` - Grid size, e.g. `{5, 5}`
- `m_robot_speed` - Movement interval speed in milliseconds, e.g. `500`

The defualt configuration can be found in the project's `Makefile`



## Example Usage

In this example, we set the grid to `4 x 8` and control robot `m_robot_1` movement. We also list all active robots.

```
Erlang/OTP 24 [erts-12.3.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V12.3.1  (abort with ^G)
(m_robot@127.0.0.1)1>
(m_robot@127.0.0.1)1> m_robot_api:grid(4,8).
ok
(m_robot@127.0.0.1)2> m_robot_api:move( m_robot_1, 2, 3, "E", "LFRFF" ).
{4,4,"E",'NOT_LOST'}
(m_robot@127.0.0.1)3> m_robot_api:move( m_robot_1, 0, 2, "E", "FFLFRFF" ).
{4,3,"E",'NOT_LOST'}
(m_robot@127.0.0.1)4> m_robot_api:list().
[{"m_robot_10",<0.287.0>,worker,[m_robot_statem]},
 {"m_robot_9",<0.286.0>,worker,[m_robot_statem]},
 {"m_robot_8",<0.285.0>,worker,[m_robot_statem]},
 {"m_robot_7",<0.284.0>,worker,[m_robot_statem]},
 {"m_robot_6",<0.283.0>,worker,[m_robot_statem]},
 {"m_robot_5",<0.282.0>,worker,[m_robot_statem]},
 {"m_robot_4",<0.281.0>,worker,[m_robot_statem]},
 {"m_robot_3",<0.280.0>,worker,[m_robot_statem]},
 {"m_robot_2",<0.279.0>,worker,[m_robot_statem]},
 {"m_robot_1",<0.278.0>,worker,[m_robot_statem]}]
 (m_robot@127.0.0.1)4>
 (m_robot@127.0.0.1)4>

```


## Next steps


- Implement a CLI to handle Erlang native commands
- Implement a UI to animate the robots movements
- Improve the speed metric of the robot
- Improve listing of all available robots
- Evaluate the movement algorithm and explore any efficiency improvements
- Include automated test suite and CI pipeline (container based)



