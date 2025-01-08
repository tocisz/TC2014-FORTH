cold
variable rows
variable cols

variable x
variable y

35 constant '#'
94 constant '^'
46 constant '.'

variable pos
: readData ( - )
	variable [ cell negate ] literal allot
	0 rows !
	0 cols !
	0 pos !
	begin
		key ( key )
		dup 13 - if
			dup '#' = if
				drop
				1 c,
			else
				'^' = if
					pos @ x !
					rows @ y !
				then
				0 c,
			then
			1 pos +!
		else
			drop
			cols @ 0= if
				\ now we know the width
				pos @ cols !
			else
				\ check width
				pos @ cols @ - if
					exit
				then
			then
			1 rows +!
			0 pos !
		then
	again
;

readData tab
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
