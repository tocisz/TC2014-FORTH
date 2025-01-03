cold
variable cols
variable rows
variable tab

variable pos
: readData ( - )
	0 rows !
	0 cols !
	0 pos !
	here tab !
	begin
		key ( key )
		dup 13 - if
			c,
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

readData
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX

