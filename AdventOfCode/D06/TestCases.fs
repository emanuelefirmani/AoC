﻿module AdventOfCode.D06.TestCases

let testShort = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

let testLong = "..#.........#...#......#...........#.#...#............#...#.........#....#......................#......#..........................
...........#....#.............#.....................................#............................................#.....#..........
.................#.........#.......#.......#..#............#.........#.........#............................................#.....
......#................................#.........................................#................................................
#...............................##......#.#...........................#....................#...#...............#.................#
.............#......#...........................................#..............#..........#..............#........................
...#...................#..............#........#.......#..........#.........................#.#.....#.....#.......................
#.#...#.............................#.................................................#..............#............#...............
.............................#.......#.............#......#...............#.....#.................................................
....................................#.....................................#.................#........#..........#.#..#...........#
......................#......#...............................#.................#.......#......................#.......#...........
...#..............#....................#............#...........##..#.....................................................#.......
....#.........#...................................#................................................#..........................#...
..............#.....#.................................................................#..#.....##.......#.........................
.........#....#...........##..........................................#......#.....................#..........#.......#...........
..#....#..........#.................................................................#....#............#............#........#.....
............#.#....#...........#.............#...........#..................#.....#.................................#.......#.....
..........#.......#............................#........#...........#.......#...#....#........#.............##.#............#.....
...................#.........#....#.....................................................................#.#..........#............
.#...#.#..................#...............................................#...........................#......................#....
.........#..............#.........#.......#.#......##..................#......#...#..#...........#.....................#..........
...............................#.............#..#......#............#..........#......................#..........#.............#..
..........#.....#..........................#..................................................#.#..#.....#........................
............#..##.................................#....................#.....#.......................................#............
......................#.#...................#......#..............................................................#...............
.......#.............................................................................#............................................
........#.................#....................................................#.......................................#....##.#..
..................#........................#......#..##....#.........#...##..#....................................#...............
.............#..............................................................................##...#......#.................#.......
..................................#.#.............................................................................#...............
...........#.................................#.......................................................................#......#.....
.....#............................................................#................................................#..............
.....#...........#.#..........#..............#..........................#.................................................#.......
....#........#...#..........#..#..........................................................................#.......................
.........................................................................#.....#.......................................#....#.....
...............#...#..#.....#.......................#.................#........#..........##....................................#.
.......................#....#...#..................#..................#.....................................#.............#.......
...........#..#...#..............................................^........#........................#......................#.......
.............................................#.................................................................................#..
......#...#.........#.....................................#...............................................#..#....#.......#.......
...#......................................................#.................#..#.............................................#....
.....#......................#.......................................#.............................................#....#..........
...#.....................................................#......#...#.......#....#..............................#....#............
......#.....#.....#...........#...#................#...........................................#..................................
...................................#...............#......................................................#.......................
..............................#...#........................................................#.#.............#......................
......#.......#............#..#......................#..#.....#..................#......#.........................................
#......#....#.......#....................#.....#.#............................................................................#...
.#....#...................................#.................#....................#........................................#.......
............................................#...................................#.......#..............#.........................#
............##.........#..............................................................................#.........#.................
##.................................................#.......................#.#.....#..............................................
...........................#..........#...#.#.........................#.................................##.....#..................
.............#..........#........................................#............#............#.....................................#
.....................................................#.................................................#..........................
.....#........................................#.............................#...#.....................#...........................
.......#..................................#...........#.......#.....#........................................#.......#............
.............#.......................#......................#.....#.......#..................................#..#........#........
.......................................................#......#...............#.........#.............#............#..............
....#...............................#.#.....................#.................................................................#..#
...........................#........#..........................................#........................................#.........
....#.............................................................................................................................
.##........#.......#..................#.#....#.............................................................#.##...................
.#..#.......................##........................................................#...............................#...........
.....#.#..............#...........................................#...............................................................
.....#.#............................#.............................................................................................
......................#............................#....#............................................................#............
.......#.......................................................................#..................#...##..........................
...............#...................................................#.....................................#...............#.#......
....#..........#.......#..#................................#......................................................................
#......##..................................................................#...#........................#.........................
..................................#........................................................................#...........#......#...
.#....................#....#..................................#..#..................#.#.......................#...................
......#..#................................................................................................##......................
.....##........#..#.......................#.........#............#....#...........................................................
...............#...........#.................#...........#.........#.......................#.........#........................#...
............................................................................................#..#..................................
..........................................#.............#..#...............#.............................#.#......#.............#.
......................#............#..........#............................................................#...#................#.
...........#...................#..................................................................................................
.......#........................#.................................................................#.................#.............
#...##................#......................................................................#................................#...
...#.....#.......................#...#..............#....................#.........#.........#....................................
....#.....................................#.............................................................................#.........
.........#..............................................................................#..............#..........................
...................................#.#...............#........................................................................##..
.........#............#..............#..........................................................................#...#.............
.....#.......................#..#....#.#....................#..........................................#..........................
.......................................#........#.........#.....................#.............#...........#...............#.......
................#.................................................................................................................
.......#....................................#..................#...........#.....#.........................#......................
...................#................................................................#...............#................#............
..........#.......................#......................#...........#..#........#....#.........................#.................
...................#........................#.......#................................................#............................
.......................#......................................................#...........#.......#...................#...........
........#..........#........#......#......................#......................................#...............#................
...........................................................#...#..................................................................
.......................#......................#............#..#....#...#............................#.............................
.#.#...............#..........#..........##........#..............................................................................
##.........#....................#...................#.........#......................##..................#...............#........
.#......................#...#...#......................#............#......................#.................#..................#.
#..............................#......................................#.........#.#...............................................
...........#....#......#.......#..........................#..........................................#......#............#..#.....
................................................................................................#.......#.........................
....#..................................#..........................#.................................#...#...#.....................
.......................#.#..#.............................#.#...........................#......#..........#.......................
...##.......................................#..................#....................#.............................................
...#.......#........................#..........##.....#............#......................#.....................................#.
......#..............#.............................#.............................................#.............................#..
#...........................................#.......#....#.#..........#........#............#.......................#.......#.....
..................#.......#............#.....#.............................................................#......................
.#.....................................................................................#...................................#......
...........#.........#.#.................#..........................#............................................#.......##.......
.......................#......#.................................................#........................#.................#......
..................#........................................................#................................................#....#
...............................#.......#....#.........#..........#.....#.......................................#..#...............
......##................##...#....#...#...#............................................................................#..........
.......................#............#...................................#....#.......#.............................#..............
..............................#.........................................................#........#.............#..................
........................#............................................#..................#.....#...................#...............
............#......#...............................#.............................................................................#
.#.........#.........#.........#.......#.#..#...#...............................#.......#.........#..........................#....
........................#.................................................#....................................##......#..........
....#.......#.....................##...#..........................................................................................
.................................#.......................................#.......................#.#......#.......................
.#.............#..................#...........................#.................#...........................................#.....
.....#.........#................#..........#..#..........#......................#...#......................#....#.................
.................#......#.....................................................................#...................................
......................#.......................................................#..#......#..............#..#..#..........#.........
...............................#.........#........#..................#.#.........#.#.#........#..#.........#....##...#............"