create robot-inp s" input.txt" slurp-file 2,
: robot-find-split
  robot-inp 2@ s\" \n\n" search 2drop
  robot-inp 2@ drop
  -
;
create robot-inp-split robot-find-split ,

0 field: robot-vec-x field: robot-vec-y constant robot-vec
create robot-vec-west robot-vec allot -1 0 swap robot-vec-west 2!
create robot-vec-east robot-vec allot 1 0 swap robot-vec-east 2!
create robot-vec-north robot-vec allot 0 -1 swap robot-vec-north 2!
create robot-vec-south robot-vec allot 0 1 swap robot-vec-south 2!
create robot-pos robot-vec allot
create robot-pos-scratch robot-vec allot \ pending "scratch" position that we might move to
create robot-pos-boulder robot-vec allot \ position of boulder being investigated
: robot-pos>scratch robot-pos 2@ robot-pos-scratch 2! ;
: robot-scratch>pos robot-pos-scratch 2@ robot-pos 2! ;
: robot-scratch>boulder robot-pos-scratch 2@ robot-pos-boulder 2! ;
: robot-. s>d swap over dabs <<# #s rot sign #> type #>> ;
: robot-vec-. cr s\" <" type dup robot-vec-x @ robot-. s\" , " type robot-vec-y @ robot-. s\" >" type ;
: robot-vec-+ { base off -- ret } base 2@ off 2@ rot + -rot + swap base 2! ;

create robot-map robot-inp @ cells allot \ let's make sure we have enough space for the map
0 constant ROBOT-MAP-FLOOR
1 constant ROBOT-MAP-WALL
2 constant ROBOT-MAP-BOULDER
create robot-map-width 0 ,
create robot-map-height 0 ,
: robot-pos-from-idx
  { idx } 
  idx robot-map-width @ mod robot-pos robot-vec-x !
  idx robot-map-width @ / robot-pos robot-vec-y !
;
: robot-idx-from-xy robot-map-width @ * + ;
: robot-idx-from-vec
  dup robot-vec-x @
  swap robot-vec-y @
  robot-idx-from-xy
;
: robot-parse-map ( -- n )
  \ n is number of cells written in robot-map
  0 \ index in robot-map
  robot-find-split 0 u+do
    robot-inp 2@ drop i chars + c@
    case
      '.' of ROBOT-MAP-FLOOR true endof
      '#' of ROBOT-MAP-WALL true endof
      'O' of ROBOT-MAP-BOULDER true endof
      '@' of dup robot-pos-from-idx ROBOT-MAP-FLOOR true endof
      #lf of robot-map-width @ 0= if i robot-map-width ! then false endof
      false swap
    endcase
    if
      over cells robot-map + ! \ write out to robot-map
      1 + \ increment index
    then
  loop
;
robot-parse-map robot-map-width @ / robot-map-height !

create robot-ins robot-inp @ cells allot \ and also for the instructions
create robot-ins-counter 0 ,
: robot-parse-ins
  0 \ index in robot-ins
  robot-inp 2@ nip
  robot-inp-split @ 2 +
  u+do
    robot-inp 2@ drop i chars + c@
    case
      '<' of robot-vec-west true endof
      '>' of robot-vec-east true endof
      '^' of robot-vec-north true endof
      'v' of robot-vec-south true endof
      false swap
    endcase
    if
      over cells robot-ins + !
      1 +
    then
  loop
;
robot-parse-ins drop

: robot-map-@ ( vec -- v )
  robot-idx-from-vec cells robot-map + @
;
: robot-map-! ( x vec -- )
  robot-idx-from-vec cells robot-map + !
;
: robot-map-is-player ( x y -- b )
  robot-pos robot-vec-y @ = swap
  robot-pos robot-vec-x @ =
  and
;
: robot-map-.-x
  { y -- }
  robot-map-width @ 0 u+do
    robot-map i y robot-idx-from-xy cells + @
    case
      ROBOT-MAP-FLOOR of
        i y robot-map-is-player
        if '@' else '.' then xemit
      endof
      ROBOT-MAP-WALL of '#' xemit endof
      ROBOT-MAP-BOULDER of 'O' xemit endof
    endcase
  loop
;
: robot-map-.
  robot-map-height @ 0 u+do
    cr i robot-map-.-x
  loop
;
: robot-score-x
  { y -- s }
  0
  robot-map-width @ 0 u+do
    robot-map i y robot-idx-from-xy cells + @
    case
      ROBOT-MAP-BOULDER of y 100 * i + + endof
    endcase
  loop
;
: robot-score
  0
  robot-map-height @ 0 u+do
    i robot-score-x +
  loop
;
: robot-walk-boulders
  { dir -- } \ repeatedly move robot-pos-boulder in dir. returns true if we find a floor tile
  begin
    robot-pos-boulder robot-map-@
    ROBOT-MAP-BOULDER =
  while
    robot-pos-boulder dir robot-vec-+
  repeat
  robot-pos-boulder robot-map-@ ROBOT-MAP-FLOOR =
;
: robot-move-by
  { dir -- }
  robot-pos>scratch
  robot-pos-scratch dir robot-vec-+ \ scratch now contains new candidate
  robot-pos-scratch robot-map-@
  case
    ROBOT-MAP-FLOOR of robot-scratch>pos endof \ if the cell is empty, just move there
    ROBOT-MAP-BOULDER of \ if the cell is a boulder, find the first floor in that direction
      robot-scratch>boulder
      dir robot-walk-boulders if
        robot-scratch>pos
        ROBOT-MAP-FLOOR robot-pos-scratch robot-map-!
        ROBOT-MAP-BOULDER robot-pos-boulder robot-map-!
      then
    endof 
  endcase
;
: robot-move
  robot-ins robot-ins-counter @ cells + @
  robot-move-by
  robot-ins-counter @ 1 + robot-ins-counter !
;
: robot-move-all
  begin
    robot-ins robot-ins-counter @ cells + @ 0<>
  while
    robot-move
  repeat
;

\ : robot-parse robot-parse-map ;
