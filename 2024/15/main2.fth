create robot-inp s" input.txt" slurp-file 2,
: robot-find-split
  robot-inp 2@ s\" \n\n" search 2drop
  robot-inp 2@ drop
  -
;
create robot-inp-split robot-find-split ,

0 field: robot-vec-x field: robot-vec-y field: robot-vec-vert constant robot-vec
create robot-vec-west robot-vec allot -1 0 swap robot-vec-west 2!
0 robot-vec-west robot-vec-vert !
create robot-vec-east robot-vec allot 1 0 swap robot-vec-east 2!
0 robot-vec-east robot-vec-vert !
create robot-vec-north robot-vec allot 0 -1 swap robot-vec-north 2!
1 robot-vec-north robot-vec-vert !
create robot-vec-south robot-vec allot 0 1 swap robot-vec-south 2!
1 robot-vec-south robot-vec-vert !
create robot-pos robot-vec allot
create robot-pos-scratch robot-vec allot \ pending "scratch" position that we might move to
create robot-pos-backup robot-vec allot
: robot-pos>scratch robot-pos 2@ robot-pos-scratch 2! ;
: robot-scratch>pos robot-pos-scratch 2@ robot-pos 2! ;
: robot-backup>scratch robot-pos-backup 2@ robot-pos-scratch 2! ;
: robot-scratch>backup robot-pos-scratch 2@ robot-pos-backup 2! ;
: robot-. s>d swap over dabs <<# #s rot sign #> type #>> ;
: robot-vec-. cr s\" <" type dup robot-vec-x @ robot-. s\" , " type robot-vec-y @ robot-. s\" >" type ;
: robot-vec-+ { base off -- ret } base 2@ off 2@ rot + -rot + swap base 2! ;
: robot-vec-inverse
  case
    robot-vec-west of robot-vec-east endof
    robot-vec-east of robot-vec-west endof
    robot-vec-north of robot-vec-south endof
    robot-vec-south of robot-vec-north endof
  endcase
;

create robot-map robot-inp @ 2 * cells allot \ let's make sure we have enough space for the map
0 constant ROBOT-MAP-FLOOR
1 constant ROBOT-MAP-WALL
2 constant ROBOT-MAP-BOULDER-LEFT
3 constant ROBOT-MAP-BOULDER-RIGHT
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
      '.' of
        ROBOT-MAP-FLOOR over cells robot-map + ! 1 +
        ROBOT-MAP-FLOOR over cells robot-map + ! 1 +
      endof
      '#' of
        ROBOT-MAP-WALL over cells robot-map + ! 1 +
        ROBOT-MAP-WALL over cells robot-map + ! 1 +
      endof
      'O' of
        ROBOT-MAP-BOULDER-LEFT over cells robot-map + ! 1 +
        ROBOT-MAP-BOULDER-RIGHT over cells robot-map + ! 1 +
      endof
      '@' of
        dup robot-pos-from-idx
        ROBOT-MAP-FLOOR over cells robot-map + ! 1 +
        ROBOT-MAP-FLOOR over cells robot-map + ! 1 +
      endof
      #lf of robot-map-width @ 0= if dup robot-map-width ! then endof
    endcase
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

: robot-map-@ ( vec -- v ) robot-idx-from-vec cells robot-map + @ ;
: robot-map-! ( x vec -- ) robot-idx-from-vec cells robot-map + ! ;
: robot-map-is-player ( x y -- b ) robot-pos robot-vec-y @ = swap robot-pos robot-vec-x @ = and ;
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
      ROBOT-MAP-BOULDER-LEFT of '[' xemit endof
      ROBOT-MAP-BOULDER-RIGHT of ']' xemit endof
    endcase
  loop
;
: robot-map-. robot-map-height @ 0 u+do cr i robot-map-.-x loop ;
: robot-score-x
  { y -- s }
  0
  robot-map-width @ 0 u+do
    robot-map i y robot-idx-from-xy cells + @
    case
      ROBOT-MAP-BOULDER-LEFT of y 100 * i + + endof
    endcase
  loop
;
: robot-score
  0
  robot-map-height @ 0 u+do
    i robot-score-x +
  loop
;

: robot-scratchxy robot-pos-scratch robot-vec-x @ robot-pos-scratch robot-vec-y @ ;

: robot-check-move-horiz
  { dir x y }
  robot-map x y robot-idx-from-xy cells + @
  case
    ROBOT-MAP-FLOOR of true endof
    ROBOT-MAP-WALL of false endof
    ROBOT-MAP-BOULDER-LEFT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
    endof
    ROBOT-MAP-BOULDER-RIGHT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
    endof
  endcase
;

: robot-do-move-horiz
  { dir x y }
  robot-map x y robot-idx-from-xy cells + @
  case
    ROBOT-MAP-BOULDER-LEFT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ robot-scratchxy { newx newy }
      dir newx newy recurse
      ROBOT-MAP-BOULDER-LEFT newx newy robot-idx-from-xy cells robot-map + !
    endof
    ROBOT-MAP-BOULDER-RIGHT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ robot-scratchxy { newx newy }
      dir newx newy recurse
      ROBOT-MAP-BOULDER-RIGHT newx newy robot-idx-from-xy cells robot-map + !
    endof
  endcase
  ROBOT-MAP-FLOOR x y robot-idx-from-xy cells robot-map + !
;

: robot-check-move-vert
  { dir x y }
  robot-map x y robot-idx-from-xy cells + @
  case
    ROBOT-MAP-FLOOR of true endof
    ROBOT-MAP-WALL of false endof
    ROBOT-MAP-BOULDER-LEFT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
      x 1 + y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
      and
    endof
    ROBOT-MAP-BOULDER-RIGHT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
      x 1 - y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+
      dir robot-scratchxy recurse
      and
    endof
  endcase
;

: robot-do-move-vert
  { dir x y }
  x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ robot-pos-scratch robot-map-@ { next }
  robot-map x y robot-idx-from-xy cells + @
  case
    ROBOT-MAP-BOULDER-LEFT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ robot-scratchxy { newx newy }
      dir newx newy recurse
      next ROBOT-MAP-BOULDER-LEFT <> if 
         x 1 + y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ dir robot-scratchxy recurse
      then
      ROBOT-MAP-BOULDER-LEFT newx newy robot-idx-from-xy cells robot-map + !
      ROBOT-MAP-BOULDER-RIGHT newx 1 + newy robot-idx-from-xy cells robot-map + !
      ROBOT-MAP-FLOOR x 1 + y robot-idx-from-xy cells robot-map + !
    endof
    ROBOT-MAP-BOULDER-RIGHT of
      x y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ robot-scratchxy { newx newy }
      dir newx newy recurse
      next ROBOT-MAP-BOULDER-RIGHT <> if
         x 1 - y swap robot-pos-scratch 2! robot-pos-scratch dir robot-vec-+ dir robot-scratchxy recurse
      then

      ROBOT-MAP-BOULDER-LEFT newx 1 - newy robot-idx-from-xy cells robot-map + !
      ROBOT-MAP-BOULDER-RIGHT newx newy robot-idx-from-xy cells robot-map + !
      ROBOT-MAP-FLOOR x 1 - y robot-idx-from-xy cells robot-map + !
    endof
  endcase
  ROBOT-MAP-FLOOR x y robot-idx-from-xy cells robot-map + !
;

: robot-move-boulder
  { dir -- }
  robot-scratch>backup
  dir robot-scratchxy
  robot-pos-scratch robot-map-@ { old }
  dir robot-vec-vert @ if robot-check-move-vert else robot-check-move-horiz then
  if
    robot-backup>scratch
    dir robot-scratchxy
    dir robot-vec-vert @ if
      robot-do-move-vert

      robot-backup>scratch robot-scratchxy { x y }
      old case
        ROBOT-MAP-BOULDER-LEFT of ROBOT-MAP-FLOOR x 1 + y robot-idx-from-xy cells robot-map + ! endof
        ROBOT-MAP-BOULDER-RIGHT of ROBOT-MAP-FLOOR x 1 - y robot-idx-from-xy cells robot-map + ! endof
      endcase
    else
      robot-do-move-horiz
    then
    robot-backup>scratch robot-scratch>pos
  then
;
: robot-move-by
  { dir -- }
  robot-pos>scratch
  robot-pos-scratch dir robot-vec-+ \ scratch now contains new candidate
  robot-pos-scratch robot-map-@
  case
    ROBOT-MAP-FLOOR of robot-scratch>pos endof \ if the cell is empty, just move there
    ROBOT-MAP-BOULDER-LEFT of dir robot-move-boulder endof 
    ROBOT-MAP-BOULDER-RIGHT of dir robot-move-boulder endof 
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
