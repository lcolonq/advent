USING: assocs io.encodings.utf8 io.files kernel math namespaces
prettyprint ranges sequences sequences.extras splitting vectors ;
IN: main

SYMBOL: coords
H{ } clone coords set-global

SYMBOL: width
SYMBOL: height

: dump-pair ( value key assoc -- )
    drop [ "Set: " swap unparse append ] dip " = " swap unparse append append .
    ;

:: add-coord ( coord v -- )
    coord coords get-global v of ?push
    v coords get-global set-at
    ;

:: parse-line ( line y -- )
    line length width set-global
    line [| v x | v CHAR: . = [ { x y } v add-coord ] unless ] each-index
    ;

: parse-input ( -- )
    H{ } clone coords set-global
    "input.txt" utf8 file-contents
    "\n" split
    [ "\n" swap remove ] map
    [ empty? not ] filter
    dup length height set-global
    [ parse-line ] each-index ;

: inbounds ( c -- c )
    [ first [ width get-global < ] [ 0 >= ] bi and ]
    [ second [ height get-global < ] [ 0 >= ] bi and ]
    bi and ;

: cdiff ( x y -- z )
    over
    zip [ [ first ] [ second ] bi - ] map
    [| base diff |
     0 width get-global 2 * [a..b]
     [| scale |
      base
      diff 
      zip [ [ first ] [ second scale * ] bi - ] map
     ]
     map
    ] call
    [ inbounds ] filter
    ;

:: ctable ( nodes -- table )
    nodes [ dup f ?push swap nodes remove swap ?push ] map
    ;

: possible-antinodes ( nodes -- antinodes )
    ctable [| p | p second [ p first cdiff ] map concat ] map
    ;

: antinodes ( nodes -- antinodes )
    possible-antinodes concat [ inbounds ] filter
    ;

: all-antinodes ( -- antinodes )
    coords get-global [ swap drop antinodes ] { } assoc>map concat
    deduplicate
    ;
