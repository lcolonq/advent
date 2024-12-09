let
  lib = (import <nixpkgs> {}).lib;
  inp = lib.strings.removeSuffix "\n" (builtins.readFile "/home/llll/src/advent/2024/9/input.txt");
  nums = map (c: lib.strings.charToInt c - lib.strings.charToInt "0") (lib.strings.stringToCharacters inp);
  expanded = 
    lib.lists.imap0
      (idx2: cnt:
        { id =
            if builtins.bitAnd idx2 1 == 0
            then (idx2 / 2)
            else null;
          length = cnt;
        }
      )
      nums;
  narrow =
    builtins.foldl'
      (acc: x:
        let
          state = acc.state;
          e = builtins.elemAt state acc.end;
          len = builtins.length state;
        in
          if builtins.deepSeq [acc (builtins.trace acc.end null)] (e.id == null)
          then { state = state; end = acc.end - 1; }
          else
            let
              idx = lib.lists.findFirstIndex
                (c: c.id == null && c.length >= e.length)
                null
                state;
            in
              if idx == null || idx >= acc.end
              then { state = state; end = acc.end - 1; }
              else {
                state = lib.lists.flatten
                  (lib.lists.imap0
                    ( i: v:
                      if i == idx
                      then
                        let
                          gaplen = v.length - e.length;
                        in
                          if gaplen == 0
                          then
                            [ e ]
                          else
                            [ e { id = null; length = gaplen; } ]
                      else
                        if i == acc.end
                        then
                          if i == len - 1
                          then []
                          else [ { id = null; length = v.length; } ]
                        else v
                    )
                    state);
                end = acc.end - 1;
              }
      )
      { state = expanded; end = builtins.length expanded - 1; }
      (lib.lists.range 0 (builtins.length expanded - 1));
  checksum =
    (builtins.foldl'
      (acc: x: {
        idx = acc.idx + x.length;
        sum =
          acc.sum +
          (if x.id == null
           then 0
           else x.id * builtins.foldl' (a: b: a + b) 0 (lib.lists.range acc.idx (acc.idx + x.length - 1)));
      })
      { sum = 0; idx = 0; }
      narrow.state).sum;
in
{ res = checksum;
}
