let
  lib = (import <nixpkgs> {}).lib;
  inp = lib.strings.removeSuffix "\n" (builtins.readFile "/home/llll/src/advent/2024/9/input.txt");
  nums = map (c: lib.strings.charToInt c - lib.strings.charToInt "0") (lib.strings.stringToCharacters inp);
  expanded = lib.lists.flatten
    (lib.lists.imap0
      (idx2: cnt:
        lib.lists.replicate cnt
          (if builtins.bitAnd idx2 1 == 0
           then (idx2 / 2)
           else null
          )
      )
      nums
    );
  attrset = builtins.zipAttrsWith
    (idx: id: builtins.elemAt id 0)
    (lib.lists.imap0 (idx: val: { "${toString idx}" = val; }) expanded);
  indices = lib.lists.range 0 (builtins.length expanded - 1);
  revindices = lib.lists.reverseList indices;
  narrow =
    builtins.deepSeq
      attrset
      (builtins.foldl'
        (acc: mcursor:
          let
            end = acc.end;
            state = acc.state;
            cursor = mcursor + acc.coffset;
          in
            if builtins.deepSeq [acc cursor (builtins.trace cursor null)] (cursor >= end)
            then acc
            else
              let
                cval = state."${toString cursor}";
              in
                if cval == null
                then
                  if state."${toString end}" == null
                  then {
                    state = state;
                    end = end - 1;
                    coffset = acc.coffset - 1;
                  }
                  else {
                    state =
                      builtins.removeAttrs
                        (state // { "${toString cursor}" = state."${toString end}"; })
                        [ "${toString end}" ];
                    end = end - 1;
                    coffset = acc.coffset;
                  }
                else acc
        )
        { state = attrset; end = builtins.length indices - 1; coffset = 0; }
        indices);
  filtered = lib.attrsets.filterAttrs (k: v: v != null) narrow.state;
  filteredlen = builtins.length (lib.attrsets.attrValues filtered);
  relist =
    builtins.deepSeq
      filtered
      (builtins.foldl' (acc: x: acc ++ [filtered."${toString x}"]) [] (lib.lists.range 0 (filteredlen - 1)));
  checksum =
    builtins.deepSeq
      relist
      (builtins.foldl' (x: y: x + y) 0 (lib.lists.imap0 (idx: v: idx * v) relist));
in
{ res = checksum;
  inherit expanded attrset narrow;
}
