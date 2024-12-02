# Experimental!

Inp     ← &fras "input.txt"
Rows    ← ⊜(♭[⊜(⋕)⊸≠@ ]) ⊸≠@\nInp
Left    ← ≡(⊡0)Rows
Right   ← ≡(⊡1)Rows
PartOne ← /+≡(/(⌵-))⍉⊟ ⍆ Left ⍆ Right
PartOne

PartTwo ← /+∵(×/+⌕:Right .) Left
PartTwo
