# Experimental!

Inp       ← &fras "input.txt"
Reports   ← ⍚(⊜⋕⊸≠ @ ) ⊜□⊸≠@\nInp
Diff      ← ◌:□-↙-1⧻.,↘1.°□
All       ← =⧻⟜/+
Any       ← >0/+
CheckDiff ← (
  ⟜(All(×⊃(≤¯ 1)(≥¯3)))
  ⟜(All(×⊃(≥1)(≤3)))
  +∩(/×°□)◌
)

Diffs   ← ≡(Diff)Reports
PartOne ← /+ ≡CheckDiff Diffs
PartOne

RangeMask  ← =⊙⇡
RangeMasks ← ∵(RangeMask)⇡.⧻°□
MissingOne ← ◌:≡(□▽:°□,)⊙□¬RangeMasks.

RMO      ← ⍚(⊂□⟜MissingOne) Reports
RMODiffs ← ⍚(⍚(CheckDiff Diff))°□RMO
PartTwo  ← °□ /+ ≡(Any°□) RMODiffs
PartTwo
