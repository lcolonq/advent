#! /usr/bin/env bqn
inp ← •FChars "input.txt"
spl←>((⊢-˜(¬×+`))'
'=inp)⊔inp⋄m←×´≢spl
xs←'X'=spl⋄ms←'M'=spl⋄as←'A'=spl⋄ss←'S'=spl
U←m⊸|-⟜1⌾(0⊸⊑)⋄D←m⊸|1⊸+⌾(0⊸⊑)
L←m⊸|-⟜1⌾(1⊸⊑)⋄R←m⊸|1⊸+⌾(1⊸⊑)
_w←{((𝔽𝕩)⊑⎊0 ms)∧((𝔽𝔽𝕩)⊑⎊0 as)∧((𝔽𝔽𝔽𝕩)⊑⎊0 ss)}
G1←{a←𝕩⋄+´{f←𝕩⊑⟨U,D,R,L,U∘R,U∘L,D∘R,D∘L⟩⋄F _w a}¨↕8}
_x←{(𝕩⊑⎊0 ms)∧((𝔽𝕩)⊑⎊0 as)∧((𝔽𝔽𝕩)⊑⎊0 ss)}
GU2←(D∘L _x U∘R)∧(D∘R _x U∘L)
GD2←(U∘L _x D∘R)∧(U∘R _x D∘L)
GR2←(D∘L _x U∘R)∧(U∘L _x D∘R)
GL2←(D∘R _x U∘L)∧(U∘R _x D∘L)
G2←GU2∨GD2∨GR2∨GL2
•Show⟨+´G1¨/○⥊⟜(↕≢)xs,+´G2¨/○⥊⟜(↕≢)as⟩
