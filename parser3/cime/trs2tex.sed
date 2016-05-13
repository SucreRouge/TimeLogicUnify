s/;//g
s/[a]/p/g
s/[b]/q/g
s/[c]/r/g
s/\([pqr]\)/ \1/g
s/->/\\rewrite{}{}/g
s/[|]/\\vee /g
s/[&]/\\wedge /g
s/[=]/ \&, \& /g
s/[-~]/\\neg /g
s/[N]/X/g
s/\([XFGAEU]\)/\\\1/g
s/[>]/\\rightarrow /g
s/[0]/\\bot /g
s/[1]/\\top /g

