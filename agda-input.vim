if exists("*which_key#register")
  let l:cornelis_agda_prefix = get(g:, "cornelis_agda_prefix", "<localleader>")
  exec "inoremap <buffer> " . l:cornelis_agda_prefix . " <C-O>:WhichKey \"agda\"<CR>"
  call which_key#register('agda', "g:agda_input")
endif

if !exists("g:agda_input")
  let g:agda_input = {}
endif


if !exists("g:cornelis_no_agda_input")

  " Mapping unicode symbols.
  call cornelis#bind_input("to", "→")
  call cornelis#bind_input("to", "→")
  call cornelis#bind_input("->", "→")
  call cornelis#bind_input("<-", "←")
  call cornelis#bind_input("and", "∧")
  call cornelis#bind_input("or", "∨")
  call cornelis#bind_input("neg", "¬")
  call cornelis#bind_input("qed", "∎")
  call cornelis#bind_input("o", "∘")
  call cornelis#bind_input("comp", "∘")
  call cornelis#bind_input("<", "⟨")
  call cornelis#bind_input(">", "⟩")
  call cornelis#bind_input("==", "≡")
  call cornelis#bind_input("==n", "≢")
  call cornelis#bind_input("::", "∷")
  call cornelis#bind_input("all", "∀")
  call cornelis#bind_input("ex", "∃")
  call cornelis#bind_input("le", "≤")
  call cornelis#bind_input("ge", "≥")
  call cornelis#bind_input("'", "′")
  call cornelis#bind_input("top", "⊤")
  call cornelis#bind_input("bot", "⊥")
  call cornelis#bind_input("?=", "≟")
  call cornelis#bind_input("o*", "⊛")
  call cornelis#bind_input("x", "×")
  call cornelis#bind_input("o+", "⊕")
  call cornelis#bind_input("t>", "▹")
  call cornelis#bind_input(".-", "∸")
  call cornelis#bind_input("{{", "⦃")
  call cornelis#bind_input("}}", "⦄")
  call cornelis#bind_input("nat", "ℕ")
  call cornelis#bind_input("=>", "⇒")
  call cornelis#bind_input("in", "∈")
  call cornelis#bind_input("ni", "∋")

  call cornelis#bind_input("_0", "₀")
  call cornelis#bind_input("_1", "₁")
  call cornelis#bind_input("_2", "₂")
  call cornelis#bind_input("_3", "₃")
  call cornelis#bind_input("_4", "₄")
  call cornelis#bind_input("_5", "₅")
  call cornelis#bind_input("_6", "₆")
  call cornelis#bind_input("_7", "₇")
  call cornelis#bind_input("_8", "₈")
  call cornelis#bind_input("_9", "₉")

  call cornelis#bind_input("^0", "⁰")
  call cornelis#bind_input("^1", "¹")
  call cornelis#bind_input("^2", "²")
  call cornelis#bind_input("^3", "³")
  call cornelis#bind_input("^4", "⁴")
  call cornelis#bind_input("^5", "⁵")
  call cornelis#bind_input("^6", "⁶")
  call cornelis#bind_input("^7", "⁷")
  call cornelis#bind_input("^8", "⁸")
  call cornelis#bind_input("^9", "⁹")


  " Agda-input proper
  call cornelis#bind_input("=n", "≠")
  call cornelis#bind_input("~", "∼")
  call cornelis#bind_input("~n", "≁")
  call cornelis#bind_input("~~", "≈")
  call cornelis#bind_input("~~n", "≉")
  call cornelis#bind_input("~~~", "≋")
  call cornelis#bind_input(":~", "∻")
  call cornelis#bind_input("~-", "≃")
  call cornelis#bind_input("~-n", "≄")
  call cornelis#bind_input("-~", "≂")
  call cornelis#bind_input("~=", "≅")
  call cornelis#bind_input("~=n", "≇")
  call cornelis#bind_input("~~-", "≊")
  call cornelis#bind_input("==", "≡")
  call cornelis#bind_input("==n", "≢")
  call cornelis#bind_input("===", "≣")
  call cornelis#bind_input(".=", "≐")
  call cornelis#bind_input(".=.", "≑")
  call cornelis#bind_input(":=", "≔")
  call cornelis#bind_input("=:", "≕")
  call cornelis#bind_input("=o", "≗")
  call cornelis#bind_input("(=", "≘")
  call cornelis#bind_input("and=", "≙")
  call cornelis#bind_input("or=", "≚")
  call cornelis#bind_input("*=", "≛")
  call cornelis#bind_input("t=", "≜")
  call cornelis#bind_input("def=", "≝")
  call cornelis#bind_input("m=", "≞")
  call cornelis#bind_input("?=", "≟")
  call cornelis#bind_input("<=", "≤")
  call cornelis#bind_input(">=", "≥")
  call cornelis#bind_input("<=n", "≰")
  call cornelis#bind_input(">=n", "≱")
  call cornelis#bind_input("len", "≰")
  call cornelis#bind_input("gen", "≱")
  call cornelis#bind_input("<n", "≮")
  call cornelis#bind_input(">n", "≯")
  call cornelis#bind_input("<~", "≲")
  call cornelis#bind_input(">~", "≳")
  call cornelis#bind_input("<~n", "⋦")
  call cornelis#bind_input(">~n", "⋧")
  call cornelis#bind_input("<~nn", "≴")
  call cornelis#bind_input(">~nn", "≵")
  call cornelis#bind_input("sub", "⊂")
  call cornelis#bind_input("sup", "⊃")
  call cornelis#bind_input("subn", "⊄")
  call cornelis#bind_input("supn", "⊅")
  call cornelis#bind_input("sub=", "⊆")
  call cornelis#bind_input("sup=", "⊇")
  call cornelis#bind_input("sub=n", "⊈")
  call cornelis#bind_input("sup=n", "⊉")
  call cornelis#bind_input("squb", "⊏")
  call cornelis#bind_input("squp", "⊐")
  call cornelis#bind_input("squb=", "⊑")
  call cornelis#bind_input("squp=", "⊒")
  call cornelis#bind_input("squb=n", "⋢")
  call cornelis#bind_input("squp=n", "⋣")
  call cornelis#bind_input("inn", "∉")
  call cornelis#bind_input("nin", "∌")
  call cornelis#bind_input("and", "∧")
  call cornelis#bind_input("or", "∨")
  call cornelis#bind_input("And", "⋀")
  call cornelis#bind_input("Or", "⋁")
  call cornelis#bind_input("i", "∩")
  call cornelis#bind_input("un", "∪")
  call cornelis#bind_input("u+", "⊎")
  call cornelis#bind_input("u.", "⊍")
  call cornelis#bind_input("I", "⋂")
  call cornelis#bind_input("Un", "⋃")
  call cornelis#bind_input("U+", "⨄")
  call cornelis#bind_input("U.", "⨃")
  call cornelis#bind_input("glb", "⊓")
  call cornelis#bind_input("lub", "⊔")
  call cornelis#bind_input("Glb", "⨅")
  call cornelis#bind_input("Lub", "⨆")
  call cornelis#bind_input("|-", "⊢")
  call cornelis#bind_input("|-n", "⊬")
  call cornelis#bind_input("-|", "⊣")
  call cornelis#bind_input("|=", "⊨")
  call cornelis#bind_input("|=n", "⊭")
  call cornelis#bind_input("||-", "⊩")
  call cornelis#bind_input("||-n", "⊮")
  call cornelis#bind_input("||=", "⊫")
  call cornelis#bind_input("||=n", "⊯")
  call cornelis#bind_input("|||-", "⊪")
  call cornelis#bind_input("|", "∣")
  call cornelis#bind_input("|n", "∤")
  call cornelis#bind_input("||", "∥")
  call cornelis#bind_input("||n", "∦")
  call cornelis#bind_input("all", "∀")
  call cornelis#bind_input("ex", "∃")
  call cornelis#bind_input("exn", "∄")
  call cornelis#bind_input("0", "∅")
  call cornelis#bind_input("C", "∁")
  call cornelis#bind_input("cul", "⌜")
  call cornelis#bind_input("cuL", "⌈")
  call cornelis#bind_input("cur", "⌝")
  call cornelis#bind_input("cuR", "⌉")
  call cornelis#bind_input("cll", "⌞")
  call cornelis#bind_input("clL", "⌊")
  call cornelis#bind_input("clr", "⌟")
  call cornelis#bind_input("clR", "⌋")
  call cornelis#bind_input("qed", "∎")
  call cornelis#bind_input("x", "×")
  call cornelis#bind_input("o", "∘")
  call cornelis#bind_input("comp", "∘")
  call cornelis#bind_input(".", "∙")
  call cornelis#bind_input("*", "⋆")
  call cornelis#bind_input(".+", "∔")
  call cornelis#bind_input(".-", "∸")
  call cornelis#bind_input("::", "∷")
  call cornelis#bind_input("::-", "∺")
  call cornelis#bind_input("-:", "∹")
  call cornelis#bind_input("+ ", "⊹")
  call cornelis#bind_input("+-", "±")
  call cornelis#bind_input("-+", "∓")
  call cornelis#bind_input("surd3", "∛")
  call cornelis#bind_input("surd4", "∜")
  call cornelis#bind_input("increment", "∆")
  call cornelis#bind_input("inf", "∞")
  call cornelis#bind_input("&", "⅋")
  call cornelis#bind_input("z;", "⨟")
  call cornelis#bind_input("z:", "⦂")
  call cornelis#bind_input("o+", "⊕")
  call cornelis#bind_input("o--", "⊖")
  call cornelis#bind_input("ox", "⊗")
  call cornelis#bind_input("o/", "⊘")
  call cornelis#bind_input("o.", "⊙")
  call cornelis#bind_input("oo", "⊚")
  call cornelis#bind_input("o*", "⊛")
  call cornelis#bind_input("o=", "⊜")
  call cornelis#bind_input("o-", "⊝")
  call cornelis#bind_input("O+", "⨁")
  call cornelis#bind_input("Ox", "⨂")
  call cornelis#bind_input("O.", "⨀")
  call cornelis#bind_input("O*", "⍟")
  call cornelis#bind_input("b+", "⊞")
  call cornelis#bind_input("b-", "⊟")
  call cornelis#bind_input("bx", "⊠")
  call cornelis#bind_input("b.", "⊡")
  call cornelis#bind_input("l-", "←")
  call cornelis#bind_input("<-", "←")
  call cornelis#bind_input("l=", "⇐")
  call cornelis#bind_input("<=", "⇐")
  call cornelis#bind_input("r-", "→")
  call cornelis#bind_input("->", "→")
  call cornelis#bind_input("r=", "⇒")
  call cornelis#bind_input("=>", "⇒")
  call cornelis#bind_input("u-", "↑")
  call cornelis#bind_input("u=", "⇑")
  call cornelis#bind_input("d-", "↓")
  call cornelis#bind_input("d=", "⇓")
  call cornelis#bind_input("ud-", "↕")
  call cornelis#bind_input("ud=", "⇕")
  call cornelis#bind_input("lr-", "↔")
  call cornelis#bind_input("<->", "↔")
  call cornelis#bind_input("lr=", "⇔")
  call cornelis#bind_input("<=>", "⇔")
  call cornelis#bind_input("ul-", "↖")
  call cornelis#bind_input("ul=", "⇖")
  call cornelis#bind_input("ur-", "↗")
  call cornelis#bind_input("ur=", "⇗")
  call cornelis#bind_input("dr-", "↘")
  call cornelis#bind_input("dr=", "⇘")
  call cornelis#bind_input("dl-", "↙")
  call cornelis#bind_input("dl=", "⇙")
  call cornelis#bind_input("l==", "⇚")
  call cornelis#bind_input("l-2", "⇇")
  call cornelis#bind_input("l-r-", "⇆")
  call cornelis#bind_input("r==", "⇛")
  call cornelis#bind_input("r-2", "⇉")
  call cornelis#bind_input("r-3", "⇶")
  call cornelis#bind_input("r-l-", "⇄")
  call cornelis#bind_input("u==", "⟰")
  call cornelis#bind_input("u-2", "⇈")
  call cornelis#bind_input("u-d-", "⇅")
  call cornelis#bind_input("d==", "⟱")
  call cornelis#bind_input("d-2", "⇊")
  call cornelis#bind_input("d-u-", "⇵")
  call cornelis#bind_input("l--", "⟵")
  call cornelis#bind_input("<--", "⟵")
  call cornelis#bind_input("l~", "↜")
  call cornelis#bind_input("r--", "⟶")
  call cornelis#bind_input("-->", "⟶")
  call cornelis#bind_input("r~", "↝")
  call cornelis#bind_input("lr--", "⟷")
  call cornelis#bind_input("<-->", "⟷")
  call cornelis#bind_input("lr~", "↭")
  call cornelis#bind_input("l-n", "↚")
  call cornelis#bind_input("<-n", "↚")
  call cornelis#bind_input("l=n", "⇍")
  call cornelis#bind_input("r-n", "↛")
  call cornelis#bind_input("->n", "↛")
  call cornelis#bind_input("r=n", "⇏")
  call cornelis#bind_input("=>n", "⇏")
  call cornelis#bind_input("lr-n", "↮")
  call cornelis#bind_input("<->n", "↮")
  call cornelis#bind_input("lr=n", "⇎")
  call cornelis#bind_input("<=>n", "⇎")
  call cornelis#bind_input("l-|", "↤")
  call cornelis#bind_input("ll-", "↞")
  call cornelis#bind_input("r-|", "↦")
  call cornelis#bind_input("rr-", "↠")
  call cornelis#bind_input("u-|", "↥")
  call cornelis#bind_input("uu-", "↟")
  call cornelis#bind_input("d-|", "↧")
  call cornelis#bind_input("dd-", "↡")
  call cornelis#bind_input("ud-|", "↨")
  call cornelis#bind_input("l->", "↢")
  call cornelis#bind_input("r->", "↣")
  call cornelis#bind_input("r-o", "⊸")
  call cornelis#bind_input("-o", "⊸")
  call cornelis#bind_input("dz", "↯")
  call cornelis#bind_input("sq", "□")
  call cornelis#bind_input("sq.", "▣")
  call cornelis#bind_input("sqo", "▢")
  call cornelis#bind_input("pab", "▰")
  call cornelis#bind_input("paw", "▱")
  call cornelis#bind_input("dib", "◆")
  call cornelis#bind_input("diw", "◇")
  call cornelis#bind_input("di.", "◈")
  call cornelis#bind_input("lz", "◊")
  call cornelis#bind_input("cib", "●")
  call cornelis#bind_input("ciw", "○")
  call cornelis#bind_input("ci.", "◎")
  call cornelis#bind_input("ci..", "◌")
  call cornelis#bind_input("ciO", "◯")
  call cornelis#bind_input("st6", "✶")
  call cornelis#bind_input("st8", "✴")
  call cornelis#bind_input("st12", "✹")
  call cornelis#bind_input("star", "★")
  call cornelis#bind_input("bA", "𝔸")
  call cornelis#bind_input("bB", "𝔹")
  call cornelis#bind_input("bC", "ℂ")
  call cornelis#bind_input("bD", "𝔻")
  call cornelis#bind_input("bE", "𝔼")
  call cornelis#bind_input("bF", "𝔽")
  call cornelis#bind_input("bG", "𝔾")
  call cornelis#bind_input("bH", "ℍ")
  call cornelis#bind_input("bI", "𝕀")
  call cornelis#bind_input("bJ", "𝕁")
  call cornelis#bind_input("bK", "𝕂")
  call cornelis#bind_input("bL", "𝕃")
  call cornelis#bind_input("bM", "𝕄")
  call cornelis#bind_input("bN", "ℕ")
  call cornelis#bind_input("bO", "𝕆")
  call cornelis#bind_input("bP", "ℙ")
  call cornelis#bind_input("bQ", "ℚ")
  call cornelis#bind_input("bR", "ℝ")
  call cornelis#bind_input("bS", "𝕊")
  call cornelis#bind_input("bT", "𝕋")
  call cornelis#bind_input("bU", "𝕌")
  call cornelis#bind_input("bV", "𝕍")
  call cornelis#bind_input("bW", "𝕎")
  call cornelis#bind_input("bX", "𝕏")
  call cornelis#bind_input("bY", "𝕐")
  call cornelis#bind_input("bZ", "ℤ")
  call cornelis#bind_input("bGG", "ℾ")
  call cornelis#bind_input("bGP", "ℿ")
  call cornelis#bind_input("bGS", "⅀")
  call cornelis#bind_input("ba", "𝕒")
  call cornelis#bind_input("bb", "𝕓")
  call cornelis#bind_input("bc", "𝕔")
  call cornelis#bind_input("bd", "𝕕")
  call cornelis#bind_input("be", "𝕖")
  call cornelis#bind_input("bf", "𝕗")
  call cornelis#bind_input("bg", "𝕘")
  call cornelis#bind_input("bh", "𝕙")
  call cornelis#bind_input("bi", "𝕚")
  call cornelis#bind_input("bj", "𝕛")
  call cornelis#bind_input("bk", "𝕜")
  call cornelis#bind_input("bl", "𝕝")
  call cornelis#bind_input("bm", "𝕞")
  call cornelis#bind_input("bn", "𝕟")
  call cornelis#bind_input("bo", "𝕠")
  call cornelis#bind_input("bp", "𝕡")
  call cornelis#bind_input("bq", "𝕢")
  call cornelis#bind_input("br", "𝕣")
  call cornelis#bind_input("bs", "𝕤")
  call cornelis#bind_input("bt", "𝕥")
  call cornelis#bind_input("bu", "𝕦")
  call cornelis#bind_input("bv", "𝕧")
  call cornelis#bind_input("bw", "𝕨")
  call cornelis#bind_input("bx", "𝕩")
  call cornelis#bind_input("by", "𝕪")
  call cornelis#bind_input("bz", "𝕫")
  call cornelis#bind_input("bGg", "ℽ")
  call cornelis#bind_input("bGp", "ℼ")
  call cornelis#bind_input("b0", "𝟘")
  call cornelis#bind_input("b1", "𝟙")
  call cornelis#bind_input("b2", "𝟚")
  call cornelis#bind_input("b3", "𝟛")
  call cornelis#bind_input("b4", "𝟜")
  call cornelis#bind_input("b5", "𝟝")
  call cornelis#bind_input("b6", "𝟞")
  call cornelis#bind_input("b7", "𝟟")
  call cornelis#bind_input("b8", "𝟠")
  call cornelis#bind_input("b9", "𝟡")
  call cornelis#bind_input("B0", "𝟎")
  call cornelis#bind_input("B1", "𝟏")
  call cornelis#bind_input("B2", "𝟐")
  call cornelis#bind_input("B3", "𝟑")
  call cornelis#bind_input("B4", "𝟒")
  call cornelis#bind_input("B5", "𝟓")
  call cornelis#bind_input("B6", "𝟔")
  call cornelis#bind_input("B7", "𝟕")
  call cornelis#bind_input("B8", "𝟖")
  call cornelis#bind_input("B9", "𝟗")
  call cornelis#bind_input("[[", "⟦")
  call cornelis#bind_input("]]", "⟧")
  call cornelis#bind_input("<", "⟨")
  call cornelis#bind_input(">", "⟩")
  call cornelis#bind_input("<<", "⟪")
  call cornelis#bind_input(">>", "⟫")
  call cornelis#bind_input("((", "⦅")
  call cornelis#bind_input("))", "⦆")
  call cornelis#bind_input("{{", "⦃")
  call cornelis#bind_input("}}", "⦄")
  call cornelis#bind_input("(b", "⟅")
  call cornelis#bind_input(")b", "⟆")
  call cornelis#bind_input("lbag", "⟅")
  call cornelis#bind_input("rbag", "⟆")
  call cornelis#bind_input("|>", "⦊")
  call cornelis#bind_input("|)", "⦈")
  call cornelis#bind_input("))", "⦆")
  call cornelis#bind_input("bub", "•")
  call cornelis#bind_input("buw", "◦")
  call cornelis#bind_input("but", "‣")
  call cornelis#bind_input("b", "♭")
  call cornelis#bind_input("#", "♯")
  call cornelis#bind_input("\\", "\\")
  call cornelis#bind_input("en", "–")
  call cornelis#bind_input("em", "—")
  call cornelis#bind_input("!!", "‼")
  call cornelis#bind_input("??", "⁇")
  call cornelis#bind_input("?!", "‽")
  call cornelis#bind_input("!?", "⁉")
  call cornelis#bind_input("8<", "✂")
  call cornelis#bind_input("tie", "⁀")
  call cornelis#bind_input("undertie", "‿")
  call cornelis#bind_input("_~", "̰")
  call cornelis#bind_input("^l-", "⃖")
  call cornelis#bind_input("^r-", "⃗")
  call cornelis#bind_input("^lr", "⃡")
  call cornelis#bind_input("_lr", "͍")
  call cornelis#bind_input("Ga", "α")
  call cornelis#bind_input("GA", "Α")
  call cornelis#bind_input("Gb", "β")
  call cornelis#bind_input("GB", "Β")
  call cornelis#bind_input("Gg", "γ")
  call cornelis#bind_input("GG", "Γ")
  call cornelis#bind_input("Gd", "δ")
  call cornelis#bind_input("GD", "Δ")
  call cornelis#bind_input("Ge", "ε")
  call cornelis#bind_input("GE", "Ε")
  call cornelis#bind_input("Gz", "ζ")
  call cornelis#bind_input("GZ", "Ζ")
  call cornelis#bind_input("Gh", "η")
  call cornelis#bind_input("GH", "Η")
  call cornelis#bind_input("Gth", "θ")
  call cornelis#bind_input("GTH", "Θ")
  call cornelis#bind_input("Gi", "ι")
  call cornelis#bind_input("GI", "Ι")
  call cornelis#bind_input("Gk", "κ")
  call cornelis#bind_input("GK", "Κ")
  call cornelis#bind_input("Gl", "λ")
  call cornelis#bind_input("GL", "Λ")
  call cornelis#bind_input("Gl-", "ƛ")
  call cornelis#bind_input("Gm", "μ")
  call cornelis#bind_input("GM", "Μ")
  call cornelis#bind_input("Gn", "ν")
  call cornelis#bind_input("GN", "Ν")
  call cornelis#bind_input("Gx", "ξ")
  call cornelis#bind_input("GX", "Ξ")
  call cornelis#bind_input("Gr", "ρ")
  call cornelis#bind_input("GR", "Ρ")
  call cornelis#bind_input("Gs", "σ")
  call cornelis#bind_input("GS", "Σ")
  call cornelis#bind_input("Gt", "τ")
  call cornelis#bind_input("GT", "Τ")
  call cornelis#bind_input("Gu", "υ")
  call cornelis#bind_input("GU", "Υ")
  call cornelis#bind_input("Gf", "φ")
  call cornelis#bind_input("GF", "Φ")
  call cornelis#bind_input("Gc", "χ")
  call cornelis#bind_input("GC", "Χ")
  call cornelis#bind_input("Gp", "ψ")
  call cornelis#bind_input("GP", "Ψ")
  call cornelis#bind_input("Go", "ω")
  call cornelis#bind_input("GO", "Ω")
  call cornelis#bind_input("MiA", "𝐴")
  call cornelis#bind_input("MiB", "𝐵")
  call cornelis#bind_input("MiC", "𝐶")
  call cornelis#bind_input("MiD", "𝐷")
  call cornelis#bind_input("MiE", "𝐸")
  call cornelis#bind_input("MiF", "𝐹")
  call cornelis#bind_input("MiG", "𝐺")
  call cornelis#bind_input("MiH", "𝐻")
  call cornelis#bind_input("MiI", "𝐼")
  call cornelis#bind_input("MiJ", "𝐽")
  call cornelis#bind_input("MiK", "𝐾")
  call cornelis#bind_input("MiL", "𝐿")
  call cornelis#bind_input("MiM", "𝑀")
  call cornelis#bind_input("MiN", "𝑁")
  call cornelis#bind_input("MiO", "𝑂")
  call cornelis#bind_input("MiP", "𝑃")
  call cornelis#bind_input("MiQ", "𝑄")
  call cornelis#bind_input("MiR", "𝑅")
  call cornelis#bind_input("MiS", "𝑆")
  call cornelis#bind_input("MiT", "𝑇")
  call cornelis#bind_input("MiU", "𝑈")
  call cornelis#bind_input("MiV", "𝑉")
  call cornelis#bind_input("MiW", "𝑊")
  call cornelis#bind_input("MiX", "𝑋")
  call cornelis#bind_input("MiY", "𝑌")
  call cornelis#bind_input("MiZ", "𝑍")
  call cornelis#bind_input("Mia", "𝑎")
  call cornelis#bind_input("Mib", "𝑏")
  call cornelis#bind_input("Mic", "𝑐")
  call cornelis#bind_input("Mid", "𝑑")
  call cornelis#bind_input("Mie", "𝑒")
  call cornelis#bind_input("Mif", "𝑓")
  call cornelis#bind_input("Mig", "𝑔")
  call cornelis#bind_input("Mih", "ℎ")
  call cornelis#bind_input("Mii", "𝑖")
  call cornelis#bind_input("Mij", "𝑗")
  call cornelis#bind_input("Mik", "𝑘")
  call cornelis#bind_input("Mil", "𝑙")
  call cornelis#bind_input("Mim", "𝑚")
  call cornelis#bind_input("Min", "𝑛")
  call cornelis#bind_input("Mio", "𝑜")
  call cornelis#bind_input("Mip", "𝑝")
  call cornelis#bind_input("Miq", "𝑞")
  call cornelis#bind_input("Mir", "𝑟")
  call cornelis#bind_input("Mis", "𝑠")
  call cornelis#bind_input("Mit", "𝑡")
  call cornelis#bind_input("Miu", "𝑢")
  call cornelis#bind_input("Miv", "𝑣")
  call cornelis#bind_input("Miw", "𝑤")
  call cornelis#bind_input("Mix", "𝑥")
  call cornelis#bind_input("Miy", "𝑦")
  call cornelis#bind_input("Miz", "𝑧")
  call cornelis#bind_input("MIA", "𝑨")
  call cornelis#bind_input("MIB", "𝑩")
  call cornelis#bind_input("MIC", "𝑪")
  call cornelis#bind_input("MID", "𝑫")
  call cornelis#bind_input("MIE", "𝑬")
  call cornelis#bind_input("MIF", "𝑭")
  call cornelis#bind_input("MIG", "𝑮")
  call cornelis#bind_input("MIH", "𝑯")
  call cornelis#bind_input("MII", "𝑰")
  call cornelis#bind_input("MIJ", "𝑱")
  call cornelis#bind_input("MIK", "𝑲")
  call cornelis#bind_input("MIL", "𝑳")
  call cornelis#bind_input("MIM", "𝑴")
  call cornelis#bind_input("MIN", "𝑵")
  call cornelis#bind_input("MIO", "𝑶")
  call cornelis#bind_input("MIP", "𝑷")
  call cornelis#bind_input("MIQ", "𝑸")
  call cornelis#bind_input("MIR", "𝑹")
  call cornelis#bind_input("MIS", "𝑺")
  call cornelis#bind_input("MIT", "𝑻")
  call cornelis#bind_input("MIU", "𝑼")
  call cornelis#bind_input("MIV", "𝑽")
  call cornelis#bind_input("MIW", "𝑾")
  call cornelis#bind_input("MIX", "𝑿")
  call cornelis#bind_input("MIY", "𝒀")
  call cornelis#bind_input("MIZ", "𝒁")
  call cornelis#bind_input("MIa", "𝒂")
  call cornelis#bind_input("MIb", "𝒃")
  call cornelis#bind_input("MIc", "𝒄")
  call cornelis#bind_input("MId", "𝒅")
  call cornelis#bind_input("MIe", "𝒆")
  call cornelis#bind_input("MIf", "𝒇")
  call cornelis#bind_input("MIg", "𝒈")
  call cornelis#bind_input("MIh", "𝒉")
  call cornelis#bind_input("MIi", "𝒊")
  call cornelis#bind_input("MIj", "𝒋")
  call cornelis#bind_input("MIk", "𝒌")
  call cornelis#bind_input("MIl", "𝒍")
  call cornelis#bind_input("MIm", "𝒎")
  call cornelis#bind_input("MIn", "𝒏")
  call cornelis#bind_input("MIo", "𝒐")
  call cornelis#bind_input("MIp", "𝒑")
  call cornelis#bind_input("MIq", "𝒒")
  call cornelis#bind_input("MIr", "𝒓")
  call cornelis#bind_input("MIs", "𝒔")
  call cornelis#bind_input("MIt", "𝒕")
  call cornelis#bind_input("MIu", "𝒖")
  call cornelis#bind_input("MIv", "𝒗")
  call cornelis#bind_input("MIw", "𝒘")
  call cornelis#bind_input("MIx", "𝒙")
  call cornelis#bind_input("MIy", "𝒚")
  call cornelis#bind_input("MIz", "𝒛")
  call cornelis#bind_input("McA", "𝒜")
  call cornelis#bind_input("McB", "ℬ")
  call cornelis#bind_input("McC", "𝒞")
  call cornelis#bind_input("McD", "𝒟")
  call cornelis#bind_input("McE", "ℰ")
  call cornelis#bind_input("McF", "ℱ")
  call cornelis#bind_input("McG", "𝒢")
  call cornelis#bind_input("McH", "ℋ")
  call cornelis#bind_input("McI", "ℐ")
  call cornelis#bind_input("McJ", "𝒥")
  call cornelis#bind_input("McK", "𝒦")
  call cornelis#bind_input("McL", "ℒ")
  call cornelis#bind_input("McM", "ℳ")
  call cornelis#bind_input("McN", "𝒩")
  call cornelis#bind_input("McO", "𝒪")
  call cornelis#bind_input("McP", "𝒫")
  call cornelis#bind_input("McQ", "𝒬")
  call cornelis#bind_input("McR", "ℛ")
  call cornelis#bind_input("McS", "𝒮")
  call cornelis#bind_input("McT", "𝒯")
  call cornelis#bind_input("McU", "𝒰")
  call cornelis#bind_input("McV", "𝒱")
  call cornelis#bind_input("McW", "𝒲")
  call cornelis#bind_input("McX", "𝒳")
  call cornelis#bind_input("McY", "𝒴")
  call cornelis#bind_input("McZ", "𝒵")
  call cornelis#bind_input("Mca", "𝒶")
  call cornelis#bind_input("Mcb", "𝒷")
  call cornelis#bind_input("Mcc", "𝒸")
  call cornelis#bind_input("Mcd", "𝒹")
  call cornelis#bind_input("Mce", "ℯ")
  call cornelis#bind_input("Mcf", "𝒻")
  call cornelis#bind_input("Mcg", "ℊ")
  call cornelis#bind_input("Mch", "𝒽")
  call cornelis#bind_input("Mci", "𝒾")
  call cornelis#bind_input("Mcj", "𝒿")
  call cornelis#bind_input("Mck", "𝓀")
  call cornelis#bind_input("Mcl", "𝓁")
  call cornelis#bind_input("Mcm", "𝓂")
  call cornelis#bind_input("Mcn", "𝓃")
  call cornelis#bind_input("Mco", "ℴ")
  call cornelis#bind_input("Mcp", "𝓅")
  call cornelis#bind_input("Mcq", "𝓆")
  call cornelis#bind_input("Mcr", "𝓇")
  call cornelis#bind_input("Mcs", "𝓈")
  call cornelis#bind_input("Mct", "𝓉")
  call cornelis#bind_input("Mcu", "𝓊")
  call cornelis#bind_input("Mcv", "𝓋")
  call cornelis#bind_input("Mcw", "𝓌")
  call cornelis#bind_input("Mcx", "𝓍")
  call cornelis#bind_input("Mcy", "𝓎")
  call cornelis#bind_input("Mcz", "𝓏")
  call cornelis#bind_input("MCA", "𝓐")
  call cornelis#bind_input("MCB", "𝓑")
  call cornelis#bind_input("MCC", "𝓒")
  call cornelis#bind_input("MCD", "𝓓")
  call cornelis#bind_input("MCE", "𝓔")
  call cornelis#bind_input("MCF", "𝓕")
  call cornelis#bind_input("MCG", "𝓖")
  call cornelis#bind_input("MCH", "𝓗")
  call cornelis#bind_input("MCI", "𝓘")
  call cornelis#bind_input("MCJ", "𝓙")
  call cornelis#bind_input("MCK", "𝓚")
  call cornelis#bind_input("MCL", "𝓛")
  call cornelis#bind_input("MCM", "𝓜")
  call cornelis#bind_input("MCN", "𝓝")
  call cornelis#bind_input("MCO", "𝓞")
  call cornelis#bind_input("MCP", "𝓟")
  call cornelis#bind_input("MCQ", "𝓠")
  call cornelis#bind_input("MCR", "𝓡")
  call cornelis#bind_input("MCS", "𝓢")
  call cornelis#bind_input("MCT", "𝓣")
  call cornelis#bind_input("MCU", "𝓤")
  call cornelis#bind_input("MCV", "𝓥")
  call cornelis#bind_input("MCW", "𝓦")
  call cornelis#bind_input("MCX", "𝓧")
  call cornelis#bind_input("MCY", "𝓨")
  call cornelis#bind_input("MCZ", "𝓩")
  call cornelis#bind_input("MCa", "𝓪")
  call cornelis#bind_input("MCb", "𝓫")
  call cornelis#bind_input("MCc", "𝓬")
  call cornelis#bind_input("MCd", "𝓭")
  call cornelis#bind_input("MCe", "𝓮")
  call cornelis#bind_input("MCf", "𝓯")
  call cornelis#bind_input("MCg", "𝓰")
  call cornelis#bind_input("MCh", "𝓱")
  call cornelis#bind_input("MCi", "𝓲")
  call cornelis#bind_input("MCj", "𝓳")
  call cornelis#bind_input("MCk", "𝓴")
  call cornelis#bind_input("MCl", "𝓵")
  call cornelis#bind_input("MCm", "𝓶")
  call cornelis#bind_input("MCn", "𝓷")
  call cornelis#bind_input("MCo", "𝓸")
  call cornelis#bind_input("MCp", "𝓹")
  call cornelis#bind_input("MCq", "𝓺")
  call cornelis#bind_input("MCr", "𝓻")
  call cornelis#bind_input("MCs", "𝓼")
  call cornelis#bind_input("MCt", "𝓽")
  call cornelis#bind_input("MCu", "𝓾")
  call cornelis#bind_input("MCv", "𝓿")
  call cornelis#bind_input("MCw", "𝔀")
  call cornelis#bind_input("MCx", "𝔁")
  call cornelis#bind_input("MCy", "𝔂")
  call cornelis#bind_input("MCz", "𝔃")
  call cornelis#bind_input("MfA", "𝔄")
  call cornelis#bind_input("MfB", "𝔅")
  call cornelis#bind_input("MfC", "ℭ")
  call cornelis#bind_input("MfD", "𝔇")
  call cornelis#bind_input("MfE", "𝔈")
  call cornelis#bind_input("MfF", "𝔉")
  call cornelis#bind_input("MfG", "𝔊")
  call cornelis#bind_input("MfH", "ℌ")
  call cornelis#bind_input("MfI", "ℑ")
  call cornelis#bind_input("MfJ", "𝔍")
  call cornelis#bind_input("MfK", "𝔎")
  call cornelis#bind_input("MfL", "𝔏")
  call cornelis#bind_input("MfM", "𝔐")
  call cornelis#bind_input("MfN", "𝔑")
  call cornelis#bind_input("MfO", "𝔒")
  call cornelis#bind_input("MfP", "𝔓")
  call cornelis#bind_input("MfQ", "𝔔")
  call cornelis#bind_input("MfR", "ℜ")
  call cornelis#bind_input("MfS", "𝔖")
  call cornelis#bind_input("MfT", "𝔗")
  call cornelis#bind_input("MfU", "𝔘")
  call cornelis#bind_input("MfV", "𝔙")
  call cornelis#bind_input("MfW", "𝔚")
  call cornelis#bind_input("MfX", "𝔛")
  call cornelis#bind_input("MfY", "𝔜")
  call cornelis#bind_input("MfZ", "ℨ")
  call cornelis#bind_input("Mfa", "𝔞")
  call cornelis#bind_input("Mfb", "𝔟")
  call cornelis#bind_input("Mfc", "𝔠")
  call cornelis#bind_input("Mfd", "𝔡")
  call cornelis#bind_input("Mfe", "𝔢")
  call cornelis#bind_input("Mff", "𝔣")
  call cornelis#bind_input("Mfg", "𝔤")
  call cornelis#bind_input("Mfh", "𝔥")
  call cornelis#bind_input("Mfi", "𝔦")
  call cornelis#bind_input("Mfj", "𝔧")
  call cornelis#bind_input("Mfk", "𝔨")
  call cornelis#bind_input("Mfl", "𝔩")
  call cornelis#bind_input("Mfm", "𝔪")
  call cornelis#bind_input("Mfn", "𝔫")
  call cornelis#bind_input("Mfo", "𝔬")
  call cornelis#bind_input("Mfp", "𝔭")
  call cornelis#bind_input("Mfq", "𝔮")
  call cornelis#bind_input("Mfr", "𝔯")
  call cornelis#bind_input("Mfs", "𝔰")
  call cornelis#bind_input("Mft", "𝔱")
  call cornelis#bind_input("Mfu", "𝔲")
  call cornelis#bind_input("Mfv", "𝔳")
  call cornelis#bind_input("Mfw", "𝔴")
  call cornelis#bind_input("Mfx", "𝔵")
  call cornelis#bind_input("Mfy", "𝔶")
  call cornelis#bind_input("Mfz", "𝔷")
  call cornelis#bind_input("_a", "ₐ")
  call cornelis#bind_input("_e", "ₑ")
  call cornelis#bind_input("_h", "ₕ")
  call cornelis#bind_input("_i", "ᵢ")
  call cornelis#bind_input("_j", "ⱼ")
  call cornelis#bind_input("_k", "ₖ")
  call cornelis#bind_input("_l", "ₗ")
  call cornelis#bind_input("_m", "ₘ")
  call cornelis#bind_input("_n", "ₙ")
  call cornelis#bind_input("_o", "ₒ")
  call cornelis#bind_input("_p", "ₚ")
  call cornelis#bind_input("_r", "ᵣ")
  call cornelis#bind_input("_s", "ₛ")
  call cornelis#bind_input("_t", "ₜ")
  call cornelis#bind_input("_u", "ᵤ")
  call cornelis#bind_input("_v", "ᵥ")
  call cornelis#bind_input("_x", "ₓ")
  call cornelis#bind_input("_Gb", "ᵦ")
  call cornelis#bind_input("_Gg", "ᵧ")
  call cornelis#bind_input("_Gr", "ᵨ")
  call cornelis#bind_input("_Gf", "ᵩ")
  call cornelis#bind_input("_Gc", "ᵪ")
  call cornelis#bind_input("^a", "ᵃ")
  call cornelis#bind_input("^b", "ᵇ")
  call cornelis#bind_input("^c", "ᶜ")
  call cornelis#bind_input("^d", "ᵈ")
  call cornelis#bind_input("^e", "ᵉ")
  call cornelis#bind_input("^f", "ᶠ")
  call cornelis#bind_input("^g", "ᵍ")
  call cornelis#bind_input("^h", "ʰ")
  call cornelis#bind_input("^i", "ⁱ")
  call cornelis#bind_input("^j", "ʲ")
  call cornelis#bind_input("^k", "ᵏ")
  call cornelis#bind_input("^l", "ˡ")
  call cornelis#bind_input("^m", "ᵐ")
  call cornelis#bind_input("^n", "ⁿ")
  call cornelis#bind_input("^o", "ᵒ")
  call cornelis#bind_input("^p", "ᵖ")
  call cornelis#bind_input("^r", "ʳ")
  call cornelis#bind_input("^s", "ˢ")
  call cornelis#bind_input("^t", "ᵗ")
  call cornelis#bind_input("^u", "ᵘ")
  call cornelis#bind_input("^v", "ᵛ")
  call cornelis#bind_input("^w", "ʷ")
  call cornelis#bind_input("^x", "ˣ")
  call cornelis#bind_input("^y", "ʸ")
  call cornelis#bind_input("^z", "ᶻ")
  call cornelis#bind_input("^A", "ᴬ")
  call cornelis#bind_input("^B", "ᴮ")
  call cornelis#bind_input("^D", "ᴰ")
  call cornelis#bind_input("^E", "ᴱ")
  call cornelis#bind_input("^G", "ᴳ")
  call cornelis#bind_input("^H", "ᴴ")
  call cornelis#bind_input("^I", "ᴵ")
  call cornelis#bind_input("^J", "ᴶ")
  call cornelis#bind_input("^K", "ᴷ")
  call cornelis#bind_input("^L", "ᴸ")
  call cornelis#bind_input("^M", "ᴹ")
  call cornelis#bind_input("^N", "ᴺ")
  call cornelis#bind_input("^O", "ᴼ")
  call cornelis#bind_input("^P", "ᴾ")
  call cornelis#bind_input("^R", "ᴿ")
  call cornelis#bind_input("^T", "ᵀ")
  call cornelis#bind_input("^U", "ᵁ")
  call cornelis#bind_input("^V", "ⱽ")
  call cornelis#bind_input("^W", "ᵂ")
  call cornelis#bind_input("^Gb", "ᵝ")
  call cornelis#bind_input("^Gg", "ᵞ")
  call cornelis#bind_input("^Gd", "ᵟ")
  call cornelis#bind_input("^Ge", "ᵋ")
  call cornelis#bind_input("^Gth", "ᶿ")
  call cornelis#bind_input("^Gf", "ᵠ")
  call cornelis#bind_input("^Gc", "ᵡ")
  call cornelis#bind_input(" ", " ")
  call cornelis#bind_input("!", "¡")
  call cornelis#bind_input("cent", "¢")
  call cornelis#bind_input("brokenbar", "¦")
  call cornelis#bind_input("degree", "°")
  call cornelis#bind_input("?", "¿")
  call cornelis#bind_input("^a_", "ª")
  call cornelis#bind_input("^o_", "º")

endif

