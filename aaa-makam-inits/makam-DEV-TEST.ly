\version "2.17.6"

%{

Define 1/9 alterations.

%}

#(define-public KOMA 1/8)
#(define-public BAKIYE 4/8)
#(define-public KUCUK 5/8)


%{

Define pitch names

%}

makamPitchNames = #`(
  (c . ,(ly:make-pitch -1 0 NATURAL))
  (d . ,(ly:make-pitch -1 1 NATURAL))
  (e . ,(ly:make-pitch -1 2 NATURAL))
  (f . ,(ly:make-pitch -1 3 NATURAL))
  (g . ,(ly:make-pitch -1 4 NATURAL))
  (a . ,(ly:make-pitch -1 5 NATURAL))
  (b . ,(ly:make-pitch -1 6 NATURAL))
  
  (cc . ,(ly:make-pitch -1 0 KOMA))
  (dc . ,(ly:make-pitch -1 1 KOMA))
  (ec . ,(ly:make-pitch -1 2 KOMA))
  (fc . ,(ly:make-pitch -1 3 KOMA))
  (gc . ,(ly:make-pitch -1 4 KOMA))
  (ac . ,(ly:make-pitch -1 5 KOMA))
  (bc . ,(ly:make-pitch -1 6 KOMA))

  (cb . ,(ly:make-pitch -1 0 BAKIYE))
  (db . ,(ly:make-pitch -1 1 BAKIYE))
  (eb . ,(ly:make-pitch -1 2 BAKIYE))
  (fb . ,(ly:make-pitch -1 3 BAKIYE))
  (gb . ,(ly:make-pitch -1 4 BAKIYE))
  (ab . ,(ly:make-pitch -1 5 BAKIYE))
  (bb . ,(ly:make-pitch -1 6 BAKIYE))

  (ck . ,(ly:make-pitch -1 0 KUCUK))
  (dk . ,(ly:make-pitch -1 1 KUCUK))
  (ek . ,(ly:make-pitch -1 2 KUCUK))
  (fk . ,(ly:make-pitch -1 3 KUCUK))
  (gk . ,(ly:make-pitch -1 4 KUCUK))
  (ak . ,(ly:make-pitch -1 5 KUCUK))
  (bk . ,(ly:make-pitch -1 6 KUCUK))


  ;; f for flat.
  (cfc . ,(ly:make-pitch -1 0 (- KOMA)))
  (dfc . ,(ly:make-pitch -1 1 (- KOMA)))
  (efc . ,(ly:make-pitch -1 2 (- KOMA)))
  (ffc . ,(ly:make-pitch -1 3 (- KOMA)))
  (gfc . ,(ly:make-pitch -1 4 (- KOMA)))
  (afc . ,(ly:make-pitch -1 5 (- KOMA)))
  (bfc . ,(ly:make-pitch -1 6 (- KOMA)))
  
  (cfb . ,(ly:make-pitch -1 0 (- BAKIYE)))
  (dfb . ,(ly:make-pitch -1 1 (- BAKIYE)))
  (efb . ,(ly:make-pitch -1 2 (- BAKIYE)))
  (ffb . ,(ly:make-pitch -1 3 (- BAKIYE)))
  (gfb . ,(ly:make-pitch -1 4 (- BAKIYE)))
  (afb . ,(ly:make-pitch -1 5 (- BAKIYE)))
  (bfb . ,(ly:make-pitch -1 6 (- BAKIYE)))

  (cfk . ,(ly:make-pitch -1 0 (- KUCUK)))
  (dfk . ,(ly:make-pitch -1 1 (- KUCUK)))
  (efk . ,(ly:make-pitch -1 2 (- KUCUK)))
  (ffk . ,(ly:make-pitch -1 3 (- KUCUK)))
  (gfk . ,(ly:make-pitch -1 4 (- KUCUK)))
  (afk . ,(ly:make-pitch -1 5 (- KUCUK)))
  (bfk . ,(ly:make-pitch -1 6 (- KUCUK)))
  



)


%% set pitch names.
pitchnames = \makamPitchNames 
#(ly:parser-set-note-names parser makamPitchNames)

#(define eksikMirroredSlashedFlat
  (if (defined? 'eksikMirroredSlashedFlat)
       eksikMirroredSlashedFlat #f))

makamGlyphs = #`((1 . "accidentals.doublesharp")
       (5/8 . "accidentals.sharp.slashslashslash.stem")
       (4/8 . "accidentals.sharp")
       (1/8 . "accidentals.sharp.slashslash.stem")
       (0 . "accidentals.natural")
       (-1/8 . "accidentals.mirroredflat")

       (-4/8 . "accidentals.flat.slash")
       (-5/8 . "accidentals.flat")
       (-1 . "accidentals.flatflat"))

%% set key signatures

buzurk = #'((3 . 4/8))
cargah = #'((0 . 0/8))
evcara = #'((6 . -1/8)(3 . 4/8)(0 . 4/8)(5 . 4/8)(2 . 4/8))
ferahnak = #'((0 . 4/8)(3 . 4/8))
guldeste = #'((1 . 4/8)(3 . 5/8))
hicaz = #'((6 . -4/8)(0 . 4/8))
hicazkar = #'((6 . -1/8)(2 . -4/8)(5 . -4/8)(3 . 4/8))
kurdi = #'((6 . -5/8))
kurdilihicazkar = #'((6 . -5/8)(2 . -5/8)(5 . -5/8))
mahur = #'((3 . 5/8))
nihavend = #'((6 . -5/8)(2 . -5/8))
nisabur = #'((0 . 4/8))
nisaburek = #'((3 . 5/8)(0 . 4/8))
rast = #`((6 . -1/8)(3 . 4/8))
rengidil = #'((5 . -4/8)(6 . -1/8)(1 . -4/8))
%revnaknuma = #'((6 . -1/8)(2 . -1/8)(0 . 4/8))
saba = #'((6 . -1/8)(1 . -4/8))
segah = #'((6 . -1/8)(2 . -1/8)(3 . 4/8))
segahmaye = #'((6 . -1/8)(2 . -1/8)(3 . 4/8))
sultaniyegah = #'((6 . -5/8)(0 . 4/8))
suzidil = #'((3 . 1/8)(-3 . 4/8)(1 . 4/8))
suznak = #'((6 . -1/8)(2 . -4/8)(3 . 4/8))
sedaraban = #'((6 . -4/8)(2 . -4/8)(3 . 4/8)(0 . 4/8))
serefnuma = #'((6 . -4/8)(3 . 4/8)(0 . 4/8))
sevkefza = #'((6 . -5/8)(1 . -4/8))
ussak = #'((6 . -1/8))
zirgule = #'((6 . -4/8)(3 . 1/8)(0 . 4/8)(4 . 4/8))

acem=\ussak
acemasiran=\kurdi
acemkurdi=\kurdi
arazbar=\segah
askefza=\cargah
bestenigar=\saba
beyati=\ussak
beyatiaraban=\suznak
buselik=\cargah
buselikasiran=\buzurk
canfeza=\saba
cargahOLD=\saba
dilkeside=\rast
dilkeshaveran=\rast
dugah=\saba
eskisipihr=\saba
evic=\rast
ferahfeza=\kurdi
ferahnuma=\nihavend
gerdaniye=\rast
gulizar=\rast
hicazuzzal=\serefnuma
hisar=\ussak
hisarbuselik=\cargah
huseyni=\rast
huseyniasiran=\rast
huzzam=\suznak
irak=\rast
isfahan=\ussak
isfahanek=\ussak
karcigar=\suznak
kucek=\saba
muhayyer=\rast
muhayyerkurdi=\kurdi
muhayyersunbule=\saba
mustear=\segah
neva=\rast
neveser=\sedaraban
nihavend=\nihavend
nikriz=\hicaz
nuhuft=\rast
pencgah=\rast
pesendide=\rast
rahatulervah=\serefnuma
rehavi=\rast
revnaknuma=\segah
ruhnuvaz=\mahur
sabazemzeme=\saba
sazkar=\rast
sevkedil=\rast
sultaniirak=\rast
sultanisegah=\segah
suzidilara=\cargah
sehnaz=\hicaz
sehnazbuselik=\cargah
sevkaver=\segah
sevkitarab=\saba
sivenuma=\saba
tahirbuselik=\rast
tahir=\rast
tarzicedid=\kurdi
vechiarazbar=\segah
yenisipihr=\ussak
yegah=\rast
zavil=\mahur
zirefkend=\cargah

\layout {
  \context {
    \Score
    \override KeySignature.glyph-name-alist = \makamGlyphs
    \override Accidental.glyph-name-alist = \makamGlyphs
    \override AccidentalCautionary.glyph-name-alist = \makamGlyphs
    \override TrillPitchAccidental.glyph-name-alist = \makamGlyphs
    \override AmbitusAccidental.glyph-name-alist = \makamGlyphs

    keyAlterationOrder = #`(
       (6 . ,-1/8)(6 . ,-4/8)(6 . ,-5/8)
       (1 . ,-4/8)
       (2 . ,-4/8)
       (5 . ,-4/8)
       (3 . ,1/8)(2 . ,-1/8)(3 . ,4/8)(3 . ,5/8)
       (2 . ,-5/8)
       (4 . ,4/8)
       (1 . ,4/8)
       (5 . ,-5/8)
       (0 . ,4/8)
       (5 . ,4/8)
       (2 . ,4/8)
       )

\override KeySignature #'padding-pairs = #'(
    (("accidentals.mirroredflat" . "accidentals.flat.slash") . 0.5)
    (("accidentals.mirroredflat" . "accidentals.mirroredflat") . 0.5)
    (("accidentals.mirroredflat" . "accidentals.sharp") . 0.5)
    (("accidentals.flat.slash" . "accidentals.flat.slash") . 0.1)
    (("accidentals.flat" . "accidentals.flat.slash") . 0.3)
    (("accidentals.flat.slash" . "accidentals.sharp") . 0.5)
    (("accidentals.flat" . "accidentals.flat") . 0.3)
    (("accidentals.sharp" . "accidentals.sharp") . 0.3)
    (("accidentals.flat" . "accidentals.sharp") . 0.5)
    (("accidentals.sharp.slashslash.stem" . "accidentals.sharp") . 0.3)
  )

  }
}

