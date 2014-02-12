\version "2.17.6"

%{

  Define 1/9 alterations.

%}


#(define-public EKSIK-IKI 5/18)
#(define-public EKSIK-UC 6/18)

#(define-public KOMA 1/9)
#(define-public BAKIYE 4/9)
#(define-public KUCUK 5/9)
#(define-public BUYUKMUCENNEB 8/9)

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

                     (cbm . ,(ly:make-pitch -1 0 BUYUKMUCENNEB))
                     (dbm . ,(ly:make-pitch -1 1 BUYUKMUCENNEB))
                     (ebm . ,(ly:make-pitch -1 2 BUYUKMUCENNEB))
                     (fbm . ,(ly:make-pitch -1 3 BUYUKMUCENNEB))
                     (gbm . ,(ly:make-pitch -1 4 BUYUKMUCENNEB))
                     (abm . ,(ly:make-pitch -1 5 BUYUKMUCENNEB))
                     (bbm . ,(ly:make-pitch -1 6 BUYUKMUCENNEB))

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

                     (cfi . ,(ly:make-pitch -1 0 (- EKSIK-IKI)))
                     (dfi . ,(ly:make-pitch -1 1 (- EKSIK-IKI)))
                     (efi . ,(ly:make-pitch -1 2 (- EKSIK-IKI)))
                     (ffi . ,(ly:make-pitch -1 3 (- EKSIK-IKI)))
                     (gfi . ,(ly:make-pitch -1 4 (- EKSIK-IKI)))
                     (afi . ,(ly:make-pitch -1 5 (- EKSIK-IKI)))
                     (bfi . ,(ly:make-pitch -1 6 (- EKSIK-IKI)))

                     (cfu . ,(ly:make-pitch -1 0 (- EKSIK-UC)))
                     (dfu . ,(ly:make-pitch -1 1 (- EKSIK-UC)))
                     (efu . ,(ly:make-pitch -1 2 (- EKSIK-UC)))
                     (ffu . ,(ly:make-pitch -1 3 (- EKSIK-UC)))
                     (gfu . ,(ly:make-pitch -1 4 (- EKSIK-UC)))
                     (afu . ,(ly:make-pitch -1 5 (- EKSIK-UC)))
                     (bfu . ,(ly:make-pitch -1 6 (- EKSIK-UC)))


                     (cfbm . ,(ly:make-pitch -1 0 (- BUYUKMUCENNEB)))
                     (dfbm . ,(ly:make-pitch -1 1 (- BUYUKMUCENNEB)))
                     (efbm . ,(ly:make-pitch -1 2 (- BUYUKMUCENNEB)))
                     (ffbm . ,(ly:make-pitch -1 3 (- BUYUKMUCENNEB)))
                     (gfbm . ,(ly:make-pitch -1 4 (- BUYUKMUCENNEB)))
                     (afbm . ,(ly:make-pitch -1 5 (- BUYUKMUCENNEB)))
                     (bfbm . ,(ly:make-pitch -1 6 (- BUYUKMUCENNEB)))

                     )


%% set pitch names.
pitchnames = \makamPitchNames
#(ly:parser-set-note-names parser makamPitchNames)

#(define eksikMirroredSlashedFlat
   (if (defined? 'eksikMirroredSlashedFlat)
       eksikMirroredSlashedFlat #f))

makamGlyphs = #`((1 . "accidentals.doublesharp")
                 (8/9 . "accidentals.sharp.slashslashslash.stemstem")
                 (5/9 . "accidentals.sharp.slashslashslash.stem")
                 (4/9 . "accidentals.sharp")
                 (1/9 . "accidentals.sharp.slashslash.stem")
                 (0 . "accidentals.natural")
                 (-1/9 . "accidentals.mirroredflat")
                 (-5/18 . ,(if eksikMirroredSlashedFlat
                               "accidentals.mirroredflat.backslash"
                               "accidentals.mirroredflat"))
                 (-6/18 . ,(if eksikMirroredSlashedFlat
                               "accidentals.mirroredflat.backslash"
                               "accidentals.mirroredflat"))
                 (-4/9 . "accidentals.flat.slash")
                 (-5/9 . "accidentals.flat")
                 (-8/9 . "accidentals.flat.slashslash")
                 (-1 . "accidentals.flatflat"))

%% set key signatures

buzurkDEV = #'((3 . 4/9))
cargahDEV = #'((0 . 0/9))
evcaraDEV = #'((6 . -1/9)(3 . 4/9)(0 . 4/9)(5 . 4/9)(2 . 4/9))
ferahnakDEV = #'((0 . 4/9)(3 . 4/9))
guldesteDEV = #'((1 . 4/9)(3 . 5/9))
hicazDEV = #'((6 . -4/9)(0 . 4/9))
hicazkarDEV = #'((6 . -1/9)(2 . -4/9)(5 . -4/9)(3 . 4/9))
kurdiDEV = #'((6 . -5/9))
kurdilihicazkarDEV = #'((6 . -5/9)(2 . -5/9)(5 . -5/9))
mahurDEV = #'((3 . 5/9))
nihavendDEV = #'((6 . -5/9)(2 . -5/9))
nisaburDEV = #'((0 . 4/9))
nisaburekDEV = #'((3 . 5/9)(0 . 4/9))
rastDEV = #`((3 . ,4/9) (6 . ,-1/9))
rengidilDEV = #'((5 . -4/9)(6 . -1/9)(1 . -4/9))
revnaknumaDEV = #'((6 . -1/9)(0 . 4/9)(5 . 4/9))
sabaDEV = #'((6 . -1/9)(1 . -4/9))
segahDEV = #'((6 . -1/9)(2 . -1/9))
sultaniyegahDEV = #'((6 . -5/9)(0 . 4/9))
suzidilDEV = #'((3 . 1/9)(-3 . 4/9)(1 . 4/9))
suznakDEV = #'((6 . -1/9)(2 . -4/9)(3 . 4/9))
sedarabanDEV = #'((6 . -4/9)(2 . -4/9)(3 . 4/9)(0 . 4/9))
serefnumaDEV = #'((6 . -4/9)(3 . 4/9)(0 . 4/9))
sevkefzaDEV = #'((6 . -5/9)(1 . -4/9))
ussakDEV = #'((6 . -1/9))
vechiarazbarDEV = #'((6 . -1/9)(3 . -1/9))
zirguleDEV = #'((6 . -4/9)(3 . 1/9)(0 . 4/9)(4 . 4/9))

acemDEV=\ussakDEV
acemasiranDEV=\kurdiDEV
acemkurdiDEV=\kurdiDEV
arazbarDEV=\segahDEV
askefzaDEV=\cargahDEV
bestenigarDEV=\sabaDEV
beyatiDEV=\ussakDEV
beyatiarabanDEV=\suznakDEV
buselikDEV=\cargahDEV
buselikasiranDEV=\buzurkDEV
canfezaDEV=\sabaDEV
cargahOLDDEV=\sabaDEV
dilkesideDEV=\rastDEV
dilkeshaveranDEV=\rastDEV
dugahDEV=\sabaDEV
eskisipihrDEV=\sabaDEV
evicDEV=\rastDEV
ferahfezaDEV=\kurdiDEV
ferahnumaDEV=\nihavendDEV
gerdaniyeDEV=\rastDEV
gulizarDEV=\rastDEV
hisarDEV=\ussakDEV
hisarbuselikDEV=\cargahDEV
huseyniDEV=\rastDEV
huseyniasiranDEV=\rastDEV
huzzamDEV=\suznakDEV
irakDEV=\rastDEV
isfahanDEV=\ussakDEV
isfahanekDEV=\ussakDEV
karcigarDEV=\suznakDEV
kucekDEV=\sabaDEV
muhayyerDEV=\rastDEV
muhayyerkurdiDEV=\kurdiDEV
muhayyersunbuleDEV=\kurdiDEV
mustearDEV=\segahDEV
nevaDEV=\rastDEV
neveserDEV=\sedarabanDEV
nihavendDEV=\nihavendDEV
nikrizDEV=\hicazDEV
nuhuftDEV=\rastDEV
pencgahDEV=\rastDEV
pesendideDEV=\rastDEV
rahatulervahDEV=\serefnumaDEV
rehaviDEV=\rastDEV
ruhnuvazDEV=\mahurDEV
sabazemzemeDEV=\sabaDEV
sazkarDEV=\rastDEV
sevkedilDEV=\rastDEV
sultaniirakDEV=\rastDEV
sultanisegahDEV=\segahDEV
suzidilaraDEV=\cargahDEV
sehnazDEV=\hicazDEV
sehnazbuselikDEV=\cargahDEV
sevkaverDEV=\segahDEV
sevkitarabDEV=\sabaDEV
sivenumaDEV=\sabaDEV
tahirbuselikDEV=\rastDEV
tahirDEV=\rastDEV
tarzicedidDEV=\kurdiDEV
yenisipihrDEV=\ussakDEV
yegahDEV=\rastDEV
zavilDEV=\mahurDEV
zirefkendDEV=\cargahDEV

\layout {
  \context {
    \Score
    \override KeySignature.glyph-name-alist = \makamGlyphs
    \override Accidental.glyph-name-alist = \makamGlyphs
    \override AccidentalCautionary.glyph-name-alist = \makamGlyphs
    \override TrillPitchAccidental.glyph-name-alist = \makamGlyphs
    \override AmbitusAccidental.glyph-name-alist = \makamGlyphs
  }

  \context {
    \Score
    keyAlterationOrder =
    #`(
       (6 . ,-1/9)(3 . ,4/9)(0 . ,4/9)
       )
  }

  %{\context {
    \Score
    keyAlterationOrder =
    #`(
       (6 . -1/9)(3 . 4/9)(0 . 4/9)(5 . 4/9)(2 . 4/9)
       (0 . 4/9)(3 . 4/9)
       (1 . 4/9)(3 . 5/9)
       (6 . -4/9)(0 . 4/9)
       (6 . -1/9)(2 . -4/9)(5 . -4/9)(3 . 4/9)
       (6 . -5/9)
       (6 . -5/9)(2 . -5/9)(5 . -5/9)
       (3 . 5/9)
       (6 . -5/9)(2 . -5/9)
       (0 . 4/9)
       (3 . 5/9)(0 . 4/9)
       (3 . ,4/9) (6 . ,-1/9)
       (5 . -4/9)(6 . -1/9)(1 . -4/9)
       (6 . -1/9)(0 . 4/9)(5 . 4/9)
       (6 . -1/9)(1 . -4/9)
       (6 . -1/9)(2 . -1/9)
       (6 . -5/9)(0 . 4/9)
       (3 . 1/9)(-3 . 4/9)(1 . 4/9)
       (6 . -1/9)(2 . -4/9)(3 . 4/9)
       (6 . -4/9)(2 . -4/9)(3 . 4/9)(0 . 4/9)
       (6 . -4/9)(3 . 4/9)(0 . 4/9)
       (6 . -5/9)(1 . -4/9)
       (6 . -1/9)
       (6 . -1/9)(3 . -1/9)
       (6 . -4/9)(3 . 1/9)(0 . 4/9)(4 . 4/9)
       )
  } %}
}

