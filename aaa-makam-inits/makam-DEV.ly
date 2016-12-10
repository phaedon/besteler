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

Buzurk = #'((3 . 4/9))
Cargah = #'((0 . 0/9))
Evcara = #'((6 . -1/9)(3 . 4/9)(0 . 4/9)(5 . 4/9)(2 . 4/9))
Ferahnak = #'((0 . 4/9)(3 . 4/9))
Guldeste = #'((1 . 4/9)(3 . 5/9))
Hicaz = #'((6 . -4/9)(0 . 4/9))
Hicazkar = #'((6 . -1/9)(2 . -4/9)(5 . -4/9)(3 . 4/9))
Kurdi = #'((6 . -5/9))
KurdiliHicazkar = #'((6 . -5/9)(2 . -5/9)(5 . -5/9))
Mahur = #'((3 . 5/9))
Nihavend = #'((6 . -5/9)(2 . -5/9))
Nisabur = #'((0 . 4/9))
Nisaburek = #'((3 . 5/9)(0 . 4/9))
Rast = #`((6 . -1/9)(3 . 4/9))
Rengidil = #'((5 . -4/9)(6 . -1/9)(1 . -4/9))
Saba = #'((6 . -1/9)(1 . -4/9))
Segah = #'((6 . -1/9)(2 . -1/9)(3 . 4/9))
Segahmaye = #'((6 . -1/9)(2 . -1/9)(3 . 4/9))
SultaniYegah = #'((6 . -5/9)(0 . 4/9))
Suzidil = #'((3 . 1/9)(-3 . 4/9)(1 . 4/9))
Suznak = #'((6 . -1/9)(2 . -4/9)(3 . 4/9))
SedAraban = #'((6 . -4/9)(2 . -4/9)(3 . 4/9)(0 . 4/9))
Serefnuma = #'((6 . -4/9)(3 . 4/9)(0 . 4/9))
Sevkefza = #'((6 . -5/9)(1 . -4/9))
Ussak = #'((6 . -1/9))
Zirgule = #'((6 . -4/9)(3 . 1/9)(0 . 4/9)(4 . 4/9))

Acem=\Ussak
AcemAsiran=\Kurdi
AcemKurdi=\Kurdi
Arazbar=\Segah
Askefza=\Cargah
BesteNigar=\Saba
Beyati=\Ussak
BeyatiAraban=\Suznak
Buselik=\Cargah
BuselikAsiran=\Buzurk
Canfeza=\Saba
CargahOLD=\Saba
Dilkeside=\Rast
Dilkeshaveran=\Rast
Dugah=\Saba
Eskisipihr=\Saba
Evic=\Rast
Ferahfeza=\Kurdi
Ferahnuma=\Nihavend
Gerdaniye=\Rast
Gulizar=\Rast
HicazUzzal=\Serefnuma
Hisar=\Ussak
HisarBuselik=\Cargah
Huseyni=\Rast
HuseyniAsiran=\Rast
Huzzam=\Suznak
Irak=\Rast
Isfahan=\Ussak
Isfahanek=\Ussak
Karcigar=\Suznak
Kucek=\Saba
Muhayyer=\Rast
MuhayyerKurdi=\Kurdi
MuhayyerSunbule=\Saba
Mustear=\Segah
Neva=\Rast
Neveser=\SedAraban
Nihavend=\Nihavend
Nikriz=\Hicaz
Nuhuft=\Rast
Pencgah=\Rast
Pesendide=\Rast
Rahatulervah=\Serefnuma
Rehavi=\Rast
Revnaknuma=\Segah
Ruhnuvaz=\Mahur
SabaZemzeme=\Saba
Sazkar=\Rast
Sevkedil=\Rast
SultaniIrak=\Rast
SultaniSegah=\Segah
Suzidilara=\Cargah
Sehnaz=\Hicaz
SehnazBuselik=\Cargah
Sevkaver=\Segah
Sevkitarab=\Saba
Sivenuma=\Saba
TahirBuselik=\Rast
Tahir=\Rast
Tarzicedid=\Kurdi
VechiArazbar=\Segah
YeniSipihr=\Ussak
Yegah=\Rast
Zavil=\Mahur
Zirefkend=\Cargah

\layout {
  \context {
    \Score
    \override KeySignature.glyph-name-alist = \makamGlyphs
    \override Accidental.glyph-name-alist = \makamGlyphs
    \override AccidentalCautionary.glyph-name-alist = \makamGlyphs
    \override TrillPitchAccidental.glyph-name-alist = \makamGlyphs
    \override AmbitusAccidental.glyph-name-alist = \makamGlyphs

    keyAlterationOrder = #`(
       (6 . ,-1/9)(6 . ,-4/9)(6 . ,-5/9)
       (1 . ,-4/9)
       (2 . ,-4/9)
       (5 . ,-4/9)
       (3 . ,1/9)(2 . ,-1/9)(3 . ,4/9)(3 . ,5/9)
       (2 . ,-5/9)
       (4 . ,4/9)
       (1 . ,4/9)
       (5 . ,-5/9)
       (0 . ,4/9)
       (5 . ,4/9)
       (2 . ,4/9)
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

