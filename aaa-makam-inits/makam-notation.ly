%{
  Shortcuts common for Turkish Classical / Ottoman music notation.
  Key Signatures for various makams in plain Latin characters
  Notation for usuls
  
  by Adam Good, 2007
%}

\version "2.14.2"

%looksfaster = {\shiftDurations #1 #0}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TURKISH MUSIC NOTATION STAFF OVERIDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
StaffOverides = {
  \set Staff.extraNatural = ##f
  \override Staff.TimeSignature #'break-visibility = #end-of-line-invisible
  \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
  \set Staff.printKeyCancellation = ##f

  \override Score.BreakAlignment #'break-align-orders =
  #(make-vector  3
    '(left-edge
      clef
      key-cancellation
      staff-bar
      key-signature
      time-signature
  ))
  
  \override Score.BarLine #'space-alist = #'(
    (time-signature . (extra-space . 0.75))
    (custos . (minimum-space . 2.0))
    (clef . (minimum-space . 1.0))
    (key-signature . (extra-space . 1.0))
    (key-cancellation . (extra-space . 1.0))
    (first-note . (fixed-space . 1.3))
    (next-note . (semi-fixed-space . 1.8))
    (right-edge . (extra-space . 0.0))
  )

  \override Score.VoltaBracket #'font-name = #"New Century Schoolbook"
  \override Score.VoltaBracket #'font-size = #-2.0
  \override BreathingSign  #'Y-offset = #3
  \override NoteHead #'font-size = #-1
  
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \override Score.RehearsalMark #'X-offset = #1.3
  \override Score.RehearsalMark #'break-align-symbols = #'(clef)
  \override Score.RehearsalMark #'font-size = #'-0.1
  
}

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% MAKAM MUSIC KEY SIGNATURES
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{buzurk={\set Staff.keySignature=#'((3 . 4/9))}
cargah={\set Staff.keySignature=#'()}
evcara={\set Staff.keySignature=#'((6 . -1/9)(3 . 4/9)(0 . 4/9)(5 . 4/9)(2 . 4/9))}
ferahnak={\set Staff.keySignature=#'((0 . 4/9)(3 . 4/9))}
guldeste={\set Staff.keySignature=#'((1 . 4/9)(3 . 5/9))}
hicaz={\set Staff.keySignature=#'((6 . -4/9)(0 . 4/9))}
hicazkar={\set Staff.keySignature=#'((6 . -1/9)(2 . -4/9)(5 . -4/9)(3 . 4/9))}
kurdi={\set Staff.keySignature=#'((6 . -5/9))}
kurdilihicazkar={\set Staff.keySignature=#'((6 . -5/9)(2 . -5/9)(5 . -5/9))}
mahur={\set Staff.keySignature=#'((3 . 5/9))}
nihavend={\set Staff.keySignature=#'((6 . -5/9)(2 . -5/9))}
nisabur={\set Staff.keySignature=#'((0 . 4/9))}
nisaburek={\set Staff.keySignature=#'((3 . 5/9)(0 . 4/9))}
rast={\set Staff.keySignature=#'((6 . -1/9)(3 . 4/9))}
rengidil={\set Staff.keySignature=#'((5 . -4/9)(6 . -1/9)(1 . -4/9))}
revnaknuma={\set Staff.keySignature=#'((6 . -1/9)(0 . 4/9)(5 . 4/9))}
saba={\set Staff.keySignature=#'((6 . -1/9)(1 . -4/9))}
segah={\set Staff.keySignature=#'((6 . -1/9)(2 . -1/9))}
sultaniyegah={\set Staff.keySignature=#'((6 . -5/9)(0 . 4/9))}
suzidil={\set Staff.keySignature=#'((3 . 1/9)(-3 . 4/9)(1 . 4/9))}
suznak={\set Staff.keySignature=#'((6 . -1/9)(2 . -4/9)(3 . 4/9))}
sedaraban={\set Staff.keySignature=#'((6 . -4/9)(2 . -4/9)(3 . 4/9)(0 . 4/9))}
serefnuma={\set Staff.keySignature=#'((6 . -4/9)(3 . 4/9)(0 . 4/9))}
sevkefza={\set Staff.keySignature=#'((6 . -5/9)(1 . -4/9))}
ussak={\set Staff.keySignature=#'((6 . -1/9))}
vechiarazbar={\set Staff.keySignature=#'((6 . -1/9)(3 . -1/9))}
zirgule={\set Staff.keySignature=#'((6 . -4/9)(3 . 1/9)(0 . 4/9)(4 . 4/9))}

%% MORE KEY SIGNATURES IN COMMON WITH ABOVE
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
muhayyersunbule=\kurdi
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
yenisipihr=\ussak
yegah=\rast
zavil=\mahur
zirefkend=\cargah %}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TUPLETS AND TRIPLETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TupletString = {
  \set tupletSpannerDuration = #(ly:make-moment 1 4) \tupletUp
}

TupletStringSixteenths = {
  \set tupletSpannerDuration = #(ly:make-moment 1 8) \tupletUp
}

Triplet = #(define-music-function (parser location music) (ly:music?)
  #{ \set tupletSpannerDuration = #(ly:make-moment 1 4) \tupletUp \times 2/3
    $music #})

TripletSixteen = #(define-music-function (parser location music) (ly:music?)
  #{ \set tupletSpannerDuration = #(ly:make-moment 1 8) \tupletUp \times 2/3
    $music #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIFFERENT ENDINGS ON REPEATS OR HANEYE MARKINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MyVolta = #(define-music-function (parser location marktext) (markup?)
  #{ 
    \set Score.repeatCommands = #(list (list 'volta
  #{ \markup { $marktext } #}
))
#})

MyVoltaOff = { \set Score.repeatCommands = #'((volta #f)) }

parenthesize =
#(define-music-function (parser loc arg) (ly:music?)
  "Tag @var{arg} to be parenthesized."
  (if (memq 'event-chord (ly:music-property arg 'types)) 
      ; arg is an EventChord -> set the parenthesize property on all child notes and rests
      (map 
	(lambda (ev) 
		(if (or (memq 'note-event (ly:music-property ev 'types))
			(memq 'rest-event (ly:music-property ev 'types)))
		    (set! (ly:music-property ev 'parenthesize) #t)
		)
	)
	(ly:music-property arg 'elements))
      ; No chord, simply set property for this expression:
      (set! (ly:music-property arg 'parenthesize) #t)
  )
  arg)

#(define-public (bracket-stencils grob)
  (let ((lp (grob-interpret-markup grob (markup #:fontsize 3.5 #:translate (cons -0.3 -0.5) "[")))
	(rp (grob-interpret-markup grob (markup #:fontsize 3.5 #:translate (cons -0.3 -0.5) "]"))))
       (list lp rp)))

bracketify = #(define-music-function (parser loc arg) (ly:music?)
  (_i "Tag @var{arg} to be parenthesized.")
  #{
    \once \override ParenthesesItem #'stencils = #bracket-stencils
    \parenthesize $arg
#})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SEGNO AND FINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CustomSegno = #(define-music-function (parser location markp marktext) (string? string?) 
  #{ 
    \once \override Score.RehearsalMark #'break-align-symbols = #'(clef)
    \once \override Score.RehearsalMark #'X-offset = #2.0
    \once \override Score.RehearsalMark #'Y-offset = #1.0
    \once \override Score.RehearsalMark #'font-size = #'0.0
    \mark \markup {\normalsize \musicglyph #"scripts.segno"
      \hspace #0.2 \raise #-1.5 $markp \hspace #0.2 \raise #-1.5 $marktext }
#})

CustomSegnoBox = #(define-music-function (parser location markp marktext) (string? string?) 
  #{ 
    \once \override Score.RehearsalMark #'break-align-symbols = #'(clef)
    \once \override Score.RehearsalMark #'X-offset = #2.0
    \once \override Score.RehearsalMark #'Y-offset = #1.0
    \once \override Score.RehearsalMark #'font-size = #'0.0
    \mark \markup {\normalsize \musicglyph #"scripts.segno"
      \hspace #0.2 \raise #-1.5 \bold \box $markp \hspace #0.2 \raise #-1.5 $marktext }
#})

% Function to print a break after a specific number of beats
breakafter = #(define-music-function (parser location count beats) (integer? ly:music?)
  #{
    \repeat unfold $count $beats \break
  #}
)

% Function to print a specified number of slashes
MySkip = #(define-music-function (parser location count) ( integer?)
  #{
    \repeat unfold $count { \skip1 }
  #}
)

MyRehearse =
#(define-music-function (parser location specify marktext) (pair? string?)
  (make-music 'TextScriptEvent
    'text (markup #:hspace 0.0 #:raise 0.0 #:translate specify #:italic #:fontsize #'-1 marktext)))

MySegno =
#(define-music-function (parser location specify) (pair?)
  (make-music 'TextScriptEvent
    'direction UP
    'text (markup #:hspace 0.0 #:raise 0.0 #:translate specify #:musicglyph "scripts.segno")))

MyCoda = { \mark \markup { \musicglyph #"scripts.coda" } }

#(define-markup-command (rehearse layout props name) (string?)
  (interpret-markup layout props
    (markup #:hspace 0.0 #:raise name)))

% Function to print a specified number of slashes
comp = #(define-music-function (parser location count) ( integer?)
  #{
    \override Rest #'stencil = #ly:percent-repeat-item-interface::beat-slash
    \override Rest #'thickness = #0.48
    \override Rest #'slope = #1.7
    \repeat unfold $count { r4 }
    \revert Rest #'stencil
  #}
)

DaCapoAlFineDOWN=_\markup{ \hspace #-8 \raise #-2.0 "D.C. al fine" }
DaCapoAlFineUP=^\markup{ \hspace #-8 \raise #0.0 "D.C. al fine" }

MessageMU =
#(define-music-function
  (parser location text)
  (string?)
  #{
    \tempo \markup \override #'(font-series . medium) \italic $text
  #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TEXT SPANNERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SpanTextUp = #(define-music-function (parser location spantext) (string?) 
  #{ 
\override TextSpanner #'direction = #UP
\once \override TextSpanner #'(bound-details left padding) = #-0.7
\once \override TextSpanner
    #'(bound-details left text) = \markup { \small { \concat { $spantext } } }
\once \override TextSpanner #'style = #'dashed-line
\once \override TextSpanner
    #'(bound-details left stencil-align-dir-y) = #CENTER
\once \override TextSpanner #'(bound-details right text) =
    \markup { \draw-line #'(0 . -1) }
\once \override TextSpanner #'(bound-details right padding) = #-1
\once \override TextSpanner #'font-shape = #'italic
#})

SpanTextDown = #(define-music-function (parser location spantext) (string?) 
  #{ 
\override TextSpanner #'direction = #DOWN
\once \override TextSpanner #'(bound-details left padding) = #-0.7
\once \override TextSpanner
    #'(bound-details left text) = \markup { \small { \concat { $spantext } } }
\once \override TextSpanner #'style = #'dashed-line
\once \override TextSpanner
    #'(bound-details left stencil-align-dir-y) = #CENTER
\once \override TextSpanner #'(bound-details right text) =
    \markup { \draw-line #'(0 . 1) }
\once \override TextSpanner #'(bound-details right padding) = #-1
\once \override TextSpanner #'font-shape = #'italic
#})

XSpanText = #(define-music-function (parser location fretnum osp dirn shorten adjBreak adjEnd) (string? number? number? pair? number? number?) 
  #{ 
    \once \override TextSpanner #'outside-staff-priority = #$osp 
    \once \override TextSpanner #'bound-details #'left #'text = \markup { \small { \concat { $fretnum " " } } } 
    %\once \override TextSpanner #'style = #'line 
    \once \override TextSpanner #'style = #'dashed-line
    \once \override TextSpanner #'font-shape = #'italic 
    \once \override TextSpanner #'direction = #$dirn
    %draw a bracket edge on RHS 
    \once \override TextSpanner #'bound-details #'right #'text =  \markup { \draw-line #(cons 0 (/ $dirn -1)) } 
    % set alignment of line with reference to left text 
    \once \override TextSpanner #'bound-details #'left #'stencil-align-dir-y = #CENTER 
    % change X pos of LH and RH end as desired 
    \once \override TextSpanner #'bound-details #'left #'padding = #(car $shorten) 
    \once \override TextSpanner #'bound-details #'right #'padding = #(cdr $shorten) 
    % allow adjustment of line end when it wraps to following stave 
    \once \override TextSpanner #'bound-details #'right-broken #'padding = #$adjEnd 
    % adjust LH end of line when it wraps to following stave so that it doesn't 
    % extend to the left of the notes on the stave 
    \once \override TextSpanner #'bound-details #'left-broken #'X = #$adjBreak 
    % optional override to remove text and bracket edge at line breaks 
    \once \override TextSpanner #'bound-details #'left-broken #'text = ##f 
    \once \override TextSpanner #'bound-details #'right-broken #'text = ##f 
#}) 

XXSpanText = #(define-music-function (parser location fretnum osp dirn shorten adjBreak adjEnd) (string? number? number? pair? number? number?) 
  #{ 
%    \once \override TextSpanner #'outside-staff-priority = #$osp 
    \once \override TextSpanner #'(bound-details left text) = \markup { \small { \concat { $fretnum " " } } } 
    \once \override TextSpanner #'style = #'dashed-line
    \once \override TextSpanner #'font-shape = #'italic 
    \once \override TextSpanner #'direction = #UP
    %draw a bracket edge on RHS 
    \once \override TextSpanner #'(bound-details right text) =  \markup { \draw-line #(cons 0 (/ UP -1)) } 
    % set alignment of line with reference to left text 
    \once \override TextSpanner #'bound-details #'left #'stencil-align-dir-y = #CENTER 
    % change X pos of LH and RH end as desired 
    \once \override TextSpanner #'bound-details #'left #'padding = #(car $shorten) 
    \once \override TextSpanner #'bound-details #'right #'padding = #(cdr $shorten) 
    % allow adjustment of line end when it wraps to following stave 
    \once \override TextSpanner #'bound-details #'right-broken #'padding = #$adjEnd 
    % adjust LH end of line when it wraps to following stave so that it doesn't 
    % extend to the left of the notes on the stave 
    \once \override TextSpanner #'bound-details #'left-broken #'X = #$adjBreak 
    % optional override to remove text and bracket edge at line breaks 
    \once \override TextSpanner #'bound-details #'left-broken #'text = ##f 
    \once \override TextSpanner #'bound-details #'right-broken #'text = ##f 
#}) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSPOSITION DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bolahenktuning = {\transposition g,} %%% rast on D
% davudtuning = {\transposition a,} %%% rast on E
% sahtuning = {\transposition bfk,} %%% rast on F
% mansurtuning = {\transposition c,} %%% rast on G
% kiztuning = {\transposition d,} %%% rast on A
% mustahsentuning = {\transposition e,} %%% rast on B
% supurdetuning = {\transposition f,} %%% rast on C
% girift = {\transposition c,}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BREAKING RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
breakUsulOn=\break
breakUsulOff={}

breakSectionOn={\break}
breakSectionOff={}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
