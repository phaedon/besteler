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

  %{\override Score.BreakAlignment #'break-align-orders =
  #(make-vector  3
    '(left-edge
      clef
      key-cancellation
      staff-bar
      key-signature
      time-signature
  )) %}
  
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
  \override TupletBracket #'bracket-visibility = ##t
}

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
      \hspace #0.2 \raise #-1.5 $markp \hspace #0.2 \raise #-1.5 \italic $marktext }
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
\once \override TextSpanner #'(bound-details right padding) = #0
\once \override TextSpanner #'font-shape = #'italic
#})

xSpanTextUp = #(define-music-function (parser location spantext) (string?) 
  #{ 
\override TextSpanner #'direction = #UP
\once \override TextSpanner #'(bound-details left padding) = #-0.7
\once \override TextSpanner #'staff-padding = #6
\once \override TextSpanner
    #'(bound-details left text) = \markup { \small { \concat { $spantext } } }
\once \override TextSpanner #'style = #'line
\once \override TextSpanner
    #'(bound-details left stencil-align-dir-y) = #LEFT
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
