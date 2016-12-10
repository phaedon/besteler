%{
  Notation common for Turkish Classical / Ottoman Usul notation.

  by Adam Good, 2012
%}

\version "2.14.2"

drumPitchNames =
#'(
    (kdh . hiconga)
    (kdl . loconga)
    )

midiDrumPitches = #`(
                      (hiconga . ,(ly:make-pitch 0 2 FLAT))
                      (loconga . ,(ly:make-pitch 0 2 NATURAL))

                      ;; "transposing" pitches:
                      (oneup . ,(ly:make-pitch 0 1 NATURAL))
                      (Twoup . ,(ly:make-pitch 0 2 NATURAL))
                      (threeup . ,(ly:make-pitch 0 3 NATURAL))
                      (Fourup . ,(ly:make-pitch 0 4 NATURAL))
                      (fiveup . ,(ly:make-pitch 0 5 NATURAL))
                      (onedown . ,(ly:make-pitch -1 6 NATURAL))
                      (Twodown . ,(ly:make-pitch -1 5 NATURAL))
                      (threedown . ,(ly:make-pitch -1 4 NATURAL))
                      (Fourdown . ,(ly:make-pitch -1 3 NATURAL))
                      (fivedown . ,(ly:make-pitch -1 2 NATURAL))
                      )

#(map
  (lambda (k-v)
    (module-define! (current-module)
      (car k-v)
      (alist->hash-table (cdr k-v)))
    )
  '((kudum-style .
      (
        (loconga () #f -1)
        (hiconga () #f 1)
        ))

    ))

% These lines define the position of the kudum drums in the stave;
% if you like, you can change it or you can use special note heads
% for the kudums.
#(define mydrums '((hiconga default #t -5)
                   (loconga default #t 5)))

kudumstaff = {

  % This defines a staff with only Two lines.
  % It also defines the positions of the Two lines.
  \override Staff.StaffSymbol #'line-positions = #'(5 -5)

  % This is neccessary; if not entered, the barline would be too short!
  %\override Staff.BarLine #'bar-size = #2

  \autoBeamOff

  \override Stem #'length = #6.50
  \set DrumStaff.drumStyleTable = #(alist->hash-table mydrums)

  \override Staff.TimeSignature #'break-visibility = #end-of-line-invisible

  \override Score.VoltaBracket #'font-name = #"New Century Schoolbook"
  \override Score.VoltaBracket #'font-size = #-2.0
  \override BreathingSign  #'Y-offset = #3
  \override NoteHead #'font-size = #-1

  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \override Score.RehearsalMark #'X-offset = #0
  \override Score.RehearsalMark #'extra-spacing-hEight = #'(3 . 3)
  \override Score.RehearsalMark #'break-align-symbols = #'(clef)
  \override Score.RehearsalMark #'font-size = #'-0.1

}

#(set! paper-alist (cons '("usul size" . (cons (* 11 in) (* 8.5 in))) paper-alist))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% USUL SPECIFICS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compound time sig with parentheses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parenthps = #"
0 0 translate
45 45 scale
0 0 0 setrgbcolor
[] 0 setdash
1 setlinewidth
0 setlinejoin
0 setlinecap
newpath
0.0147465 0 moveto
0.011106 0.00351338 0.00916298 0.00598483 0.00710032 0.00978362 curveto
0.00248893 0.01832964 3e-008 0.02925362 3e-008 0.04074722 curveto
3e-008 0.05157272 0.00218483 0.06173772 0.00625033 0.07019282 curveto
0.00855727 0.07474937 0.01061993 0.07759971 0.0147465 0.08158922 curveto
0.0147465 0.07807584 lineto
0.00867816 0.06971302 0.00625033 0.05898344 0.00625033 0.04074724 curveto
0.00625033 0.02250984 0.00867816 0.01187244 0.0147465 0.00351344 curveto
0.0147465 6e-008 lineto
0.0147465 0 lineto
closepath
fill"

parenthL = \markup
{
  \with-dimensions #'(0 . 0.68) #'(0 . 3.67)
  \postscript #parenthps
}

parenthR = \markup { \rotate #180 \parenthL }

#(define (compound-time-parentheses one Two num)
   (markup #:override '(baseline-skip . 0) #:number
     (#:line
      (

        #:concat (#:vcenter parenthL #:hspace 0.2 (#:left-align (#:center-column (one num))) #:hspace 0.2 #:vcenter parenthR)

        #:left-align (#:center-column (Two num))


        ))))


%% BEAMING
SemaiBeams = {
  % BEAMING FOR 3/4
  \set Timing.beatStructure = #'(1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

SofyanBeams = {
  % BEAMING FOR 4/4
  \set Timing.beatStructure = #'(1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

NimBerefsanBeams = {
  % BEAMING FOR NIMBEREFSAN USUL
  \set Timing.beatStructure = #'(2 2)
}

EvsatBeams = {
  % BEAMING FOR EVSAT USUL...the 5/8 needs this
  \set Timing.beatStructure = #'(2 2 1)
}

TurkAksagiBeams = {
  % BEAMING FOR 5/8
  \set Timing.beatStructure = #'(2 2 1)
}

YurukSemaiBeams = {
  % BEAMING FOR 6/8
  \set Timing.beatStructure = #'(3 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 2 4 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

SixTwoBeams = {
  % BEAMING FOR 6/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2 2 2 2 2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

SenginSemaiBeams = {
  % BEAMING FOR 6/4
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  %{ \set Timing.beamExceptions =
     #'(;start of alist
     (end . ;entry for end of Beams
     ( ;start of alist of end points
     ((1 . 16) . (4 4 4 4 4 4)) ;rule for 1/32 Beams -- end each 1/16
     ))) %close all entries %}
}

SenginSemaiEightBeams = {
  % BEAMING FOR sengin Semai 6/8
  \set Timing.beatStructure = #'(2 1 1 2)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (2 2 2 2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

DevriHindiBeams = {
  % BEAMING FOR 7/8 DEVRI HINDI
  \set Timing.beatStructure = #'(2 1 2 2)
}

DevriTuranBeams = {
  % BEAMING FOR 7/8 DEVRI TURAN
  \set Timing.beatStructure = #'(2 2 2 1)
}

DuyekBeams = {
  % BEAMING FOR 8/8
  \set Timing.beatStructure = #'(2 2 2 2)
}

MusemmenBeams = {
  % BEAMING FOR 8/8
  \set Timing.beatStructure = #'(3 2 3)
}

AksakBeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 2 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

EvferBeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

RaksAksagiBeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

ZeybekBeams = {
  % BEAMING FOR 9/8 ZEYBEK
  \set Timing.beatStructure = #'(2 2 2 2 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

AgirZeybekBeams = {
  % BEAMING FOR 9/4 ZEYBEK
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1)
  %{\set Timing.beamExceptions =
    #'(                         ;start of alist
    (end .                   ;entry for end of Beams
    (                       ;start of alist of end points
    ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 Beams -- end each 1/16
    )))        %}             %close all entries
}

OynakBeams = {
  % BEAMING FOR 9/8 OYNAK
  \set Timing.beatStructure = #'(2 1 2 2 2 )
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 16) . (4 2 4 4 4 ))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

AksakSemaiBeams = {
  % BEAMING FOR 10/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1)
}

CurcunaBeams = {
  % BEAMING FOR 10/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1)
}

CurcunaSixteenBeams = {
  % BEAMING FOR 10/16 CURCUNA
  \set Timing.beatStructure = #'(3 2 2 3)
}

AyinDevriRevaniBeams = {
  % BEAMING FOR 14/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1 2 2)
}

LongbarBeams = {
  % BEAMING FOR NIMBEREFSAN USUL
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2 2 2 2 2 2 2 2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}


SixteenTwoBeams = {
  % BEAMING FOR 16/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2 2 2 2 2 2 2))   ;rule for 1/32 Beams -- end each 1/16
          )))                     %close all entries
}

CifteDuyekBeams = {
  % BEAMING FOR 16/4
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
}

%% NIBAR
increaseBarNumber = \applyContext
#(lambda (x)
   (let ((measurepos (ly:context-property x 'measurePosition)))
     (if (< 0 (ly:moment-main-numerator measurepos))
         (begin
          (ly:context-set-property!
           (ly:context-property-where-defined x 'internalBarNumber)
           'internalBarNumber
           (1+ (ly:context-property x 'internalBarNumber)))
          (ly:context-set-property!
           (ly:context-property-where-defined x 'currentBarNumber)
           'currentBarNumber
           (1+ (ly:context-property x 'currentBarNumber)))
          (ly:context-set-property!
           (ly:context-property-where-defined x 'measurePosition)
           'measurePosition
           (ly:make-moment 0 1
             (ly:moment-grace-numerator measurepos)
             (ly:moment-grace-denominator measurepos)))))))


nibar = #(define-music-function (parser location x) (string?)
           #{
             \bar $x
             \increaseBarNumber
           #})

%% SOME THINGS FOR MAKING USUL NOTATION
#(define mydrums '((ridebell default #t  3)
                   (cowbell  default #t -3)))


%% USUL NOTATION OVERRIDES
usulnotation = {
  \override Staff.StaffSymbol #'line-count = 3
  \override Staff.StaffSymbol #'staff-space = #2.0
  \override Stem #'length = #3.5
  \override Staff.TimeSignature #'break-visibility = #end-of-line-invisible
  \override Score.Clef #'stencil = ##f
  \autoBeamOff
  \clef percussion
}

%% DUM'S AND TEK'S
DUM=\markup {\fontsize #-2 \halign #-.4 \center-align {"Düm"}}
DU=\markup {\fontsize #-2 \halign #-.4 \center-align {"Dü"}}
TEK=\markup {\fontsize #-2 \halign #-.2 \center-align {"Tek"}}
TEEK=\markup {\fontsize #-2 \halign #-.4 \center-align {"Teek"}}
TE=\markup {\fontsize #-2 \halign #-.4 \center-align {"Te"}}
TA=\markup {\fontsize #-2 \halign #-.4 \center-align {"Ta"}}
KE=\markup {\fontsize #-2 \halign #-.4 \center-align {"Ke"}}
ME=\markup {\fontsize #-2 \halign #-.4 \center-align {"Me"}}
HEK=\markup {\fontsize #-2 \halign #-.4 \center-align {"Hek"}}
KA=\markup {\fontsize #-2 \halign #-.4 \center-align {"Kâ"}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEGIN USUL DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
UsulClef =  {
  \clef percussion
  \autoBeamOff
  %\override TextScript #'Y-extent = #'(-1.5 . 1.5)
}

%%%%%%%%%%% CifteSofyan %%%%%%%%%%%%%
CifteSofyanPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\DUM \stemDown kdh4._\TEK
  }
}

CifteSofyanPatternVelOne = {
  \drummode {
    %\stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh_\KA
    
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME kdh_\TE kdh_\KE }
    kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% nim Sofyan %%%%%%%%%%%%%
NimSofyan = {\time 2/4  }

NimSofyanBarsUsul = {
  \time 2/4 s2 \nibar ""
}

NimSofyanPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

%%%%%%%%%%% Semai %%%%%%%%%%%%%
Semai = {\time 3/4 \SemaiBeams}

SemaiBarsUsul = {
  \time 3/4 s2. \nibar ""
}

SemaiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK
  }
}

%%%%%%%%%%% Sofyan %%%%%%%%%%%%%
Sofyan = {\numericTimeSignature \time 4/4 \SofyanBeams }

SofyanBarsUsul = {
  \time 4/4 s1 \nibar ""
}

SofyanPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \stemDown kdh4_\TE \stemDown kdh4_\KE
  }
}

SofyanPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE
    \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% turksAksagi %%%%%%%%%%%%%
TurkAksagi = {\time 5/8 \TurkAksagiBeams }

TurkAksagiBars = { s2. \bar "|" }

TurkAksagiBarsUsul = {
  \time 5/8 s4 \nibar "!" s4. \nibar ""
}

TurkAksagiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh8_\TEK
  }
}

TurkAksagiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE
    \stemDown kdh8_\TEK kdh_\KA kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% SenginSemai %%%%%%%%%%%%%
SenginSemai = {\time 6/4 \SenginSemaiBeams }

SenginSemaiEight = {\time 6/8 \SenginSemaiEightBeams }

SenginSemaiPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh2_\TEK
  }
}

%%%%%%%%%%% YurukSemai %%%%%%%%%%%%%
YurukSemai = {\time 6/8 \YurukSemaiBeams }

YurukSemaiBarsUsul = {
  \time 6/8 s4. \nibar "!" s4. \nibar ""
}

YurukSemaiPatternOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK kdh_\TEK
    \stemUp kdl8^\DUM \stemDown kdh4_\TEK
  }
}

YurukSemaiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK kdh_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemDown kdh8_\TEK \stemDown kdh8_\KA
  }
}

%%%%%%%%%%% DevriHindi %%%%%%%%%%%%%
DevriHindi = {\time 7/8 \DevriHindiBeams }

DevriHindiBarsUsul = {
  \time 7/8 s4. \nibar "!" s2 \nibar ""
}

DevriHindiPatternOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DUM \stemDown kdh_\TEK
  }
}

DevriHindiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh_\KA
  }
}

DevriHindiPatternVelTwo = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% DevriTuranBars %%%%%%%%%%%%%
DevriTuran = {\time 7/8 \DevriTuranBeams }

DevriTuranBarsUsul = {
  \time 7/8 s2 \nibar "!" s4. \nibar ""
}

DevriTuranPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh4._\TEK
  }
}

DevriTuranPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DU \stemDown kdh_\ME
    \repeat unfold 2 {\stemDown kdh_\TE \stemDown kdh_\KE}
    \stemDown kdh16_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% AgirDuyek %%%%%%%%%%%%%
AgirDuyek = {\time 8/4 }

AgirDuyekBars = { s1 \nibar "!" s1 \nibar "|" }

AgirDuyekPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% Duyek %%%%%%%%%%%%%
Duyek = { \time 8/8 \DuyekBeams }

DuyekBars = {
  s2 \nibar "!" s2 \nibar "|"
}

DuyekBarsUsul = {
  \time 8/8 s2 \nibar "!" s2 \nibar ""
}

DuyekPatternOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

DuyekPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh8_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM \stemDown kdh_\HEK \stemDown kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% Musemmen %%%%%%%%%%%%%
Musemmen = { \time 8/8 \MusemmenBeams }

MusemmenBarsUsul = {

  \time 8/8 s4. \nibar "!" s4 s4. \nibar ""
}

MusemmenPatternOne = {
  \drummode {
    \stemUp kdl4.^\DUM \stemDown kdh4_\TEK \stemDown kdh4._\TEK
  }
}

MusemmenPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \repeat unfold 3 {\stemDown kdh8_\TEK kdh8_\KA}
    \stemDown kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% AgirAksak %%%%%%%%%%%%%
AgirAksak = {\time 9/4 \LongbarBeams }

AgirAksakBars = { s4*9 \bar "|" }

AgirAksakBarsUsul = {
  \time 9/4 s1 \nibar "!" s2 s2 s4 \nibar ""
}

AgirAksakPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KA 
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK
    \stemDown kdh4_\TEK
  }
}

AgirAksakPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh_\KE kdh4_\TEK kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh2_\TEK
    \stemDown kdh8_\TE kdh8_\KE
  }
}

%%%%%%%%%%% Aksak %%%%%%%%%%%%%
Aksak = {\time 9/8 \AksakBeams }

AksakBarsUsul = {
  \time 9/8 s2 \nibar "!" s4 s4. \nibar ""
}

AksakPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh8_\KE
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
  }
}

AksakPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh_\KA
    
    \stemUp kdl16^\DU \stemDown kdh_\ME
    \stemUp kdl8^\DUM
    \stemDown kdh4_\TEK kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% Karsilama %%%%%%%%%%%%%
Karsilama = {\time 9/8 \ZeybekBeams }

KarsilamaPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

%%%%%%%%%%% Zeybek %%%%%%%%%%%%%
Zeybek = {\time 9/8 \ZeybekBeams }

ZeybekPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

AgirZeybek = {\time 9/4 \AgirZeybekBeams }

%%%%%%%%%%% Oynak %%%%%%%%%%%%%
Oynak = {\time 9/8 \OynakBeams }

OynakPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

%%%%%%%%%%% Evfer %%%%%%%%%%%%%
Evfer = {\time 9/8 \EvferBeams }

EvferBarsUsul = {
  \time 9/8 s2 \nibar "!" s4 s4. \nibar ""
}

EvferPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh8_\KE
    \stemUp kdl4^\DUM \stemDown kdh8_\TEK \stemDown kdh4_\TEK
  }
}

EvferPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh_\KA
    
    \stemUp kdl16^\DU \stemDown kdh_\ME
    \stemUp kdl8^\DUM
    \stemDown kdh8_\TEK kdh4_\TEK
  }
}

%%%%%%%%%%% RaksAksagi %%%%%%%%%%%%%
RaksAksagi = {\time 9/8 \RaksAksagiBeams }

RaksAksagiBarsUsul = {
  \time 9/8 s4 s4. \nibar "!" s2 \nibar ""
}

RaksAksagiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4._\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

RaksAksagiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DU \stemDown kdh_\ME kdh_\TE kdh_\KE kdh16_\TE kdh_\KE
    
    \stemUp kdl8^\DU \stemDown kdh_\ME kdh8_\TE kdh_\KE
  }
}

%%%%%%%%%%% Aksak Semai %%%%%%%%%%%%%
AksakSemai = {\time 10/8 \AksakSemaiBeams }

AksakSemaiBarsUsul = {
  \time 10/8 s4 s s s s s \nibar "!" s4 s s s s %\nibar "" \nibar "!" 
}

AksakSemaiPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh4_\KA
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
  }
}

AksakSemaiPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh_\KE kdh4_\TEK kdh_\KA kdh8_\TE kdh_\KE \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown <kdh kdl>2_\HEK kdh8_\TE kdh_\KE
  }
}

%%%%%%%%%%% Curcuna %%%%%%%%%%%%%
Curcuna = { \time 10/8 \CurcunaBeams }

CurcunaBars = {
  s8*10 \nibar "|"
}

CurcunaPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh4_\KA
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
  }
}

CurcunaPatternTwo = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM
    \stemDown kdh2_\TEK \stemDown kdh_\TEK
    \stemDown kdh2_\TEK \stemUp kdl2^\DUM kdl2^\DUM
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

CurcunaSixteen = { \time 10/16 \CurcunaSixteenBeams }

CurcunaSixteenPattern = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdl16^"te" kdh8_\KA
    \stemUp kdl8^\DUM \stemDown kdh8_\TEK \stemDown kdh16_\TEK
  }
}

%%%%%%%%%%% LenkFahte %%%%%%%%%%%%%
LenkFahte = {\time 10/4 \SenginSemaiBeams }

LenkFahteBars = {
  s4*6 \nibar "!"
  s1 \nibar "|"
}

LenkFahteEight = {\time 10/8 }

LenkFahteEightBars = {
  s8*6 \nibar "!"
  s2 \nibar "|"
}

LenkFahteBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "20" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

LenkFahtePatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \repeat unfold 2 {kdl4^\DUM}
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

LenkFahtePatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

LenkFahtePatternVelTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}

  }
}

%%%%%%%%%%% CenberAgir 12/4%%%%%%%%%%%%%
CenberAgir = {\time 12/4 \SenginSemaiBeams}

CenberAgirBars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

CenberAgirBarsBREAKS = {
  s1 \nibar "!"
  s1 \nibar "!"
  s2. \nibar "" \break
  s2. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "|"
}

CenberAgirPattern = {
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"
  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"
  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA
  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% DevriKebirFourteen 14/4 %%%%%%%%%%%%%
DevriKebirFourteen = {
  \time 14/4 %\SenginSemaiBeams
}

DevriKebirFourteenBars = {
  s4 s4 s4 s1  \nibar "!"
  s4 s4 s4 s1 \nibar "|"
}

DevriKebirFourteenBarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

DevriKebirFourteenPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "!"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% AyinDevriRevani %%%%%%%%%%%%%
AyinDevriRevani = {\time 14/8 \AyinDevriRevaniBeams }

AyinDevriRevaniBars = {
  s8*3 \nibar "!"
  s8*4 \nibar "!"
  s8*3 \nibar "!"
  s8*4 \nibar "|"
}

AyinDevriRevaniPatternOne = {
  \drummode {
    \stemUp kdl4.^\DUM
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
    \stemUp kdl4.^\DUM
    \stemDown kdh4_\TEK kdh4_\TEK
  }
}

AyinDevriRevaniPatternTwo = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh_\ME
    \stemDown kdh_\TEK \stemDown kdh_\KA \stemDown kdh_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% MuhammesSixteenTwo %%%%%%%%%%%%%
MuhammesSixteenTwo = {\time 16/2 \SixteenTwoBeams}

MuhammesSixteenTwoBars = {
  \repeat unfold 3 {s4*8 \nibar "!" } s4*8 \nibar "|"
}

MuhammesSixteenTwoPattern = {
  \drummode {
    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "!"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "!" \break
    }
  }
}

%%%%%%%%%%% MuhammesSixteenFour %%%%%%%%%%%%%
MuhammesSixteenFour = {\time 16/4 \SofyanBeams}

MuhammesSixteenFourBars = {
  \repeat unfold 3 {s4*4 \nibar "!" } s1 \nibar "|"
}

MuhammesSixteenFourPattern = {
  \drummode {
    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "!"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "!" \break
    }
  }
}

%%%%%%%%%%% NimBerefsan %%%%%%%%%%%%%
NimBerefsan = {\time 16/8 \NimBerefsanBeams }

NimBerefsanBars = {
  s8*3 \nibar "!"
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

NimBerefsanPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% NimBerefsanFour %%%%%%%%%%%%%
NimBerefsanFour = {\time 16/4 \SofyanBeams }

NimBerefsanFourBars = {
  s4*3 \nibar "!"
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

NimBerefsanFourPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% CifteDuyek %%%%%%%%%%%%%
CifteDuyek = { \time 16/4 \CifteDuyekBeams }

CifteDuyekBars = {
  \repeat unfold 3 {s1 \nibar "!" }
  s1 \nibar "|"
}

CifteDuyekPattern = {
  \drummode {
    \stemUp kdl2^\DUM \stemDown kdh1_\TEK kdh2_\TEK \bar "!"
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

CifteDuyekPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl4^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% Fahte %%%%%%%%%%%%%
Fahte = {\time 20/4 \SenginSemaiBeams }

FahteBars = {
  s1 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar "|"
}

FahteBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "20" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

FahtePatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \repeat unfold 2 {kdl4^\DUM}
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

FahtePatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

FahtePatternVelTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}

  }
}

%%%%%%%%%%% AgirFahte %%%%%%%%%%%%%
AgirFahte = {
  \time 20/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
      (end .                   ;entry for end of Beams
        (                       ;start of alist of end points
          ((1 . 8) . (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))   ;
          )))                     %close all entries
}

AgirFahteBars = {
  s1*2 \nibar "!"
  s1.*2 \nibar "!"
  s1.*2 \nibar "!"
  s1*2 \nibar "|"
}

AgirFahteBarsBREAKS = {
  s1 \nibar "!"
  s2. \nibar ""
  s2. \nibar "!"
  s2. \nibar ""
  s2. \nibar "!"
  s1 \nibar "|"
}

AgirFahteBarsLB = {
  s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "|"
}

AgirFahtePattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% Cenber %%%%%%%%%%%%%
Cenber = {\time 24/4 \SenginSemaiBeams}

CenberBars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "|"
}

CenberBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "24" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

CenberBarsFOURS = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar "|"
}

CenberPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KE
    \stemUp kdl2^\DUM kdl4^\DUM kdl4^\DUM
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

CenberPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown  kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

CenberPatternVelTwo = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \stemDown kdh4_\TEK \stemDown kdh4_\KA \stemUp kdl4^\DU \stemDown kdh_\ME \stemDown
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \repeat unfold 4 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

CenberTwo = {\time 24/2 \SixTwoBeams}

CenberTwoBars = {
  s2*2 \nibar ""  s2*2 \nibar "!"
  s2*2 \nibar ""  s2*2 \nibar "!"
  s2*3 \nibar "" s2*3 \nibar "!"
  s2*3 \nibar "" s2*3 \nibar "!"
  s2*2 \nibar ""  s2*2 \nibar "|"
}

%%%%%%%%%%% NimSakil %%%%%%%%%%%%%
NimSakil = {\time 24/4 \SenginSemaiBeams}

NimSakilBars = {
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

NimSakilPattern = {
  \drummode {
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "!"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "!"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

%%%%%%%%%%% BesteDevriRevan %%%%%%%%%%%%%
BesteDevriRevan = { \time 26/4 \SenginSemaiBeams}

BesteDevriRevanBars = {
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

BesteDevriRevanPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "!"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% Evsat %%%%%%%%%%%%%
Evsat = {\time 26/8 \EvsatBeams }

EvsatBars = {
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

EvsatPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% DevriKebir %%%%%%%%%%%%%
DevriKebir = { \time 28/4 \SenginSemaiBeams}

DevriKebirBars = {
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

DevriKebirBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "28" "6" "4" )

  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar ""
}

DevriKebirBarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

DevriKebirPatternTwo = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \repeat unfold 2 {\stemUp kdl2^\DUM}
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \stemUp kdl4^\TE \stemDown kdh_\KA \stemUp kdl4^\TE \stemDown kdh_\KA
  }
}

DevriKebirPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl1^\DUM
    \stemDown kdh1_\TEK
    \stemDown kdh1_\TEK \stemUp kdl2^\DUM
    \stemDown kdh1_\TEK
    \stemUp kdl4^\TE \stemDown kdh_\KA \stemUp kdl4^\TE \stemDown kdh_\KA
  }
}

DevriKebirPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM %\break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

DevriKebirPatternVelTwo = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \repeat unfold 2 {\stemDown <kdh kdl>2_\HEK } %\break

    \stemDown <kdh kdl>4_\HEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME }
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME }
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

DevriKebirPatternVelThree = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \repeat unfold 2 {\stemDown <kdh kdl>2_\TEK } %\break

    \stemDown <kdh kdl>4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME }
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME }
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

%%%%%%%%%%% MuzaafDevriKebir %%%%%%%%%%%%%
MuzaafDevriKebir = { \time 28/4 \SofyanBeams }

MuzaafDevriKebirBars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

MuzaafDevriKebirBarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

MuzaafDevriKebirBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "56" "6" "4" )

  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "|"

  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar ""
}

MuzaafDevriKebirPatternOne = {
  \drummode {
    \DevriKebirPatternVelOne
    \stemDown <kdl kdh>4_\HEK kdh8_\TE kdh8_\KE kdh4_\TEK kdh4_\KA kdh8_\TE kdh8_\KE kdh4_\TEK
    kdh4_\KA kdh8_\TE kdh8_\KE \stemUp kdl4^\DU \stemDown kdh_\ME
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DU \stemDown kdh_\ME \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 4 {\stemDown kdh4_\TEK \stemDown kdh_\KA }
  }
}

%%%%%%%%%%% Remel %%%%%%%%%%%%%
Remel = {\time 28/2 \SixTwoBeams}

RemelBars = {
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "|"
}

RemelPattern = {
  \drummode {
    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "!"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "!" \break
    }
  }
}

%%%%%%%%%%% RemelFour %%%%%%%%%%%%%
RemelFour = {\time 28/4 \SenginSemaiBeams}

RemelFourBars = {
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

RemelFourPattern = {
  \drummode {
    \repeat unfold 4 {
      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "!"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "!" \break
    }
  }
}

%%%%%%%%%%% Hafif %%%%%%%%%%%%%
Hafif = {\time 32/4 \SofyanBeams }

HafifBars = {
  \repeat unfold 7 {s1 \nibar "!" }
  s1 \nibar "|"
}

HafifBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "32" "4" "4" )
  \repeat unfold 7 {\time 4/4 s1 \nibar "!" } \time 4/4 s1 \nibar ""
}

HafifPatternOne = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemUp kdl4^\TE \stemDown kdh4_\KE
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh2_\TEK
    \stemUp kdl2^\DUM \stemUp kdl4^\TE \stemDown kdh4_\KE
    \repeat unfold 3 {\stemUp kdl4^\DUM \stemDown kdh4_\TEK}
    \repeat unfold 2 {\stemUp kdl4^\DUM}
    \stemDown kdh2_\TEK
    \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

HafifPatternTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemUp kdl4^\TE \stemDown kdh4_\KE
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh2_\TEK
    \stemUp kdl2^\DUM \stemUp kdl4^\TE \stemDown kdh4_\KE
    \repeat unfold 2 {\stemUp kdl4^\DUM} \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM
    \stemDown kdh4_\TA \stemDown <kdh kdl>4_\HEK
    
    \repeat unfold 2 {\stemUp kdl8^\TE \stemDown kdh8_\KE}
  }
}

HafifPatternVelOne = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME} \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA

    \stemUp kdl4^\DU \stemDown kdh_\ME \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    
    
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} 
    \stemDown <kdh kdl>4_\HEK \stemDown kdh8_\TE \stemDown kdh_\KE
    \repeat unfold 4 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

%%%%%%%%%%% Muhammes %%%%%%%%%%%%%
Muhammes = {\time 32/4 \SofyanBeams}

MuhammesBars = {
  \repeat unfold 7 {s1 \nibar "!" } s1 \nibar "|"
}

MuhammesBarsUsul = {
  \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \once \override Staff.TimeSignature #'text =
  #(compound-time-parentheses "32" "4" "4" )
  \repeat unfold 7 {\time 4/4 s1 \nibar "!" } \time 4/4 s1 \nibar ""
}

MuhammesPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KE
    \stemUp kdl2^\DUM \stemDown kdh_\TEK
    \repeat unfold 2 {\stemUp kdl2^\DUM}
    \stemDown kdh_\TEK \stemUp kdl4^\TE \stemDown kdh_\KE
    \stemUp kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\TE \stemDown kdh_\KE \stemUp kdl2^\DUM

    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK

    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

MuhammesPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM kdl4^\DUM \stemDown kdh4_\TEK kdh8_\TE kdh8_\KE
    kdh4_\TEK kdh4_\KA kdh4_\TEK kdh4_\KA
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME
    kdh4_\TEK kdh_\KA kdh_\TEK kdh_\KA
    \repeat unfold 2 {\stemDown <kdh kdl>2_\HEK}
    \stemDown <kdh kdl>4_\HEK kdh8_\TEK kdh_\KE kdh4_\TEK kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh8_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME
    \repeat unfold 2 {kdh8_\TE kdh8_\KE}
    kdh4_\TEK kdh_\KA kdh_\TEK kdh_\KA
  }
}

%%%%%%%%%%% Berefsan %%%%%%%%%%%%%
Berefsan = {\time 32/4 \SenginSemaiBeams }

BerefsanBars = {
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!" \break
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

BerefsanEightBars = {
  s8*6 \nibar "!"
  s8*6 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

BerefsanPattern = {
  \drummode {
    \stemUp kdl1^\DUM \stemDown kdh2_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemDown kdh2_\TEK \bar "!"
    \stemUp kdl1^\DUM \bar "!"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \bar "!"
    \stemUp kdl2^\DUM kdl2^\TEK \bar "!"
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK \bar "!"
    \stemUp kdl4^\TEK \stemDown kdh4_\KA \stemUp kdl4^\TEK \stemDown kdh4_\KA
  }
}

BerefsanPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% Sakil %%%%%%%%%%%%%
Sakil = {\time 48/4 \SenginSemaiBeams}

SakilBars = {
  % 4 6 4 6 6 6 4 4 4 4
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

SakilPattern = {
  \drummode {
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "!"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "!"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

%%%%%%%%%%% AgirSakil %%%%%%%%%%%%%
AgirSakil = {\time 48/2 \LongbarBeams}

AgirSakilBars = {
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*12 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "||"
}

AgirSakilPattern = {
  \drummode {
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "!"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "!"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "!"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "!"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

%%%%%%%%%%% AgirCenber 48/4 %%%%%%%%%%%%%
%AgirCenber = {\time 48/4 \SenginSemaiBeams}
AgirCenber = {\time 24/2 \LongbarBeams}

AgirCenberBars = {
  s1 \nibar "" s1 \nibar "!"
  s1 \nibar "" s1 \nibar "!"
  s1. \nibar "" s1. \nibar "!"
  s1. \nibar "" s1. \nibar "!"
  s1 \nibar "" s1 \nibar "!"
}

AgirCenberPattern = {
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"
  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"
  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA
  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% Havi %%%%%%%%%%%%%
Havi = {\time 64/4 \SenginSemaiBeams }

HaviBars = {
  \repeat unfold 2 {s1 \nibar "!" }
  \repeat unfold 2 {s1. \nibar "!"}
  \repeat unfold 3 { s1 \nibar "!" s1 \nibar "!" s1 \nibar "!" }
  s1 \nibar "!" s1 \nibar "||"
}

HaviPattern = {
  \stemUp d2^\DUM  \stemUp d4^\TE \stemDown b_\KE  \bar "|"
  \stemUp d2^\DUM \stemUp d4^\TE \stemDown b_\KE \bar "|"
  \stemUp d2^\DUM \stemUp d4^\TE \stemDown b_\KE \stemUp d^\TE \stemDown b_\KE \bar "|"
  \stemUp d2^\DUM \stemUp d4^\TE \stemDown b_\KE \stemUp d^\TE \stemDown b_\KE \bar "|"
  \stemUp d2^\DUM \stemDown b_\TEK \bar "|"
  \stemUp d^\DUM \stemUp d^\DUM \bar "|"
  \stemUp d4^\TE \stemDown b_\KE \stemUp d^\TE \stemDown b_\KE \bar "|"

  \stemUp d2^\DUM \stemUp d4^\TE \stemDown b_\KE \bar "|"
  \stemUp d^\DUM \stemUp d^\DUM \stemDown b2 \bar "|"
  \stemUp d4^\TE \stemDown b_\KE \stemUp d2^\DUM \bar "|"
  \stemDown b_\TEK \stemUp d4^\TE \stemDown b_\KE \bar "|"
  \stemUp d^\DUM \stemDown b_\TEK \stemDown b2_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b_\TEK \stemDown b2_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b_\TEK \stemUp d^\DUM \stemUp d^\DUM \bar "|"
  \stemDown b2_\TEK \stemUp d4^\TE \stemDown b_\KE \bar "|"
}

%%%%%%%%%%% DarbiFetih %%%%%%%%%%%%%
DarbiFetih = {\time 88/4 \SenginSemaiBeams }

DarbiFetihBars = {
s1 \nibar "!"
s1. \nibar "!"
\repeat unfold 4 {s1 \nibar "!" }
\repeat unfold 2 {s1. \nibar "!"}
\repeat unfold 2 {s1 \nibar "!" }
s1. \nibar "!"
\repeat unfold 8 {s1 \nibar "!" }
s1 \nibar "||"
}

%%%%%%%%%%% Darbeyn %%%%%%%%%%%%%
Darbeyn = { \SixTwoBeams }

DarbeynBars = {
  \time 28/2
  \SixTwoBeams
  \RemelBars
  \RemelBars
  \time 32/4 \MuhammesBars
}

%%%%%%%%%%% Zencir %%%%%%%%%%%%%
Zencir = {
  \time 120/4 \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
}

ZencirPatternVel = {
  \drummode {
    \CifteDuyekPatternVel
    \FahtePatternVelOne
    \CenberPatternVelOne
    \DevriKebirPatternVelOne
    \BerefsanPatternVel
  }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RHYTHMIC SLASHES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Macro to print single slash
rs = {
  \once \override Rest #'stencil = #ly:percent-repeat-item-interface::beat-slash
  \once \override Rest #'thickness = #0.48
  \once \override Rest #'slope = #1.7
  r4
}

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
