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
                     (twoup . ,(ly:make-pitch 0 2 NATURAL))
                     (threeup . ,(ly:make-pitch 0 3 NATURAL))
                     (fourup . ,(ly:make-pitch 0 4 NATURAL))
                     (fiveup . ,(ly:make-pitch 0 5 NATURAL))
                     (onedown . ,(ly:make-pitch -1 6 NATURAL))
                     (twodown . ,(ly:make-pitch -1 5 NATURAL))
                     (threedown . ,(ly:make-pitch -1 4 NATURAL))
                     (fourdown . ,(ly:make-pitch -1 3 NATURAL))
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

  % This defines a staff with only two lines.
  % It also defines the positions of the two lines.
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
  \override Score.RehearsalMark #'extra-spacing-height = #'(3 . 3)
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

#(define (compound-time-parentheses one two num)
   (markup #:override '(baseline-skip . 0) #:number
     (#:line
      (

       #:concat (#:vcenter parenthL #:hspace 0.2 (#:left-align (#:center-column (one num))) #:hspace 0.2 #:vcenter parenthR)

       #:left-align (#:center-column (two num))


       ))))


%% BEAMING
Aksakbeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

evferbeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

raksaksagibeams = {
  % BEAMING FOR 9/8
  \set Timing.beatStructure = #'(2 2 2 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

AksakSemaibeams = {
  % BEAMING FOR 10/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1)
}

ayindevrirevanibeams = {
  % BEAMING FOR 14/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1 2 2)
}

curcunabeams = {
  % BEAMING FOR 10/8
  \set Timing.beatStructure = #'(2 1 2 2 2 1)
}

curcunasixteenbeams = {
  % BEAMING FOR 10/16 CURCUNA
  \set Timing.beatStructure = #'(3 2 2 3)
}

devrihindibeams = {
  % BEAMING FOR 7/8 DEVRI HINDI
  \set Timing.beatStructure = #'(2 1 2 2)
}

devrituranbeams = {
  % BEAMING FOR 7/8 DEVRI TURAN
  \set Timing.beatStructure = #'(2 2 2 1)
}

duyekbeams = {
  % BEAMING FOR 8/8
  \set Timing.beatStructure = #'(2 2 2 2)
}

evsatbeams = {
  % BEAMING FOR EVSAT USUL...the 5/8 needs this
  \set Timing.beatStructure = #'(2 2 1)
}

longbarbeams = {
  % BEAMING FOR NIMBEREFSAN USUL
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2 2 2 2 2 2 2 2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

musemmenbeams = {
  % BEAMING FOR 8/8
  \set Timing.beatStructure = #'(3 2 3)
}


nimberefsanbeams = {
  % BEAMING FOR NIMBEREFSAN USUL
  \set Timing.beatStructure = #'(2 2)
}

semaibeams = {
  % BEAMING FOR 3/4
  \set Timing.beatStructure = #'(1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

SixTwobeams = {
  % BEAMING FOR 6/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2 2 2 2 2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

SenginSemaibeams = {
  % BEAMING FOR 6/4
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  %{ \set Timing.beamExceptions =
     #'(;start of alist
     (end . ;entry for end of beams
     ( ;start of alist of end points
     ((1 . 16) . (4 4 4 4 4 4)) ;rule for 1/32 beams -- end each 1/16
     ))) %close all entries %}
}

SenginSemaiEightbeams = {
  % BEAMING FOR sengin semai 6/8
  \set Timing.beatStructure = #'(2 1 1 2)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (2 2 2 2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

sofyanbeams = {
  % BEAMING FOR 4/4
  \set Timing.beatStructure = #'(1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

SixteenTwobeams = {
  % BEAMING FOR 16/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2 2 2 2 2 2 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

cifteduyekbeams = {
  % BEAMING FOR 16/4
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
}

turkaksagibeams = {
  % BEAMING FOR 5/8
  \set Timing.beatStructure = #'(2 2 1)
}

YurukSemaibeams = {
  % BEAMING FOR 6/8
  \set Timing.beatStructure = #'(3 3)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 2 4 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

zeybekbeams = {
  % BEAMING FOR 9/8 ZEYBEK
  \set Timing.beatStructure = #'(2 2 2 2 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
}

agirzeybekbeams = {
  % BEAMING FOR 9/4 ZEYBEK
  \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1)
  %{\set Timing.beamExceptions =
    #'(                         ;start of alist
    (end .                   ;entry for end of beams
    (                       ;start of alist of end points
    ((1 . 16) . (4 4 4 4 2))   ;rule for 1/32 beams -- end each 1/16
    )))        %}             %close all entries
}

oynakbeams = {
  % BEAMING FOR 9/8 ZEYBEK
  \set Timing.beatStructure = #'(2 1 2 2 2 )
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 16) . (4 2 4 4 4 ))   ;rule for 1/32 beams -- end each 1/16
                                                        )))                     %close all entries
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

%DUM="DÜM"
%DU="DÜ"
%TEK="TEK"
%TEEK="TEEK"
%TE="TE"
%TA="TA"
%KE="KE"
%ME="ME"
%HEK="HEK"
%KA="KÂ"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEGIN USUL DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
UsulClef =  {
  \clef percussion
  \autoBeamOff
  %\override TextScript #'Y-extent = #'(-1.5 . 1.5)
}

%%%%%%%%%%% agirAksak %%%%%%%%%%%%%
agirAksak = {\time 9/4 \longbarbeams }

agirAksakbars = { s4*9 \bar "|" }

agirAksakbarsUsul = {
   \time 9/4 s1 \nibar "!" s2 s2 s4 \nibar ""
}

agirAksakPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KA 
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK
    \stemDown kdh4_\TEK
  }
}

agirAksakPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh_\KE kdh4_\TEK kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh2_\TEK
    \stemDown kdh8_\TE kdh8_\KE
  }
}

%%%%%%%%%%% agirduyek %%%%%%%%%%%%%
agirduyek = {\time 8/4 }

agirduyekbars = { s1 \nibar "!" s1 \nibar "|" }

agirduyekPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% agirsakil %%%%%%%%%%%%%
agirsakil = {\time 48/2 \longbarbeams}

agirsakilbars = {
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*12 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "|"
}

agirsakilPattern = {
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

%%%%%%%%%%% agirsakil %%%%%%%%%%%%%
darbifetih = {\time 88/4 \longbarbeams}

darbifetihbars = {
  s4*4 \nibar "!"
  s4*6 \nibar "!"
  \repeat unfold 4 {s4*4 \nibar "!"}
  \repeat unfold 2 {s4*6 \nibar "!"}
  \repeat unfold 2 {s4*4 \nibar "!"}
  s4*6 \nibar "!"
  s4*4 \nibar "!"

  \repeat unfold 7 {s4*4 \nibar "!"}

  s4*4 \nibar "|"
}

darbifetihPattern = {
}

%%%%%%%%%%% Aksak %%%%%%%%%%%%%
Aksak = {\time 9/8 \Aksakbeams }

AksakbarsUsul = {
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

%%%%%%%%%%% evfer %%%%%%%%%%%%%
evfer = {\time 9/8 \evferbeams }

evferbarsUsul = {
   \time 9/8 s2 \nibar "!" s4 s4. \nibar ""
}

evferPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh8_\KE
    \stemUp kdl4^\DUM \stemDown kdh8_\TEK \stemDown kdh4_\TEK
  }
}

evferPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh_\KA
    
    \stemUp kdl16^\DU \stemDown kdh_\ME
    \stemUp kdl8^\DUM
    \stemDown kdh8_\TEK kdh4_\TEK
  }
}

%%%%%%%%%%% raksaksagi %%%%%%%%%%%%%
raksaksagi = {\time 9/8 \raksaksagibeams }

raksaksagibarsUsul = {
   \time 9/8 s4 s4. \nibar "!" s2 \nibar ""
}

raksaksagiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4._\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

raksaksagiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DU \stemDown kdh_\ME kdh_\TE kdh_\KE kdh16_\TE kdh_\KE
    
    \stemUp kdl8^\DU \stemDown kdh_\ME kdh8_\TE kdh_\KE
  }
}

%%%%%%%%%%% Aksak semai %%%%%%%%%%%%%
AksakSemai = {\time 10/8 \AksakSemaibeams }

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

%%%%%%%%%%% ayindevrirevani %%%%%%%%%%%%%
ayindevrirevani = {\time 14/8 \ayindevrirevanibeams }

ayindevrirevanibars = {
  s8*3 \nibar "!"
  s8*4 \nibar "!"
  s8*3 \nibar "!"
  s8*4 \nibar "|"
}

ayindevrirevaniPatternOne = {
  \drummode {
    \stemUp kdl4.^\DUM
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
    \stemUp kdl4.^\DUM
    \stemDown kdh4_\TEK kdh4_\TEK
  }
}

ayindevrirevaniPatternTwo = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh_\ME
    \stemDown kdh_\TEK \stemDown kdh_\KA \stemDown kdh_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% berefsan %%%%%%%%%%%%%
berefsan = {\time 32/4 \SenginSemaibeams }

berefsanbars = {
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!" \break
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

berefsaneightbars = {
  s8*6 \nibar "!"
  s8*6 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

XberefsanPattern = {
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

berefsanPattern = {
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

berefsanPatternVel = {
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

%%%%%%%%%%% nimberefsan %%%%%%%%%%%%%
nimberefsan = {\time 16/8 \nimberefsanbeams }

nimberefsanbars = {
  s8*3 \nibar "!"
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

nimberefsanPattern = {
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

%%%%%%%%%%% nimberefsanfour %%%%%%%%%%%%%
nimberefsanfour = {\time 16/4 \sofyanbeams }

nimberefsanfourbars = {
  s4*3 \nibar "!"
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

nimberefsanfourPattern = {
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

%%%%%%%%%%% cenber %%%%%%%%%%%%%
cenber = {\time 24/4 \SenginSemaibeams}

cenberbars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "|"
}

cenberbarsUsul = {
    \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
    \once \override Staff.TimeSignature #'text =
        #(compound-time-parentheses "24" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

cenberbarsFOURS = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar "|"
}

XcenberPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "!"
    \stemDown  kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

cenberPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KE
    \stemUp kdl2^\DUM kdl4^\DUM kdl4^\DUM
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

cenberPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown  kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

cenberPatternVelTwo = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \stemDown kdh4_\TEK \stemDown kdh4_\KA \stemUp kdl4^\DU \stemDown kdh_\ME \stemDown
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE}
    \repeat unfold 4 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

cenbertwo = {\time 24/2 \SixTwobeams}

cenbertwobars = {
  s2*2 \nibar ""  s2*2 \nibar "!"
  s2*2 \nibar ""  s2*2 \nibar "!"
  s2*3 \nibar "" s2*3 \nibar "!"
  s2*3 \nibar "" s2*3 \nibar "!"
  s2*2 \nibar ""  s2*2 \nibar "|"
}

%%%%%%%%%%% agircenber 48/4 %%%%%%%%%%%%%
%agircenber = {\time 48/4 \SenginSemaibeams}
agircenber = {\time 24/2 \longbarbeams}

agircenberbars = {
  s1 \nibar "" s1 \nibar "!"
  s1 \nibar "" s1 \nibar "!"
  s1. \nibar "" s1. \nibar "!"
  s1. \nibar "" s1. \nibar "!"
  s1 \nibar "" s1 \nibar "!"
}

agircenberPattern = {
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"

  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA

  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% cenberagir 12/4%%%%%%%%%%%%%
cenberagir = {\time 12/4 \SenginSemaibeams}

cenberagirbars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

cenberagirbarsBREAKS = {
  s1 \nibar "!"
  s1 \nibar "!"
  s2. \nibar "" \break
  s2. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "|"
}

cenberagirPattern = {
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"

  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA

  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% cifteduyek %%%%%%%%%%%%%
cifteduyek = { \time 16/4 \cifteduyekbeams }

cifteduyekbars = {
  \repeat unfold 3 {s1 \nibar "!" }
  s1 \nibar "|"
}

XcifteduyekPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl4^\DU \stemDown kdh_\ME \bar "!" \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE kdh4_\TEK \stemDown kdh_\KA
  }
}

cifteduyekPattern = {
  \drummode {
    \stemUp kdl2^\DUM \stemDown kdh1_\TEK kdh2_\TEK \bar "!"
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

cifteduyekPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl4^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% curcuna %%%%%%%%%%%%%
curcuna = { \time 10/8 \curcunabeams }

curcunabars = {
  s8*10 \nibar "|"
}

curcunaPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh4_\KA
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
  }
}

curcunaPatternTwo = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM
    \stemDown kdh2_\TEK \stemDown kdh_\TEK
    \stemDown kdh2_\TEK \stemUp kdl2^\DUM kdl2^\DUM
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

curcunasixteen = { \time 10/16 \curcunasixteenbeams }

curcunasixteenPattern = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdl16^"te" kdh8_\KA
    \stemUp kdl8^\DUM \stemDown kdh8_\TEK \stemDown kdh16_\TEK
  }
}

%%%%%%%%%%% devrikebir %%%%%%%%%%%%%
devrikebir = { \time 28/4 \SenginSemaibeams}

devrikebirbars = {
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

devrikebirbarsUsul = {
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
        
devrikebirbarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

devrikebirPatternTwo = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \repeat unfold 2 {\stemUp kdl2^\DUM}
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \stemUp kdl4^\TE \stemDown kdh_\KA \stemUp kdl4^\TE \stemDown kdh_\KA
  }
}

devrikebirPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK
    \stemUp kdl1^\DUM
    \stemDown kdh1_\TEK
    \stemDown kdh1_\TEK \stemUp kdl2^\DUM
    \stemDown kdh1_\TEK
    \stemUp kdl4^\TE \stemDown kdh_\KA \stemUp kdl4^\TE \stemDown kdh_\KA
  }
}

devrikebirPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM %\break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

devrikebirPatternVelTwo = {
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

devrikebirPatternVelThree = {
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

%%%%%%%%%%% devrikebirfourteen 14/4 %%%%%%%%%%%%%
devrikebirfourteen = {
  \time 14/4 %\SenginSemaibeams
}

devrikebirfourteenbars = {
  s4 s4 s4 s1  \nibar "!"
  s4 s4 s4 s1 \nibar "|"
}

devrikebirfourteenbarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

devrikebirfourteenPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "!"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% muzaafdevrikebir %%%%%%%%%%%%%
muzaafdevrikebir = { \time 28/4 \sofyanbeams }

muzaafdevrikebirbars = {
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

muzaafdevrikebirbarsNB = {
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "!"
  s2. \nibar "" s2. \nibar "!"
  s2 \nibar "" s2 \nibar "!" s2 \nibar "" s2 \nibar "|"
}

muzaafdevrikebirbarsUsul = {
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

muzaafdevrikebirPatternOne = {
  \drummode {
    \devrikebirPatternVelOne

    \stemDown <kdl kdh>4_\HEK kdh8_\TE kdh8_\KE kdh4_\TEK kdh4_\KA kdh8_\TE kdh8_\KE kdh4_\TEK
    kdh4_\KA kdh8_\TE kdh8_\KE \stemUp kdl4^\DU \stemDown kdh_\ME
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DU \stemDown kdh_\ME \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 4 {\stemDown kdh4_\TEK \stemDown kdh_\KA }
  }
}

%%%%%%%%%%% devrihindi %%%%%%%%%%%%%
devrihindi = {\time 7/8 \devrihindibeams }

devrihindibarsUsul = {
  \time 7/8 s4. \nibar "!" s2 \nibar ""
}

devrihindiPatternOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DUM \stemDown kdh_\TEK
  }
}

devrihindiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh_\KA
  }
}

devrihindiPatternVelTwo = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% bestedevrirevan %%%%%%%%%%%%%
bestedevrirevan = { \time 26/4 \SenginSemaibeams}

bestedevrirevanbars = {
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "!"
  s4*5 \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

bestedevrirevanPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "!"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% devrituranbars %%%%%%%%%%%%%
devrituran = {\time 7/8 \devrituranbeams }

devrituranbarsUsul = {
  \time 7/8 s2 \nibar "!" s4. \nibar ""
}

devrituranPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh4._\TEK
  }
}

devrituranPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DU \stemDown kdh_\ME
    \repeat unfold 2 {\stemDown kdh_\TE \stemDown kdh_\KE}
    \stemDown kdh16_\TE \stemDown kdh_\KE
  }
}

%%%%%%%%%%% duyek %%%%%%%%%%%%%
duyek = { \time 8/8 \duyekbeams }

duyekbars = {
  s2 \nibar "!" s2 \nibar "|"
}

duyekbarsUsul = {
  \time 8/8 s2 \nibar "!" s2 \nibar ""
}

duyekPatternOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

duyekPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE kdh8_\TEK kdh8_\KA
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM \stemDown kdh_\HEK \stemDown kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% musemmen %%%%%%%%%%%%%
musemmen = { \time 8/8 \musemmenbeams }

musemmenbarsUsul = {

   \time 8/8 s4. \nibar "!" s4 s4. \nibar ""
}

musemmenPatternOne = {
  \drummode {
    \stemUp kdl4.^\DUM \stemDown kdh4_\TEK \stemDown kdh4._\TEK
  }
}

musemmenPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \repeat unfold 3 {\stemDown kdh8_\TEK kdh8_\KA}
    \stemDown kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% evsat %%%%%%%%%%%%%
evsat = {\time 26/8 \evsatbeams }

evsatbars = {
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "!"
  s8*5 \nibar "!"
  s2 \nibar "!"
  s2 \nibar "|"
}

evsatPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% lenkfahte %%%%%%%%%%%%%
lenkfahte = {\time 10/4 \SenginSemaibeams }

lenkfahtebars = {
  s4*6 \nibar "!"
  s1 \nibar "|"
}

lenkfahteeight = {\time 10/8 }

lenkfahteeightbars = {
  s8*6 \nibar "!"
  s2 \nibar "|"
}

lenkfahtebarsUsul = {
    \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
    \once \override Staff.TimeSignature #'text =
        #(compound-time-parentheses "20" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

lenkfahtePatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \repeat unfold 2 {kdl4^\DUM}
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

lenkfahtePatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

lenkfahtePatternVelTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}

  }
}

%%%%%%%%%%% fahte %%%%%%%%%%%%%
fahte = {\time 20/4 \SenginSemaibeams }

xfahtebars = {
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "|"
}

fahtebars = {
  s1 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar ""
  s2 \nibar "!"
  s1 \nibar "|"
}

xfahtebars = {
  s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "|"
}

fahtebarsUsul = {
    \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
    \once \override Staff.TimeSignature #'text =
        #(compound-time-parentheses "20" "4" "4" )

  \time 4/4 s1 \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 6/4 s1. \nibar "!"
  \time 4/4 s1 \nibar ""
}

fahtePatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \repeat unfold 2 {kdl4^\DUM}
    \repeat unfold 3 {\stemDown kdh2_\TEK}
    \stemUp kdl2^\DUM \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK
    \repeat unfold 2 {\stemUp kdl4^\TE \stemDown kdh_\KE}
  }
}

fahtePatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
  }
}

fahtePatternVelTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \stemDown kdh4_\TEK \stemDown kdh_\KA
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA}

  }
}

%%%%%%%%%%% agirfahte %%%%%%%%%%%%%
agirfahte = {
  \time 20/2
  \set Timing.beatStructure = #'(1 1 1 1 1 1)
  \set Timing.beamExceptions =
  #'(                         ;start of alist
                              (end .                   ;entry for end of beams
                                (                       ;start of alist of end points
                                                        ((1 . 8) . (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))   ;
                                                        )))                     %close all entries
}

agirfahtebars = {
  s1*2 \nibar "!"
  s1.*2 \nibar "!"
  s1.*2 \nibar "!"
  s1*2 \nibar "|"
}

agirfahtebarsBREAKS = {
  s1 \nibar "!"
  s2. \nibar ""
  s2. \nibar "!"
  s2. \nibar ""
  s2. \nibar "!"
  s1 \nibar "|"
}

agirfahtebarsLB = {
  s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "" s2 \nibar "!"
  s2 \nibar "" s2 \nibar "|"
}

agirfahtePattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "!" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "!"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% hafif %%%%%%%%%%%%%
hafif = {\time 32/4 \sofyanbeams }

hafifbars = {
  \repeat unfold 7 {s1 \nibar "!" }
  s1 \nibar "|"
}

hafifbarsUsul = {
    \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
    \once \override Staff.TimeSignature #'text =
        #(compound-time-parentheses "32" "4" "4" )
  \repeat unfold 7 {\time 4/4 s1 \nibar "!" } \time 4/4 s1 \nibar ""
}

hafifPatternOne = {
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

hafifPatternTwo = {
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

hafifPatternVelOne = {
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

%%%%%%%%%%% havi %%%%%%%%%%%%%
havi = {\time 64/4 \SenginSemaibeams }

havibars = {
  \repeat unfold 2 {s1 \nibar "!" }
  \repeat unfold 2 {s1. \nibar "!"}
  \repeat unfold 3 { s1 \nibar "!" s1 \nibar "!" s1 \nibar "!" }
  s1 \nibar "!" s1 \nibar "|"
}

haviPattern = {
  \stemUp d2^\DUM  \stemUp d4^\TE  \stemDown b_\KE  \bar "|"
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

%%%%%%%%%%% karsilama %%%%%%%%%%%%%
karsilama = {\time 9/8 \zeybekbeams }

karsilamaPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}


%%%%%%%%%%% oynak %%%%%%%%%%%%%
oynak = {\time 9/8 \oynakbeams }

oynakPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

%%%%%%%%%%% muhammes %%%%%%%%%%%%%
muhammes = {\time 32/4 \sofyanbeams}

muhammesbars = {
  \repeat unfold 7 {s1 \nibar "!" } s1 \nibar "|"
}

muhammesbarsUsul = {
    \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
    \once \override Staff.TimeSignature #'text =
        #(compound-time-parentheses "32" "4" "4" )
  \repeat unfold 7 {\time 4/4 s1 \nibar "!" } \time 4/4 s1 \nibar ""
}

muhammesPatternOne = {
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

muhammesPatternVelOne = {
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

%%%%%%%%%%% muhammessixteentwo %%%%%%%%%%%%%
muhammessixteentwo = {\time 16/2 \SixteenTwobeams}

muhammessixteentwobars = {
  \repeat unfold 3 {s4*8 \nibar "!" } s4*8 \nibar "|"
}

muhammessixteentwoPattern = {
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

%%%%%%%%%%% muhammessixteenfour %%%%%%%%%%%%%
muhammessixteenfour = {\time 16/4 \sofyanbeams}

muhammessixteenfourbars = {
  \repeat unfold 3 {s4*4 \nibar "!" } s1 \nibar "|"
}

muhammessixteenfourPattern = {
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

%%%%%%%%%%% nimsakil %%%%%%%%%%%%%
nimsakil = {\time 24/4 \SenginSemaibeams}

nimsakilbars = {
  s1 \nibar "!"
  s1. \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

nimsakilPattern = {
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

%%%%%%%%%%% remel %%%%%%%%%%%%%
remel = {\time 28/2 \SixTwobeams}

remelbars = {
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*12 \nibar "!"
  s4*8 \nibar "!"
  s4*8 \nibar "|"
}

remelPattern = {
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

%%%%%%%%%%% remelfour %%%%%%%%%%%%%
remelfour = {\time 28/4 \SenginSemaibeams}

remelfourbars = {
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1. \nibar "!"
  s1 \nibar "!"
  s1 \nibar "|"
}

remelfourPattern = {
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

%%%%%%%%%%% sakil %%%%%%%%%%%%%
sakil = {\time 48/4 \SenginSemaibeams}

sakilbars = {
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

sakilPattern = {
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

%%%%%%%%%%% semai %%%%%%%%%%%%%
semai = {\time 3/4 \semaibeams}

semaibarsUsul = {
  \time 3/4 s2. \nibar ""
}

semaiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK
  }
}

%%%%%%%%%%% SenginSemai %%%%%%%%%%%%%
SenginSemai = {\time 6/4 \SenginSemaibeams }

SenginSemaiEight = {\time 6/8 \SenginSemaiEightbeams }

SenginSemaiPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK \bar "!"
    \stemUp kdl4^\DUM \stemDown kdh2_\TEK
  }
}

%%%%%%%%%%% nim sofyan %%%%%%%%%%%%%
nimsofyan = {\time 2/4  }

nimsofyanbarsUsul = {
  \time 2/4 s2 \nibar ""
}

nimsofyanPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

%%%%%%%%%%% sofyan %%%%%%%%%%%%%
sofyan = {\numericTimeSignature \time 4/4 \sofyanbeams }

sofyanbarsUsul = {
  \time 4/4 s1 \nibar ""
}

sofyanPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM \stemDown kdh4_\TE \stemDown kdh4_\KE
  }
}

sofyanPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE
    \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% turksaksagi %%%%%%%%%%%%%
turkaksagi = {\time 5/8 \turkaksagibeams }

turkaksagibars = { s2. \bar "|" }

turkaksagibarsUsul = {
  \time 5/8 s4 \nibar "!" s4. \nibar ""
}

turkaksagiPatternOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh8_\TEK
  }
}

turkaksagiPatternVelOne = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh16_\TE kdh_\KE
    \stemDown kdh8_\TEK kdh_\KA kdh16_\TE kdh_\KE
  }
}

%%%%%%%%%%% YurukSemai %%%%%%%%%%%%%
YurukSemai = {\time 6/8 \YurukSemaibeams }

YurukSemaibarsUsul = {
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

%%%%%%%%%%% zencir %%%%%%%%%%%%%
zencir = {
  \time 120/4 \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
}

zencirPatternVel = {
  \drummode {
    \cifteduyekPatternVel
    \fahtePatternVelOne
    \cenberPatternVelOne
    \devrikebirPatternVelOne
    \berefsanPatternVel
  }
}

%%%%%%%%%%% zeybek %%%%%%%%%%%%%
zeybek = {\time 9/8 \zeybekbeams }

zeybekPattern = {
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

agirzeybek = {\time 9/4 \agirzeybekbeams }

%%%%%%%%%%% darbeyn %%%%%%%%%%%%%
darbeyn = { \SixTwobeams }

darbeynbars = {
  \time 28/2
  \SixTwobeams
  \remelbars
  \remelbars
  \time 32/4 \muhammesbars
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
