%{
  Notation common for Turkish Classical / Ottoman Usul notation.

  by Adam Good, 2012
%}

\version "2.14.2"

drumPitchNames =
#'((acousticbassdrum . acousticbassdrum)
   (bassdrum . bassdrum)
   (hisidestick . hisidestick)
   (sidestick . sidestick)
   (losidestick . losidestick)
   (acousticsnare . acousticsnare)
   (snare . snare)
   (handclap . handclap)
   (electricsnare . electricsnare)
   (lowfloortom . lowfloortom)
   (closedhihat . closedhihat)
   (hihat . hihat)
   (highfloortom . highfloortom)
   (pedalhihat . pedalhihat)
   (lowtom . lowtom)
   (openhihat . openhihat)
   (halfopenhihat . halfopenhihat)
   (lowmidtom . lowmidtom)
   (himidtom . himidtom)
   (crashcymbala . crashcymbala)
   (crashcymbal . crashcymbal)
   (hightom . hightom)
   (ridecymbala . ridecymbala)
   (ridecymbal . ridecymbal)
   (chinesecymbal . chinesecymbal)
   (ridebell . ridebell)
   (tambourine . tambourine)
   (splashcymbal . splashcymbal)
   (cowbell . cowbell)
   (crashcymbalb . crashcymbalb)
   (vibraslap . vibraslap)
   (ridecymbalb . ridecymbalb)
   (mutehibongo . mutehibongo)
   (hibongo . hibongo)
   (openhibongo . openhibongo)
   (mutelobongo . mutelobongo)
   (lobongo . lobongo)
   (openlobongo . openlobongo)
   (mutehiconga . mutehiconga)
   (muteloconga . muteloconga)
   (openhiconga . openhiconga)
   (hiconga . hiconga)
   (openloconga . openloconga)
   (loconga . loconga)
   (hitimbale . hitimbale)
   (lotimbale . lotimbale)
   (hiagogo . hiagogo)
   (loagogo . loagogo)
   (cabasa . cabasa)
   (maracas . maracas)
   (shortwhistle . shortwhistle)
   (longwhistle . longwhistle)
   (shortguiro . shortguiro)
   (longguiro . longguiro)
   (guiro . guiro)
   (claves . claves)
   (hiwoodblock . hiwoodblock)
   (lowoodblock . lowoodblock)
   (mutecuica . mutecuica)
   (opencuica . opencuica)
   (mutetriangle . mutetriangle)
   (triangle . triangle)
   (opentriangle . opentriangle)
   (oneup . oneup)
   (twoup . twoup)
   (threeup . threeup)
   (fourup . fourup)
   (fiveup . fiveup)
   (onedown . onedown)
   (twodown . twodown)
   (threedown . threedown)
   (fourdown . fourdown)
   (fivedown . fivedown)
   (bda . acousticbassdrum)
   (bd . bassdrum)
   (ssh . hisidestick)
   (ss . sidestick)
   (ssl . losidestick)
   (sna . acousticsnare)
   (sn . snare)
   (hc . handclap)
   (sne . electricsnare)
   (tomfl . lowfloortom)
   (hhc . closedhihat)
   (hh . hihat)
   (tomfh . highfloortom)
   (hhp . pedalhihat)
   (toml . lowtom)
   (hho . openhihat)
   (hhho . halfopenhihat)
   (tomml . lowmidtom)
   (tommh . himidtom)
   (cymca . crashcymbala)
   (cymc . crashcymbal)
   (tomh . hightom)
   (cymra . ridecymbala)
   (cymr . ridecymbal)
   (cymch . chinesecymbal)
   (rb . ridebell)
   (tamb . tambourine)
   (cyms . splashcymbal)
   (cb . cowbell)
   (cymcb . crashcymbalb)
   (vibs . vibraslap)
   (cymrb . ridecymbalb)
   (bohm . mutehibongo)
   (boh . hibongo)
   (boho . openhibongo)
   (bolm . mutelobongo)
   (bol . lobongo)
   (bolo . openlobongo)
   (cghm . mutehiconga)
   (cglm . muteloconga)
   (cgho . openhiconga)
   (kdh . hiconga)
   (cglo . openloconga)
   (kdl . loconga)
   (timh . hitimbale)
   (timl . lotimbale)
   (agh . hiagogo)
   (agl . loagogo)
   (cab . cabasa)
   (mar . maracas)
   (whs . shortwhistle)
   (whl . longwhistle)
   (guis . shortguiro)
   (guil . longguiro)
   (gui . guiro)
   (cl . claves)
   (wbh . hiwoodblock)
   (wbl . lowoodblock)
   (cuim . mutecuica)
   (cuio . opencuica)
   (trim . mutetriangle)
   (tri . triangle)
   (trio . opentriangle)
   (tt . tamtam)
   (ua . oneup)
   (ub . twoup)
   (uc . threeup)
   (ud . fourup)
   (ue . fiveup)
   (da . onedown)
   (db . twodown)
   (dc . threedown)
   (dd . fourdown)
   (de . fivedown)
   )

midiDrumPitches = #`(
                     (acousticbassdrum . ,(ly:make-pitch -3 6 NATURAL))
                     (bassdrum . ,(ly:make-pitch -2 0 NATURAL))
                     (hisidestick . ,(ly:make-pitch -3 6 DOUBLE-SHARP))
                     (sidestick . ,(ly:make-pitch -2 0 SHARP))
                     (losidestick . ,(ly:make-pitch -2 1 FLAT))
                     (acousticsnare . ,(ly:make-pitch -2 1 NATURAL))
                     (snare . ,(ly:make-pitch -2 2 DOUBLE-FLAT))
                     (handclap . ,(ly:make-pitch -2 1 SHARP))
                     (electricsnare . ,(ly:make-pitch -2 2 NATURAL))
                     (lowfloortom . ,(ly:make-pitch -2 3 NATURAL))
                     (closedhihat . ,(ly:make-pitch -2 3 SHARP))
                     (hihat . ,(ly:make-pitch -2 4 FLAT))
                     (highfloortom . ,(ly:make-pitch -2 4 NATURAL))
                     (pedalhihat . ,(ly:make-pitch -2 4 SHARP))
                     (lowtom . ,(ly:make-pitch -2 5 NATURAL))
                     (openhihat . ,(ly:make-pitch -2 5 SHARP))
                     (halfopenhihat . ,(ly:make-pitch -2 5 SHARP))
                     (lowmidtom . ,(ly:make-pitch -2 6 NATURAL))
                     (himidtom . ,(ly:make-pitch -1 0 NATURAL))
                     (crashcymbala . ,(ly:make-pitch -1 0 SHARP))
                     (crashcymbal . ,(ly:make-pitch -1 1 FLAT))
                     (hightom . ,(ly:make-pitch -1 1 NATURAL))
                     (ridecymbala . ,(ly:make-pitch -1 1 SHARP))
                     (ridecymbal . ,(ly:make-pitch -1 2 FLAT))
                     (chinesecymbal . ,(ly:make-pitch -1 2 NATURAL))
                     (ridebell . ,(ly:make-pitch -1 3 NATURAL))
                     (tambourine . ,(ly:make-pitch -1 3 SHARP))
                     (splashcymbal . ,(ly:make-pitch -1 4 NATURAL))
                     (cowbell . ,(ly:make-pitch -1 4 SHARP))
                     (crashcymbalb . ,(ly:make-pitch -1 5 NATURAL))
                     (vibraslap . ,(ly:make-pitch -1 5 SHARP))
                     (ridecymbalb . ,(ly:make-pitch -1 6 NATURAL))
                     (mutehibongo . ,(ly:make-pitch -1 6 SHARP))
                     (hibongo . ,(ly:make-pitch 0 0 NATURAL))
                     (openhibongo . ,(ly:make-pitch 0 1 DOUBLE-FLAT))
                     (mutelobongo . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
                     (lobongo . ,(ly:make-pitch 0 0 SHARP))
                     (openlobongo . ,(ly:make-pitch 0 1 FLAT))
                     (mutehiconga . ,(ly:make-pitch 0 1 NATURAL))
                     (muteloconga . ,(ly:make-pitch 0 2 DOUBLE-FLAT))
                     (openhiconga . ,(ly:make-pitch 0 1 SHARP))
                     (hiconga . ,(ly:make-pitch 0 2 FLAT))
                     (openloconga . ,(ly:make-pitch 0 1 DOUBLE-SHARP))
                     (loconga . ,(ly:make-pitch 0 2 NATURAL))
                     (hitimbale . ,(ly:make-pitch 0 3 NATURAL))
                     (lotimbale . ,(ly:make-pitch 0 3 SHARP))
                     (hiagogo . ,(ly:make-pitch 0 4 NATURAL))
                     (loagogo . ,(ly:make-pitch 0 4 SHARP))
                     (cabasa . ,(ly:make-pitch 0 5 NATURAL))
                     (maracas . ,(ly:make-pitch 0 5 SHARP))
                     (shortwhistle . ,(ly:make-pitch 0 6 NATURAL))
                     (longwhistle . ,(ly:make-pitch 1 0 NATURAL))
                     (shortguiro . ,(ly:make-pitch 1 0 SHARP))
                     (longguiro . ,(ly:make-pitch 1 1 NATURAL))
                     (guiro . ,(ly:make-pitch 1 0 DOUBLE-SHARP))
                     (claves . ,(ly:make-pitch 1 1 SHARP))
                     (hiwoodblock . ,(ly:make-pitch 1 2 NATURAL))
                     (lowoodblock . ,(ly:make-pitch 1 3 NATURAL))
                     (mutecuica . ,(ly:make-pitch 1 3 SHARP))
                     (opencuica . ,(ly:make-pitch 1 4 NATURAL))
                     (mutetriangle . ,(ly:make-pitch 1 4 SHARP))
                     (triangle . ,(ly:make-pitch 1 4 DOUBLE-SHARP))
                     (opentriangle . ,(ly:make-pitch 1 5 NATURAL))

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
  '((drums-style .
      (
       (acousticbassdrum () #f -3)
       (bassdrum () #f -3)
       (sidestick cross #f 1)
       (acousticsnare () #f 1)
       (snare () #f 1)
       (handclap triangle #f 1)
       (electricsnare () #f 1)
       (lowfloortom () #f -4)
       (closedhihat cross "stopped" 3)
       (hihat cross #f 3)
       (highfloortom () #f -2)
       (pedalhihat cross #f -5)
       (lowtom () #f -1)
       (openhihat cross "open" 3)
       (halfopenhihat xcircle #f 3)
       (lowmidtom () #f 0)
       (himidtom () #f 2)
       (crashcymbala xcircle #f 5)
       (crashcymbal xcircle #f 5)
       (hightom () #f 4)
       (ridecymbala cross #f 5)
       (ridecymbal cross #f 5)
       (chinesecymbal mensural #f 5)
       (ridebell () #f 5)
       (splashcymbal diamond #f 5)
       (cowbell triangle #f 5)
       (crashcymbalb cross #f 5)
       (vibraslap diamond #f 4)
       (ridecymbalb cross #f 5)
       ))

    (timbales-style .
      ((losidestick cross #f -1)
       (lotimbale () #f -1)
       (cowbell triangle #f 2)
       (hisidestick cross #f 1)
       (hitimbale () #f 1)
       ))


    (congas-style .
      ((losidestick cross #f -1)
       (loconga () #f -1)
       (openloconga () "open" -1)
       (muteloconga () "stopped" -1)
       (hisidestick cross #f 1)
       (hiconga () #f 1)
       (openhiconga () "open" 1)
       (mutehiconga () "stopped" 1)
       ))


    (bongos-style .
      ((losidestick cross #f -1)
       (lobongo () #f -1)
       (openlobongo () "open" -1)
       (mutelobongo () "stopped" -1)
       (hisidestick cross #f 1)
       (hibongo () #f 1)
       (openhibongo () "open" 1)
       (mutehibongo () "stopped" 1)
       ))


    (percussion-style .
      ((opentriangle cross "open" 0)
       (mutetriangle cross "stopped" 0)
       (triangle cross #f 0)
       (shortguiro () staccato 0)
       (longguiro () tenuto 0)
       (guiro () #f 0)
       (cowbell triangle #f 0)
       (claves () #f 0)
       (tambourine () #f 0)
       (cabasa cross #f 0)
       (maracas () #f 0)
       (handclap () #f 0)
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
  \override Staff.BarLine #'bar-size = #3

  \autoBeamOff

  \override Stem #'length = #7.40
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% USUL SPECIFICS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  \set Timing.beatStructure = #'(3 2 2 3)
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

bellstaff = {
  \override DrumStaff.StaffSymbol #'line-positions = #'(-3 3.2)
  \set DrumStaff.drumStyleTable = #(alist->hash-table mydrums)
  \override Staff.BarLine #'bar-size = #2.2
  \set DrumStaff.instrumentName = #""
  \override Stem #'length = #7.5
}

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
DUM="düm"
DU="dü"
TEK="tek"
TEEK="teek"
TE="te"
TA="ta"
KE="ke"
ME="me"
HEK="hek"
KA="kâ"

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

agirAksakPatternMidi = { b4 d8 d d4 d b8 d b4 d2 d8 d }

agirAksakPattern = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK
    \stemDown kdh4_\TEK
  }
}

agirAksakPatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh_\KE kdh4_\TEK kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown <kdh kdl>2_\HEK
    \stemDown kdh4_\TEK
  }
}

%%%%%%%%%%% agirduyek %%%%%%%%%%%%%
agirduyek = {\time 8/4 }

agirduyekbars = { s1 \nibar "dashed" s1 \nibar "|" }

agirduyekPatternMidi = { b4 d8 d d4 d b8 d b4 d d8 d }

agirduyekPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 8/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE
    \bar "|"
  }
}

%%%%%%%%%%% agirsakil %%%%%%%%%%%%%
agirsakil = {\time 48/2 \longbarbeams}

agirsakilbars = {
  s4*8 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*8 \nibar "||"
}

agirsakilPatternMidi = { b1 b2 d2 b1 b2 d2 b2 d b1 b2 d2 b4 d4 b8 d8 b4 d1 d1 b1 b1 d1 b1 d1 d1 b1 b2 d2 b2 b2 d2 b4 d4 b2 d2 b4 d4 b2 d2 d2 b4 d4 b4 d4 }

agirsakilPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 48/2
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "dashed"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
    \bar "|"
  }
}

%%%%%%%%%%% Aksak %%%%%%%%%%%%%
Aksak = {\time 9/8 \Aksakbeams }

%%%%%%%%%%% Aksak semai %%%%%%%%%%%%%
AksakSemai = {\time 10/8 \AksakSemaibeams }

AksakSemaiPatternMidi = { b8 d16 d d8 d d16 d b16 d b8 d4 d16 d }

AksakSemaiPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh4_\KA
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
  }
}

AksakSemaiPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh_\KE kdh4_\TEK kdh_\KA kdh8_\TE kdh_\KE \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown <kdh kdl>2_\HEK kdh8_\TE kdh_\KE
  }
}

%%%%%%%%%%% ayindevrirevani %%%%%%%%%%%%%
ayindevrirevani = {\time 14/8 \ayindevrirevanibeams }

ayindevrirevaniPatternMidi = {b8 d d b d d d b16 d b8 d d d d d}

ayindevrirevaniPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 14/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl^\DU \stemDown kdh_\KE \stemDown kdh_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh_\ME \bar "dashed"
    \stemDown kdh_\TEK \stemDown kdh_\KA \stemDown kdh_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% berefsan %%%%%%%%%%%%%
berefsan = {\time 32/4 \SenginSemaibeams }

berefsanbars = {
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed" \break
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

berefsaneightbars = {
  s8*6 \nibar "dashed"
  s8*6 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "|"
}

berefsanPatternMidi = { b4 b4 d d8 d d4 d b4 b4 d d8 d d4 d b4 d8 d d4 d b8 d b4 d4 d8 d b4 d8 d d4 d b8 d b4 d4 d8 d d4 d d4 d }

XberefsanPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

berefsanPattern = {
  \drummode {
    \stemUp kdl1^\DUM \stemDown kdh2_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemDown kdh2_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \bar "dashed"
    \stemUp kdl2^\DUM kdl2^\TEK \bar "dashed"
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh4_\KA \stemUp kdl4^\TEK \stemDown kdh4_\KA
  }
}

berefsanPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% nimberefsan %%%%%%%%%%%%%
nimberefsan = {\time 16/8 \nimberefsanbeams }

nimberefsanbars = {
  s8*3 \nibar "dashed"
  s8*5 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "|"
}

nimberefsanPatternMidi = { b4 b4 d d8 d d4 d b4 b4 d d8 d d4 d b4 d8 d d4 d b8 d b4 d4 d8 d b4 d8 d d4 d b8 d b4 d4 d8 d d4 d d4 d }

nimberefsanPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% nimberefsanfour %%%%%%%%%%%%%
nimberefsanfour = {\time 16/4 \sofyanbeams }

nimberefsanfourbars = {
  s4*3 \nibar "dashed"
  s4*5 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

nimberefsanfourPatternMidi = { b4 b4 d d8 d d4 d b4 b4 d d8 d d4 d b4 d8 d d4 d b8 d b4 d4 d8 d b4 d8 d d4 d b8 d b4 d4 d8 d d4 d d4 d }

nimberefsanfourPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% cenber %%%%%%%%%%%%%
cenber = {\time 24/4 \SenginSemaibeams}

cenberbars = {
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "|"
}

cenberbarsFOURS = {
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar ""
  s2 \nibar "dashed"
  s1 \nibar ""
  s2 \nibar "dashed"
  s1 \nibar "|"
}

cenberPatternMidi = { b4 d8 d d4 d b4 b8 d b4 b4 d4 d8 d d4 d d4 d b8 d b4 d4 d8 d d4 d d4 d d4 d }

XcenberPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 24/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "dashed"
    \stemDown  kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

cenberPattern = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl2^\DUM kdl4^\DUM kdl4^\DUM \bar "dashed"
    \stemDown kdh2_\TEK kdh2_\TEK kdh2_\TEK \bar "dashed" \break
    \stemUp kdl2^\DUM \stemDown kdh_\TA \stemDown <kdh kdl>2_\HEK \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

cenberPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "dashed"
    \stemDown  kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

cenbertwo = {\time 24/2 \SixTwobeams}

cenbertwobars = {
  s2*2 \nibar ""  s2*2 \nibar "dashed"
  s2*2 \nibar ""  s2*2 \nibar "dashed"
  s2*3 \nibar "" s2*3 \nibar "dashed"
  s2*3 \nibar "" s2*3 \nibar "dashed"
  s2*2 \nibar ""  s2*2 \nibar "|"
}

%%%%%%%%%%% agircenber 48/4 %%%%%%%%%%%%%
%agircenber = {\time 48/4 \SenginSemaibeams}
agircenber = {\time 24/2 \longbarbeams}

agircenberbars = {
  s1 \nibar "" s1 \nibar "dashed"
  s1 \nibar "" s1 \nibar "dashed"
  s1. \nibar "" s1. \nibar "dashed"
  s1. \nibar "" s1. \nibar "dashed"
  s1 \nibar "" s1 \nibar "dashed"
}

agircenberPatternMidi = { b4 d8 d d4 d b4 b8 d b4 b4 d4 d8 d d4 d d4 d b8 d b4 d4 d8 d d4 d d4 d d4 d }

agircenberPattern = {
  \set Staff.instrumentName = \markup { \center-align { "24" \line { "4" } } }
  \time 24/4
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"

  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA

  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% cenberagir 12/4%%%%%%%%%%%%%
cenberagir = {\time 12/4 \SenginSemaibeams}

cenberagirbars = {
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

cenberagirbarsBREAKS = {
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s2. \nibar "" \break
  s2. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "|"
}

cenberagirPatternMidi = { b4 d8 d d4 d b4 b8 d b4 b4 d4 d8 d d4 d d4 d b8 d b4 d4 d8 d d4 d d4 d d4 d }

cenberagirPattern = {
  \set Staff.instrumentName = \markup { \center-align { "24" \line { "4" } } }
  \time 24/4
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d4^\DUM \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d4^\DUM \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemUp d4^\DUM \bar "|"

  \stemDown  b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"

  \stemUp d8^\DU \stemDown b_\ME \stemUp d4^\DUM \stemDown b4_\TEK \stemDown b8_\TE \stemDown b_\KE \stemDown b4_\TEK \stemDown b_\KA

  \stemDown b4_\TEK \stemDown b_\KA \stemDown b4_\TEK \stemDown b_\KA \bar "|"
}

%%%%%%%%%%% cifteduyek %%%%%%%%%%%%%
cifteduyek = { \time 16/4 \cifteduyekbeams }

cifteduyekbars = {
  \repeat unfold 3 {s1 \nibar "dashed" }
  s1 \nibar "|"
}

cifteduyekPatternMidi = { b4 d8 d d4 d d4 d d4 d8 d b4 d b4 d d d8 d d4 d }

XcifteduyekPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 16/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl4^\DU \stemDown kdh_\ME \bar "dashed" \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

cifteduyekPattern = {
  \drummode {
    \stemUp kdl2^\DUM \stemDown kdh1_\TEK kdh2_\TEK \bar "dashed"
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE
  }
}

cifteduyekPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl4^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% curcuna %%%%%%%%%%%%%
curcuna = { \time 10/8 \curcunabeams }

curcunaPatternMidi = { b4 b8 d4 b4 d4 d8 }

curcunaPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 10/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh4_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK
    \bar "|"
  }
}

curcunasixteen = { \time 10/16 \curcunasixteenbeams }

curcunasixteenPattern = {
  \set Staff.instrumentName = \markup { \center-align { "10" \line { "16" } } }
  \time 10/16
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d8^\DUM \stemDown d16^"te" b8_\KA \bar "|"
  \stemUp d8^\DUM \stemDown b8_\TEK \stemDown b16_\TEK \bar "|"
}

curcunasixteenPatternMidi = { \stemUp b8 b16 \stemDown d8 \stemUp b8 \stemDown d8 d16 }

%%%%%%%%%%% devrikebir %%%%%%%%%%%%%
devrikebir = { \time 28/4 \SenginSemaibeams}

devrikebirbars = {
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

devrikebirbarsNB = {
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "dashed"
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "|"
}

devrikebirPatternMidi = { b4 b d d8 d b4 d d8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d d d d d }

devrikebirPatternMidislow = { b2 b  d b4 d b8 d b4 d2 d d b b d d b4 d b d }

devrikebirPatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh2_\TEK \stemDown kdh_\TEK \bar "dashed"
    \stemDown kdh2_\TEK \stemUp kdl2^\DUM kdl2^\DUM  \bar "dashed"
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK  \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

devrikebirPatternTwo = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh1_\TEK \bar "dashed"
    \stemDown kdh1_\TEK \stemUp kdl2^\DUM  \bar "dashed"
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK  \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

devrikebirPatternThree = {
  \drummode {
    \stemUp kdl2^\DUM kdl2^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \bar "dashed"
    \stemDown kdh1_\TEK \bar "dashed"
    \stemDown kdh1_\TEK \stemUp kdl2^\DUM  \bar "dashed"
    \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK  \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

devrikebirPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

%%%%%%%%%%% devrikebirfourteen 14/4 %%%%%%%%%%%%%
devrikebirfourteen = {
  \time 14/4 %\SenginSemaibeams
}

devrikebirfourteenbars = {
  s4 s4 s4 s1  \nibar "dashed"
  s4 s4 s4 s1 \nibar "|"
}

devrikebirfourteenbarsNB = {
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "dashed"
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "|"
}

devrikebirfourteenPatternMidi = { b4 b d d8 d b4 d d8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d d d d d }

devrikebirfourteenPatternMidislow = { b2 b  d b4 d b8 d b4 d2 d d b b d d b4 d b d }

devrikebirfourteenPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 28/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% muzaafdevrikebir %%%%%%%%%%%%%
muzaafdevrikebir = { \time 28/4 \sofyanbeams }

muzaafdevrikebirbars = {
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

muzaafdevrikebirbarsNB = {
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "dashed"
  s2. \nibar "" s2. \nibar "dashed"
  s2 \nibar "" s2 \nibar "dashed" s2 \nibar "" s2 \nibar "|"
}

muzaafdevrikebirPatternMidi = { b4 b d d8 d b4 d d8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d d d d d }

muzaafdevrikebirPatternMidislow = { b2 b  d b4 d b8 d b4 d2 d d b b d d b4 d b d }

muzaafdevrikebirPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \bar "dashed" %\break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"

    \stemDown <kdl kdh>4_\HEK kdh8_\TE kdh8_\KE kdh4_\TEK kdh4_\KA kdh8_\TE kdh8_\KE kdh4_\TEK \bar "dashed"
    kdh4_\KA kdh8_\TE kdh8_\KE \stemUp kdl4^\DU \stemDown kdh_\ME \bar "dashed"
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" %\break
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \stemUp kdl4^\DU \stemDown kdh_\ME \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME} \bar "dashed"
    \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA  \bar "dashed" }

  }
}

%%%%%%%%%%% devrihindi %%%%%%%%%%%%%
devrihindi = {\time 7/8 \devrihindibeams }

devrihindiPatternMidi = { b8 d d b8 d d d }

devrihindiPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 7/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl8^\DUM \stemDown kdh_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% devrirevan %%%%%%%%%%%%%
devrirevan = { \time 26/4 \SenginSemaibeams}

devrirevanbars = {
  s4*5 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s4*5 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

devrirevanPatternMidi = { b4 b d d8 d b4 d d8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d b8 d b4 d d8 d d4 d d d d d }

devrirevanPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 28/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemDown kdh8_\TE \stemDown kdh_\KE \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed" \break
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% devrituranbars %%%%%%%%%%%%%
devrituran = {\time 7/8 \devrituranbeams }

devrituranPatternMidi = { b8 d d b8 d d d }

devrituranPattern = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh4._\TEK
  }
}

SdevrituranPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 7/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh_\TEK \stemDown kdh4._\TEK
    \bar "|"
  }
}

%%%%%%%%%%% duyek %%%%%%%%%%%%%
duyek = { \time 8/8 \duyekbeams }

duyekPatternMidi = {  b8  d16  d  d16  d  b16  d  b8  d  d16  d }

duyekPattern = {
  \drummode {
    \stemUp kdl8^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TEK \stemUp kdl4^\DUM \stemDown kdh4_\TEK
  }
}

duyekPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM \stemDown kdh8_\TE kdh8_\KE kdh4_\TEK kdh4_\KA
    \stemUp kdl8^\DU \stemDown kdh8_\ME \stemUp kdl4^\DUM \stemDown kdh4_\HEK \stemDown kdh8_\TE kdh8_\KE
  }
}

%%%%%%%%%%% evsat %%%%%%%%%%%%%
evsat = {\time 26/8 \evsatbeams }

evsatbars = {
  s8*5 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "dashed"
  s8*5 \nibar "dashed"
  s2 \nibar "dashed"
  s2 \nibar "|"
}

evsatPatternMidi = { b4 b8 d b4 b d4 d8 d d4 d d d b8 d b4 d d8 d d4 d d4 d d d }

evsatPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 20/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% fahte %%%%%%%%%%%%%
fahte = {\time 20/4 \SenginSemaibeams }

fahtebars = {
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "|"
}

fahtebarsBREAKS = {
  s1 \nibar "dashed"
  s2. \nibar ""
  s2. \nibar "dashed"
  s2. \nibar ""
  s2. \nibar "dashed"
  s1 \nibar "|"
}

fahtebarsLB = {
  s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "|"
}

fahtePatternMidi = { b4 b8 d b4 b d4 d8 d d4 d d d b8 d b4 d d8 d d4 d d4 d d d }

fahtePatternOne = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\DUM kdl4^\DUM \bar "dashed"
    \stemDown kdh2_\TEK kdh2_\TEK kdh2_\TEK  \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TA \stemDown <kdh kdl>2_\HEK \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

fahtePatternVelOne = {
  \drummode {
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" %\break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
  }
}

fahtePatternVelTwo = {
  \drummode {
    \repeat unfold 2 {\stemUp kdl4^\DUM} \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME} \bar "dashed"
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \repeat unfold 2 {\stemDown kdh4_\TEK \stemDown kdh_\KA} \bar "dashed"
    \repeat unfold 2 {\stemUp kdl8^\DU \stemDown kdh_\ME}
    \repeat unfold 2 {\stemDown kdh8_\TE \stemDown kdh_\KE} \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
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
  s1*2 \nibar "dashed"
  s1.*2 \nibar "dashed"
  s1.*2 \nibar "dashed"
  s1*2 \nibar "|"
}

agirfahtebarsBREAKS = {
  s1 \nibar "dashed"
  s2. \nibar ""
  s2. \nibar "dashed"
  s2. \nibar ""
  s2. \nibar "dashed"
  s1 \nibar "|"
}

agirfahtebarsLB = {
  s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "" s2 \nibar "dashed"
  s2 \nibar "" s2 \nibar "|"
}

agirfahtePatternMidi = { b4 b8 d b4 b d4 d8 d d4 d d d b8 d b4 d d8 d d4 d d4 d d d }

agirfahtePattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 20/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemUp kdl4^\DUM \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% hafif %%%%%%%%%%%%%
hafif = {\time 32/4 \sofyanbeams }

hafifbars = {
  \repeat unfold 7 {s1 \nibar "dashed" }
  s1 \nibar "|"
}

hafifPatternMidi = { b4 b8 d d4 d b8 d d d d4 d b4 b4 d4 d8 d b8 d d d d4 d b4 b4 d4 d8 d b8 d b8 d d8 d d8 d b8 d d8 d d4 d d4 d d4 d }

hafifPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemUp kdl8^\TE \stemDown kdh_\KE \stemDown kdh4_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed" \break
    \stemUp kdl4^\DUM \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh8_\TE \stemDown kdh_\KE \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh_\ME \stemDown kdh8_\TE \stemDown kdh_\KE \stemDown kdh4_\TEK \stemDown kdh_\KA \bar "dashed"
    \stemDown kdh4_\TEK \stemDown kdh_\KA \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% havi %%%%%%%%%%%%%
havi = {\time 64/4 \SenginSemaibeams }

havibars = {
  \repeat unfold 2 {s1 \nibar "dashed" }
  \repeat unfold 2 {s1. \nibar "dashed"}
  \repeat unfold 3 { s1 \nibar "dashed" s1 \nibar "dashed" s1 \nibar "dashed" }
  s1 \nibar "dashed" s1 \nibar "||"
}

haviPatternMidi = {  b2  b4  d b2 b4 d b2 b4 d b d b2 b4 d b d b2 d b b b4 d b d b2 b4 d b b d2 b4 d b2 d b4 d b d d2 b4 d d2 b4 d b b d2 b4 d }

haviPattern = {
  \set Staff.instrumentName = \markup { \center-align { "64" \line { "4" } } }
  \time 64/4
  \override Score.TimeSignature #'stencil = ##f
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

karsilamaPatternMidi = { b4 d d b4 d2 }

karsilamaPattern = {
  \set Staff.instrumentName = \markup { \center-align { "9" \line { "8" } } }
  \time 9/8
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}


%%%%%%%%%%% oynak %%%%%%%%%%%%%
oynak = {\time 9/8 \oynakbeams }

oynakPatternMidi = { b4 d d b4 d2 }

oynakPattern = {
  \set Staff.instrumentName = \markup { \center-align { "9" \line { "8" } } }
  \time 9/8
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

%%%%%%%%%%% muhammes %%%%%%%%%%%%%
muhammes = {\time 32/4 \sofyanbeams}

muhammesbars = {
  \repeat unfold 7 {s1 \nibar "dashed" } s1 \nibar "|"
}

muhammesPatternMidi = { \repeat unfold 2 {\repeat unfold 4 {b8 d16 d d8 d b16 d b8 d d16 d } } }


muhammesPattern = {
  \drummode {
    \stemUp kdl2^\DUM kdl4^\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemUp kdl2^\DUM kdl^\DUM\bar "dashed"
    \stemDown kdh_\TEK \stemUp kdl4^\TEK \stemDown kdh_\KA \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh_\TEK \bar "dashed"
    \stemUp kdl4^\TEK \stemDown kdh_\KA \stemUp kdl2^\DUM \bar "dashed"

    \stemUp kdl2^\TA \stemDown kdh2_\HEK \bar "dashed"

    kdl4^\TEK \stemDown kdh_\KA \stemUp kdl4^\TEK \stemDown kdh_\KA
  }
}

muhammesPatternVel = {
  \drummode {
    \stemUp kdl4^\DUM kdl4^\DUM \stemDown kdh4_\TEK kdh8_\TE kdh8_\KE \bar "dashed" kdh4_\TEK kdh4_\KA kdh4_\TEK kdh4_\KA \bar "dashed"

    \stemUp kdl4^\DU \stemDown kdh_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME \bar "dashed"
    kdh4_\TEK kdh_\KA kdh_\TEK kdh_\KA \bar "dashed" \break
    kdh2_\HEK kdh_\HEK \bar "dashed" kdh4_\HEK kdh8_\TEK kdh_\KE kdh4_\TEK kdh_\KA \bar "dashed"
    \stemUp kdl8^\DU \stemDown kdh8_\ME \stemUp kdl8^\DU \stemDown kdh8_\ME
    \repeat unfold 2 {kdh8_\TE kdh8_\KE} \bar "dashed"
    kdh4_\TEK kdh_\KA kdh_\TEK kdh_\KA
  }
}

%%%%%%%%%%% muhammessixteentwo %%%%%%%%%%%%%
muhammessixteentwo = {\time 16/2 \SixteenTwobeams}

muhammessixteentwobars = {
  \repeat unfold 3 {s4*8 \nibar "dashed" } s4*8 \nibar "|"
}

muhammessixteentwoPatternMidi = { \repeat unfold 2 {\repeat unfold 4 {b8 d16 d d8 d b16 d b8 d d16 d } } }

muhammessixteentwoPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)

    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "dashed"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "dashed" \break
    }
    \bar "|"
  }
}

%%%%%%%%%%% muhammessixteenfour %%%%%%%%%%%%%
muhammessixteenfour = {\time 16/4 \sofyanbeams}

muhammessixteenfourbars = {
  \repeat unfold 3 {s4*4 \nibar "dashed" } s1 \nibar "|"
}

muhammessixteenfourPatternMidi = { \repeat unfold 2 {\repeat unfold 4 {b8 d16 d d8 d b16 d b8 d d16 d } } }

muhammessixteenfourPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)

    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "dashed"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "dashed" \break
    }
    \bar "|"
  }
}

%%%%%%%%%%% nimsakil %%%%%%%%%%%%%
nimsakil = {\time 24/4 \SenginSemaibeams}

nimsakilbars = {
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

nimsakilPatternMidi = { b1 b2 d2 b1 b2 d2 b2 d b1 b2 d2 b4 d4 b8 d8 b4 d1 d1 b1 b1 d1 b1 d1 d1 b1 b2 d2 b2 b2 d2 b4 d4 b2 d2 b4 d4 b2 d2 d2 b4 d4 b4 d4 }

nimsakilPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 48/2
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "dashed"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
    \bar "|"
  }
}

%%%%%%%%%%% remel %%%%%%%%%%%%%
remel = {\time 28/2 \SixTwobeams}

remelbars = {
  s4*8 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*12 \nibar "dashed"
  s4*8 \nibar "dashed"
  s4*8 \nibar "|"
}

remelPatternMidi = { \repeat unfold 2 {\repeat unfold 4 {b8 d16 d d8 d b16 d b8 d d16 d } } }

remelPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)

    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "dashed"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "dashed" \break
    }
    \bar "|"
  }
}

%%%%%%%%%%% remelfour %%%%%%%%%%%%%
remelfour = {\time 28/4 \SenginSemaibeams}

remelfourbars = {
  % 4 6 4 6 4 4
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

remelfourPatternMidi = { \repeat unfold 2 {\repeat unfold 4 {b8 d16 d d8 d b16 d b8 d d16 d } } }

remelfourPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 32/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)

    \repeat unfold 4 {

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE \bar "dashed"

      \stemUp kdl8^\DUM \stemDown kdh16_\TE \stemDown kdh_\KE \stemDown kdh8_\TE
      \stemDown kdh_\KA \stemUp kdl16^\DU \stemDown kdh_\ME \stemUp kdl8^\DUM
      \stemDown kdh_\TEK \stemDown kdh16_\TE \stemDown kdh_\KE
      \bar "dashed" \break
    }
    \bar "|"
  }
}

%%%%%%%%%%% sakil %%%%%%%%%%%%%
sakil = {\time 48/4 \SenginSemaibeams}

sakilbars = {
  % 4 6 4 6 6 6 4 4 4 4
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1. \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "dashed"
  s1 \nibar "|"
}

sakilPatternMidi = { b1 b2 d2 b1 b2 d2 b2 d b1 b2 d2 b4 d4 b8 d8 b4 d1 d1 b1 b1 d1 b1 d1 d1 b1 b2 d2 b2 b2 d2 b4 d4 b2 d2 b4 d4 b2 d2 d2 b4 d4 b4 d4 }

sakilPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 48/2
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK \stemUp kdl8^\TE \stemDown kdh8_\KE \stemUp kdl4^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl1^\DUM \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemDown kdh1_\TEK \stemDown kdh1_\TEK \bar "dashed"
    \stemUp kdl1^\DUM \stemUp kdl2^\TE \stemDown kdh2_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \bar "dashed"
    \stemUp kdl2^\DUM \stemDown kdh2_\TEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl2^\DUM \bar "dashed"
    \stemDown kdh2_\TA \stemDown kdh2_\HEK \stemUp kdl4^\TE \stemDown kdh4_\KE \stemUp kdl4^\TE \stemDown kdh4_\KE
    \bar "|"
  }
}

%%%%%%%%%%% semai %%%%%%%%%%%%%
semai = {\time 3/4 \semaibeams}

semaiPatternMidi = { b4 d d }

semaiPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 3/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)

    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK
    \bar "|"
  }
}

%%%%%%%%%%% SenginSemai %%%%%%%%%%%%%
SenginSemai = {\time 6/4 \SenginSemaibeams }

SenginSemaiEight = {\time 6/8 \SenginSemaiEightbeams }

SenginSemaiPatternMidi = { b4 d d b4 d2 }

SenginSemaiPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 6/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh_\TEK \bar "dashed"
    \stemUp kdl4^\DUM \stemDown kdh2_\TEK
    \bar "|"
  }
}

%%%%%%%%%%% nim sofyan %%%%%%%%%%%%%
nimsofyan = {\time 2/4  }

nimsofyanPatternMidi = { b4 d }

nimsofyanPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \time 2/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh4_\TEK
    \bar "|"
  }
}

%%%%%%%%%%% sofyan %%%%%%%%%%%%%
sofyan = {\numericTimeSignature \time 4/4 \sofyanbeams }

sofyanPatternMidi = { b4 d8 d d4 d }

sofyanPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \numericTimeSignature
    \time 4/4
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh8_\TE \stemDown kdh_\KE
    \stemDown kdh4_\TEK \stemDown kdh_\KA
    \bar "|"
  }
}

%%%%%%%%%%% turksaksagi %%%%%%%%%%%%%
turkaksagi = {\time 5/8 \turkaksagibeams }

turkaksagibars = { s2. \bar "|" }

turkaksagiPatternMidi = { b8 d d b8 d4 }

turkaksagiPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \numericTimeSignature
    \time 5/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl4^\DUM \stemDown kdh_\TEK kdh8_\TEK
    \bar "|"
  }
}

%%%%%%%%%%% YurukSemai %%%%%%%%%%%%%
YurukSemai = {\time 6/8 \YurukSemaibeams }

YurukSemaiPatternMidi = { b8 d d b8 d4 }

YurukSemaiPattern = {
  \bellstaff
  \drummode {
    \autoBeamOff
    \numericTimeSignature
    \time 6/8
    \override TextScript #'Y-extent = #'(-1.5 . 1.5)
    \stemUp kdl8^\DUM \stemDown kdh_\TEK kdh_\TEK
    \stemUp kdl8^\DUM \stemDown kdh4_\TEK
    \bar "|"
  }
}

%%%%%%%%%%% zencir %%%%%%%%%%%%%
zencir = {
  \time 120/4 \set Timing.beatStructure = #'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
}

zencirPatternMidi = {
  \cifteduyekPatternMidi
  \fahtePatternMidi
  \cenberPatternMidi
  \devrikebirPatternMidi
  \berefsanPatternMidi
}

zencirPatternVel = {
  \drummode {
    \cifteduyekPatternVel
    \fahtePatternVelOne
    \cenberPatternVel
    \devrikebirPatternVel
    \berefsanPatternVel
  }
}

%%%%%%%%%%% zeybek %%%%%%%%%%%%%
zeybek = {\time 9/8 \zeybekbeams }

zeybekPatternMidi = { b4 d d b4 d2 }

zeybekPattern = {
  \set Staff.instrumentName = \markup { \center-align { "9" \line { "8" } } }
  \time 9/8
  \override Score.TimeSignature #'stencil = ##f
  \stemUp d4^\DUM \stemDown b_\TEK b_\TEK \bar "|"
  \stemUp d4^\DUM \stemDown b2_\TEK \bar "|"
}

agirzeybek = {\time 9/4 \zeybekbeams }

%%%%%%%%%%% darbeyn %%%%%%%%%%%%%
darbeyn = { \SixTwobeams }

darbeynbars = {
  \time 28/2
  \SixTwobeams
  \remelbars
  \remelbars
  \time 32/4 \muhammesbars
}

darbeynPatternMidi = {
  \cifteduyekPatternMidi
  \fahtePatternMidi
  \cenberPatternMidi
  \devrikebirPatternMidi
  \berefsanPatternMidi
}

\layout {
  ragged-right=##f
  \context { \Score \remove "Bar_number_engraver" }
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
