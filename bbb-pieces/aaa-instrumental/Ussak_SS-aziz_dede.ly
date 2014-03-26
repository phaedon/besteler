\version "2.12.3"

\include "makam-DEV.ly"
\include "makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOn
measures={}

HaneOne = \relative c' {
  \mark \markup {"I." \HaneText}
  g'8 a a a16 bfc a g a g fb g a4 bfc8
  g4 c8 bfc16. a32 bfc16 c bfc16. a32 g16 bfc a4 r8
  g4 d'8 c16. bfc32 c16 d bfc16. a32 bfc16 c bfc16 a g4
  a8 bfc c16 d c32 bfc a16 bfc c bfc16. a32 g16 bfc a4 r8
  
}

Teslim = \relative c' {
  \CustomSegno "Teslim" ""
  a'4 f'8 e8. c16 d8 e f g f16 e
  d4 c16 bfc a8. g16 f'16. e32 d16 e d4 r8
  c16 d e f g a g f e d d f e d d c bfc a g8
  c8. bfc16 a8 bfc16. a32 bfc16 c bfc16. a32 g16 bfc a4
  r8-\MyRehearse #'(-2.0 . -2.0) "(Fine)"
}

HaneTwo = \relative c' {
  \mark \markup {"II." \HaneText}
  g'4 d'8 d8. efb16 fb8 g16 a fb8 efb d
  d4 bfc8 c16 d c d c bfc bfc a a4 r8
  f'8 e d d16 a' g f f e d e d c bfc a g8
  c4 a8 bfc16. a32 bfc16 c bfc16. a32 g16 bfc a4
  r8-\MySegno #'(-0.0 . -0.0)
}	

HaneThree = \relative c' {
  \mark \markup {"III." \HaneText}
  g''8 a a a16 bfk a g a g fb g fb8 g a
  fb efb d a' g16 fb fb efb efb d d efb fb g a8
  g a a a16 c bfc a a g f e d8 c bfc16 a
  bfc4 a8 d c16 bfc bfc a bfc g a4  r8-\MySegno #'(-0.0 . -0.0)
  
}

HaneFour = \relative c' {
  \mark \markup {"IV." \HaneText "-" \SenginSemaiText} a'8. f'16 f8. e16 e8. d16 c8. e16 d2
  c8. d16 e8. f16 g8. a16 g8. f16 e8. d16 c8. bfc16
  a8. a'16 g8. f16 f8. e16 e8. d16 \TupletString \times 2/3 { f8 e d c bfc a}
  g8. e'16 d8. bfc16 c8. a16 bfc8. g16
  a2-\MyRehearse #'(-6.0 . -2.0) "D.S. al fine"
}

BarlinesForm = { }

TheBreaks = { }

MelodyForm = {
  \AksakSemai
  \key c \ussak
  \HaneOne \bar "||" \SectionBreak
  \Teslim \bar "|." \SectionBreak
  \HaneTwo \bar "||" \SectionBreak
  \HaneThree \bar "||" \SectionBreak
  \SenginSemai
  \HaneFour \bar "||"
}

LyricsOne={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsTwo={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsThree={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsPrint = \markup { }

usulcreate = { 
%{  \AksakSemaiPattern \break
  \SenginSemaiPattern%}
}

MelodyFormMidi = {
  \AksakSemai
  \key c \ussak
  \tempo 4=50
  \HaneOne
  \Teslim
  \HaneTwo
  \Teslim
  \HaneThree
  \Teslim
  
  \SenginSemai
  \tempo 4=70
  \HaneFour
  
  \AksakSemai
  \tempo 4=60
  \Teslim
}

UsulFormMidi = {
%{  \repeat unfold 24 { \AksakSemaiPatternMidi }
  \repeat unfold 16 { \SenginSemaiPatternMidi }
  \repeat unfold 4 { \AksakSemaiPatternMidi }%}
}

%%%%%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%%%%%%

\header {
  titleUC = \markup {\ussakText \SazSemaisiText}
  titleLC = \markup {\ussakTextLC \SazSemaisiTextLC}
  formUC = \SazSemaisiText
  formLC = \SazSemaisiTextLC
  subtitleText = \markup {""}
  composername = \markup {\MuzikText \NeyzenAzizDede \NeyzenAzizDedeDATE}
  poetname = \markup {""}
  usulname = \markup {\UsuluText \aksaksemaiText}
  tempopiece = \markup {""}
  musicsource = \markup {""}
  dedication = \markup {""}
  subsubtitle = \markup {""}
  datecreated = \markup {""}
}

\paper {
  ragged-last-bottom = ##f %t is default
  %between-system-padding = 4\mm %4\mm is default
  %between-system-space = 20\mm %20\mm is default
  system-system-spacing #'basic-distance = #14
  score-system-spacing =
  #'((basic-distance . 12)
     (minimum-distance . 6)
     (padding . 1)
     (stretchability . 12))%}
}

\book {
  \score {
    <<
      \new Staff <<
        \new Voice = "BarlinesForm" <<
          \BarlinesForm
        >>
        \new Voice = "BreaksForm" <<
          \TheBreaks
        >>
        \new Voice = "MelodyForm" \transpose d d <<
          \StaffOverides \MelodyForm
        >>
        \lyricsto "MelodyForm" \new Lyrics \LyricsOne \LyricsTwo \LyricsThree
        %\lyricsto "MelodyForm" \new Lyrics \FirstWordstwo
      >>
      %\new DrumStaff << \kudumstaff \UsulForm >>
    >>
    \layout {
      ragged-right=##f
      \context { \Score \remove "Bar_number_engraver" }
      \context { \Lyrics \override LyricText #'font-size = #-0.5 }
    }
  }
  %{  \LyricsPrint%}
}

%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%
