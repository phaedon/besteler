\version "2.12.3"

\include "makam-DEV.ly"
\include "makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOff
measures={}

melodyone = \relative c' {
  \TupletString \times 2/3 {bfc'8 c d} efk8. f16 d8. c16 bfc8. afk16
  g4 \TupletString \times 2/3 {c8 bfc c} d4 g4
  \TupletString \times 2/3 {bfc,8 c d} efk8. f16 d8. c16 bfc8. afk16
  \TupletString \times 2/3 {bfc8 c d c bfc afk} g4 r
}

melodytwo = \relative c' {
  \repeat volta 2 {  \TupletString \times 2/3 {c'8 d d d d d d d d d d d }
    \TupletString \times 2/3 {efk d c c bfk d } c2}
  
  \alternative {
    {\TupletString \times 2/3 {bfk8 c d efk d c bfk c d efk d c}
      bfk8. afk16 afk8. g16 g4 d'4}
    {\TupletString \times 2/3 {bfk8 c d efk f g afk g f efk d c}
      bfk8. afk16 afk8. g16 g2}
  }
}

melodythree = \relative c' {
  \TupletString \times 2/3 {a'8 bfk c} a8. f16 \times 2/3 {a8 bfk c} a8. f16  
  \TupletString \times 2/3 {a8 bfk c dfb c bfk c bfk a } bfk4
  \TupletString \times 2/3 {efc8 f g afk g f } efc8. dfb16 c8. bfk16
  bfk8. afc16 afc8. g16 g4 r
}

melodyfour = \relative c' {
  \repeat volta 2 {
    \TupletString \times 2/3 {afk''8 g f efk d c }
    d8. efc16 f8. g16 \TupletString \times 2/3 {afk8 g f}
    efc8. f16 g4 c
    \TupletString \times 2/3 {bfc8 c d efk d c} bfc8. afk16 g8. f16
  }
  
  \alternative {
    { f8. efk16 efk8. d16 d4 g }
    { f8. efk16 efk8. d16 d2 }
  }
}

melodyfive = \relative c' {
  \repeat volta 2 {  \TupletString \times 2/3 {f'8 g g g g g g g g g g g }
    \TupletString \times 2/3 {afk g f } efk8. d16 c2
    \TupletString \times 2/3 {bfk8 c d efk f g afk g f efk d c}
  }
  
  \alternative {
    { bfk8. afk16 afk8. g16 g4 g }
    { bfk8. afk16 afk8. g16 g4 d' }
  }
  
}

melodysix = \relative c' {
  \repeat volta 2 { 
    \repeat unfold 3 { bfc'16 c d8 r d efk16 d efk f
      d4 }
  }
  
  \alternative {
    { bfc16 c d efk d efk d c bfc c bfc afk g4 }
    { bfc4 afk g2-\MyRehearse #'(-2.0 . -2.0) "(Fine)" }
  }
}

BarlinesForm = { }

TheBreaks = { }

MelodyForm = {
  \sofyan
  \key c \kurdilihicazkar
  \repeat volta 2 { \melodyone } \SectionBreak
  \melodytwo  \SectionBreak
  \repeat volta 2 {\melodythree}
  \melodyfour
  \melodyfive \SectionBreak
  \melodysix  \bar "|." \SectionBreak
  
}

LyricsOne={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsTwo={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsThree={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsPrint = \markup { }

usulcreate = { 
  %\sofyanPattern
}

MelodyFormMidi = {
  \sofyan
  \key c \kurdilihicazkar
  \tempo 4=50
  \melodyone
  \melodytwo
  \melodythree
}

UsulFormMidi = {
%{  \repeat unfold 24 { \sofyanPatternMidi }
  \repeat unfold 4 { \sofyanPatternMidi }%}
}

%%%%%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%%%%%%

\header {
  titleUC = \markup {\kurdilihicazkarText \longaText}
  titleLC = \markup {\kurdilihicazkarTextLC \longaTextLC}
  formUC = \longaText
  formLC = \longaTextLC
  subtitleText = \markup {"How it's played Version"}
  composername = \markup {\MuzikText \KemaniSebuh \KemaniSebuhDATE}
  poetname = \markup {""}
  usulname = \markup {\UsuluText \sofyanText}
  tempopiece = \markup {""}
  musicsource = \markup {""}
  dedication = \markup {""}
  subsubtitle = \markup {""}
  datecreated = \markup {"July 6, 2010"}
}

\paper {
  ragged-last-bottom = ##t %t is default
  %between-system-padding = 4\mm %4\mm is default
  %between-system-space = 20\mm %20\mm is default
  system-system-spacing #'basic-distance = #16
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
