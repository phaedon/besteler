\version "2.12.3"

\include "makam-DEV.ly"
\include "makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOn
measures={}

HaneOne = \relative c' {
  \mark \markup {"I." \HaneText}
  c'4 d d c f8. e16 d8 e
  d e d c g'8. f16 f8. e16
  e8. d16 d8. c16 d8 e16 f e8 d
  e16 d c bfc c8 d c4 r f8. e16 f8 g
  a g f e r8 d16 e d8 e e f e d c bfc a g
}

Teslim = \relative c' {
  \CustomSegno "Teslim" ""
  r8 d'4 e8 d c bfc a bfc c bfc4
  c8 d c4 d8 e d e
  f4 e8 g f4 g8 a
  g8. f16 e8 f e f d e d4 e8 f
  e8 f e d g f e d c4 d4 d4. r8
  r8 a4 bfc8 c d c bfc c4 d8 e
  d c bfc a bfc4 c8 bfc c4 d8 c d4 e8 d
  d g f e e d d c c bfc bfc c
  bfc c bfc a a4. e'8 d8 c16 bfc a4 a4.
  r8-\MyRehearse #'(-2.0 . -2.0) "(Fine)"
}

HaneTwo = \relative c' {
  \mark \markup {"II." \HaneText}
  d'4 e e f8 g e f e d d e d c c d c bfc
  c8. bfc16 a4 a4. g'8 fb g a b a4 g8 fb e4. f8
  e8. d16 cb8 d e4. d8 g8. fb16 e4 e4. r8
  d4 e e a8 g g f f e r8 e16 f e8 d d c16 bfc e8 d16 c
  c4. d8 e4 e8 f d4 d8 e c4 c8 d d c c bfc
  r8 a16 bfc a8 g a e' d c
  c8. bfc16 bfc16 c bfc16 a a4.
  r8-\MySegno #'(-0.0 . -0.0)
}

HaneThree = \relative c' {
  \mark \markup {"III." \HaneText}
  g''4 a a2 a8 bfk a g
  fb g a bfc! g4 a
  a8 d c bfc c8. bfc16 a4 a4 r r4 c4 c8 bfc bfc a
  a8 g g fb fb8. e16 a8 g16 fb
  fb8. e16 e8 e16 d d4. r8 r4 g8 a a4. bfc8 c4 bfc8 a
  a g g f f e e d r8 c16 d c8 d e8 f16 g e8 d
  d c c bfc r8 a16 bfc a8 bfc c4 d8 e
  d c c bfc bfc8. a16 bfc8 c bfc c bfc a a4.
  r8-\MySegno #'(-0.0 . -0.0)
}

HaneFour = \relative c' {
  \mark \markup {"IV." \HaneText}
  g'4 a a4. g8 a8. g16 a8 bfc c8 d bfc c a bfc g4
  r8 a16 bfc a8 bfc c4 d8 e d4. bfc8 c4. a8 bfc8. a16 bfc8 c
  bfc8. a16 g8 a g4 a a4. bfc8 c4. r8
  bfc4 c c8 dfb16 e dfb8 c c dfb c bfc c8. bfc16 a4 bfc16 a g8 a bfc
  c dfb c bfc dfb e c dfb e f e dfb dfb c c bfc r8 a16 bfc a8 bfc
  c4 dfb8 dfb16 c c4. c16 bfc bfc8. a16 a c bfc a a4.
  r8-\MyRehearse #'(-7.0 . -2.0) "D.S. al fine"
}

BarlinesForm = {
  \repeat unfold 9 { \devrikebirbars }
}

TheBreaks = { }

MelodyForm = {
  \devrikebir
  \key c \beyati
  \HaneOne \bar "||" \SectionBreak
  \Teslim \bar "|." \SectionBreak
  \HaneTwo \bar "||" \SectionBreak
  \HaneThree \bar "||" \SectionBreak
  \HaneFour \bar "||"
}

LyricsOne={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsTwo={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsThree={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsPrint = \markup { }

usulcreate = { %\devrikebirPattern
}

MelodyFormMidi = {
  \devrikebir
  \key c \beyati
  \tempo 4=60
  \HaneOne
  \Teslim
  \HaneTwo
  \Teslim
  \HaneThree
  \Teslim
  \HaneFour
  \Teslim
}

UsulFormMidi = \repeat unfold 15 { %\devrikebirPatternMidi
}

%%%%%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%%%%%%

\header {
  titleUC = \markup {\beyatiText \PesrevText}
  titleLC = \markup {\beyatiTextLC \PesrevTextLC}
  formUC = \PesrevText
  formLC = \PesrevTextLC
  subtitleText = \markup {""}
  composername = \markup {\MuzikText \NeyzenEminDede \NeyzenEminDedeDATE}
  poetname = \markup {""}
  usulname = \markup {\UsuluText \devrikebirText}
  tempopiece = \markup {""}
  musicsource = \markup {""}
  dedication = \markup {""}
  subsubtitle = \markup {""}
  datecreated = \markup {""}
}

LyricsPrint = \markup {}

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
