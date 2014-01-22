\version "2.12.3"

\include "makam.ly"
\include "../../aaa-makam-inits/makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOn
measures={}

haneone = \relative c' {
  \mark \markup {"I." \HaneText}
  g''4 f8 g4 c16 bfc afb c bfc4 r8
  d32 c16. c32 bfc16. bfc32 afb16. g8. f16 afb16 g f efc f8 efc d
  bfk'4 a8   bfk16 c bfk8   a16 bfk a8  g8 f efc
  d8 c4 g'8 d efc16 f efc f g4 r8
}

teslim = \relative c' {
  \CustomSegno "Teslim" ""
  g''16 afk g8 f efk8. d16 efk8 f g16 afk g f efk f
  d8 bfk' afk g f16 efk d16 c bfk d c4 r8
  bfk8 c d  \times 2/3 {efk d c}  \times 2/3 {bfk afk g} afk8 g f
  g8 efk'16 d c8 d8 c16 bfk c8 bfk16 afk g4 r8-\MyRehearse #'(-2.0 . -2.0) "(Fine)"
}

hanetwo = \relative c' {
  \mark \markup {"II." \HaneText}
  bfc''16 c bfc afb g fb g8 fb g afb g fb efb
  afb4. fb4 g efb4.
  d4 afb'8 g fb16 efb d16 c bfc d c8 bfc16 afb g8
  g8 bfc16 c d bfc c8. d16 efc f efc f g4 r8-\MySegno #'(-0.0 . -0.0)
}

hanethree = \relative c' {
  \mark \markup {"III." \HaneText}
  c''8 bfk afk g4 c16 bfk afc bfk c8 dfb efc
  g8 f efc d8. c16 dfb16 c bfk a bfk4 r8
  bfk16 afc g afc g f efc8. cfc'16 bfk8 afc g8 f efc
  f4 d8 efc16 f efc8 f16 g f8 g4 r8-\MySegno #'(-0.0 . -0.0)
}

hanefour = \relative c' {
  \mark \markup {"IV." \HaneText "- Sengin Semâi"}

  \repeat volta 2 {
    bfk'4 efk d8 efk g f16 efk efk4 r4
    d8 g f16 g32 afk g16 f efk d c d bfk4 c16 d32 efk d16 c c4
    afk'8 g16 f f4 bfk8 afk16 g d'8 c16 bfk c4 r4
  }

  \alternative {
    {  c,16 d efk g f efk d c \times 2/3 {bfk16 c d} \times 2/3 {efk16 d c} bfk16 afk afk g g8 f g afc }
    {  efk'16 d c d c4 g'8 d efc16 f efc f g4 r4-\MyRehearse #'(-6.0 . -2.0) "D.S. al fine"}
  }

}

BarlinesForm = { }


TheBreaks = { }

MelodyForm = {
  \AksakSemai
  \kurdilihicazkar
  \haneone \bar "||" \SectionBreak
  \teslim \bar "|." \SectionBreak
  \hanetwo \bar "||" \SectionBreak
  \hanethree \bar "||" \SectionBreak
  \SenginSemai
  \hanefour \bar "||"
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
  \kurdilihicazkar
  \tempo 4=60
  \haneone
  \teslim
  \hanetwo
  \teslim
  \hanethree
  \teslim

  \SenginSemai
  \tempo 4=70
  \hanefour

  \AksakSemai
  \tempo 4=60
  \teslim
}

UsulFormMidi = {
  %{  \repeat unfold 24 { \AksakSemaiPatternMidi }
      \repeat unfold 16 { \SenginSemaiPatternMidi }
      \repeat unfold 4 { \AksakSemaiPatternMidi }%}
}

%%%%%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%%%%%%

\header {
  titleUC = \markup {\kurdilihicazkarText \SazSemaisiText}
  titleLC = \markup {\kurdilihicazkarTextLC \SazSemaisiTextLC}
  formUC = \SazSemaisiText
  formLC = \SazSemaisiTextLC
  subtitleText = \markup {""}
  composername = \markup {\MuzikText "Phaedon Sinis"}
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
  between-system-padding = 4\mm %4\mm is default
  between-system-space = 20\mm %20\mm is default
}

\book {
  \score {
    \new Staff  <<
      \new Voice = "BarlinesForm" { \BarlinesForm }
      \new Voice = "TheBreaks" { \TheBreaks }
      \new Voice = "MelodyForm" {
        \transpose g g
        \relative { \StaffOverides \MelodyForm \signature }
      }
      \LyricsOne \LyricsTwo \LyricsThree
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
