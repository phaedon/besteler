\version "2.12.3"

\include "makam-DEV.ly"
\include "makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOn
measures={}

melodyone = \relative c' {
  e'4 (f f8 e4 d8 d c d c c) c4 d8 e4. (c8 f4. e8)
  r4 e8 (f g a4 g8 g f4 e8 e d e d d c d c c4 a'4)
  g8. (f16 e8 f e f g16 a g f f4.)
  \SpanTextUp "(SAZ)" c8\startTextSpan d e f4\stopTextSpan
  r4 e8 (f g a4 g8 g f4 e8 e d~ d) c
  c2 c8 (bfk16 c d8. bfk16 a2 a8 g16 a bfk8. g16
  f4. f'8 f efk d c c bfk a g f4) a8. (g16)
  g8. (f16 e8 f e f g16 a g f f4.)
  \SpanTextUp "(SAZ)" g8\startTextSpan a g a4\stopTextSpan
}

melodytwo = \relative c' {
  \CustomSegno "" ""
  r4 g'4~ g c c8. (bfc16 a8 bfc) c2
  r8 f4 (e8 d c d) e f (e16 f g a g f) f2
  f4 (a8 g) f (a g f) e (f e d) c2
  bfk'4. (a8 gb a bfk16 c bfk a a4.)
  \SpanTextUp "(SAZ)" g8\startTextSpan f g a4\stopTextSpan
  e8 (f g a a g4 f8 f e4 d8 d c d) c c2 c8 (bfk16 c
  d8. bfk16 a2) a8 (g16 a bfk8. g16 f4. f'8
  f efk d c c bfk a g f4) a8. (g16)
  g8. (f16 e8 f e f g16 a g f f4.-\MyRehearse #'(-2.0 . -2.0) "(Fine)")
  \SpanTextUp "(SAZ)" c'8\startTextSpan bfc dfb c4\stopTextSpan

  \bar "||" \break

  bfc4 (c2 c8 dfb dfb c4 bfc8) r8 c4 dfb8
  (dfb8 e dfb c c dfb) c (bfc)
  c8. (bfc16 a4 bfc8 c bfc4)
  r4 c8 (dfb c4 dfb8 e dfb c4 bfc8 c e dfb c)
  c4 (bfc8 c bfc c bfc a a4.)
  \SpanTextUp "(SAZ)" f'8\startTextSpan e f g4\stopTextSpan
  f4 (g2 g8 afb g f4 e8) r f (e) f g4 (afb8 g f4 e8 afb)
  g8 (f4 e8 e8. d16 d8. c16 c8. bfk16 a8 bfk c d c bfk
  a4 g8 f) r8 a16 (bfk a8 g) g8. (f16 e8 f e f g16 a g f
  f4.)
  \SpanTextUp "(SAZ)" g8\startTextSpan a g a4-\MySegno #'(-0.0 . -0.0)\stopTextSpan
}

BarlinesForm = {
  \repeat unfold 6 { \muhammesbars }
}

TheBreaks = { }

MelodyForm = {
  \muhammes
  \key c \acemasiran
  \melodyone \bar "||" \SectionBreak
  \melodytwo \bar "||"
}

LyricsOne={
  \lyricsto "MelodyForm" \new Lyrics {
    Ah Bin ce fa gör sem \MySkip #4
    ey sa nem ah
    sen den \MySkip #4

    Te ne nen ni te ne nen ni
    dir dir dir ten na \MySkip #4
    ey sa nem ah ah sen den \MySkip #4

    Ah Ta li im dir se ni \MySkip #4
    yar yar ve fa sız e den
  }
}

LyricsTwo={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsThree={\lyricsto "MelodyForm" \new Lyrics { _ }}

LyricsPrint = \markup {
  \fill-line {
    \column {
      %\null
      "Bin cefa görsem ey sanem senden"
      "Tali'imdir seni vefasız eden"
    }
  }
}

usulcreate = {
  %\muhammesPattern
}

MelodyFormMidi = {}

UsulFormMidi = \repeat unfold 12 {
  %\muhammesPatternMidi
}

%%%%%%%%%%%%%%%%%%%% HEADER %%%%%%%%%%%%%%%%%%%%

\header {
  titleUC = \markup {\acemasiranText \besteText}
  titleLC = \markup {\acemasiranTextLC \besteTextLC}
  formUC = \besteText
  formLC = \besteTextLC
  subtitleText = \markup {(Bin cefa görsem ey sanem senden)}
  composername = \markup {\bestecompText \ZekaiDede \ZekaiDedeDATE}
  poetname = \markup {""}
  usulname = \markup {\UsuluText \muhammesText}
  tempopiece = \markup {""}
  musicsource = \markup {""}
  dedication = \markup {""}
  subsubtitle = \markup {""}
  datecreated = \markup {"June 29, 2010"}
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
  \LyricsPrint
}

%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%
