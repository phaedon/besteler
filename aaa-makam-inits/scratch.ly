\version "2.12.3"

\include "makam-DEV.ly"
\include "makam-includes.ly"

%%%%%%%%%%%%%%%%%%%% Score %%%%%%%%%%%%%%%%%%%%

usulbreak=\breakUsulOff
SectionBreak=\breakSectionOn
measures={}

music=\relative c' {a'2~ a s2 s2}

\relative c' {
\StaffOverides
\time 8/4

  \mark \markup {"\buzurk"}
  \key c \buzurk
  \music
  \mark \markup {"\cargah"}
  \key c \cargah
  \music
  \mark \markup {"\evcara"}
  \key c \evcara
  \music
  \mark \markup {"\ferahnak"}
  \key c \ferahnak
  \music
  \mark \markup {"\guldeste"}
  \key c \guldeste
  \music
  \mark \markup {"\hicaz"}
  \key c \hicaz
  \music
  \mark \markup {"\hicazkar"}
  \key c \hicazkar
  \music
  \mark \markup {"\kurdi"}
  \key c \kurdi
  \music
  \mark \markup {"\kurdilihicazkar"}
  \key c \kurdilihicazkar
  \music
  \mark \markup {"\mahur"}
  \key c \mahur
  \music
  \mark \markup {"\nihavend"}
  \key c \nihavend
  \music
  \mark \markup {"\nisabur"}
  \key c \nisabur
  \music
  \mark \markup {"\nisaburek"}
  \key c \nisaburek
  \music
  \mark \markup {"\rast"}
  \key c \rast
  \music
  \mark \markup {"\rengidil"}
  \key c \rengidil
  \music
  \mark \markup {"\saba"}
  \key c \saba
  \music
  \mark \markup {"\segah"}
  \key c \segah
  \music
  \mark \markup {"\sultaniyegah"}
  \key c \sultaniyegah
  \music
  \mark \markup {"\suzidil"}
  \key c \suzidil
  \music
  \mark \markup {"\suznak"}
  \key c \suznak
  \music
  \mark \markup {"\sedaraban"}
  \key c \sedaraban
  \music
  \mark \markup {"\serefnuma"}
  \key c \serefnuma
  \music
  \mark \markup {"\sevkefza"}
  \key c \sevkefza
  \music
  \mark \markup {"\ussak"}
  \key c \ussak
  \music
  \mark \markup {"\zirgule"}
  \key c \zirgule
  \music
}
