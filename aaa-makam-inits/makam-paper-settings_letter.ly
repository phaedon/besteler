\version "2.14.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAKAM MUSIC-PAPER-SETTINGS BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date = #(strftime "%d.%m.%Y" (localtime (current-time)))

#(define (create-page-number-stencil layout props arg)
   (if (eq? (ly:output-def-lookup layout 'print-page-number) #t)
       (interpret-markup layout props arg)
       empty-stencil))

#(define (skip-two-page-numbers layout props arg)
   (if (or (> (chain-assoc-get 'page:page-number props -1)
              (+ (ly:output-def-lookup layout 'first-page-number) 1))
           (eq? (ly:output-def-lookup layout 'print-first-page-number) #t))
       (create-page-number-stencil layout props arg)
       empty-stencil))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Paper Defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\layout {
}

\paper {
  #(define fonts (make-pango-font-tree
                  "Times New Roman"
                  "Times New Roman"
                  "Times New Roman"
                  (/ 18 19)))


%%%%% MARGINS...IMPORTANT FOR MY BOOK, TURN ON FOR letter BOOK!!!
  %#(set-paper-size "letter")
  %#(set-default-paper-size "letter" 'landscape)
  
  %%% a point (pt) is about 0.0138 inch or 0.3515 mm 
  %%% 534.850640114\pt would be 188mm for line-width
  %%% 739.687055477\pt would be 260mm for paper-height
    line-width = 534.00\pt %for Letter
    paper-width = 534.00\pt %for Letter
    paper-height =  700\pt

    %line-width = 188.00\mm %for Letter
    %paper-width = 188.00\mm %for Letter
    %paper-height =  260\mm

    top-margin = 0\mm % 5/mm default
    bottom-margin = 4\mm % 6/mm default
    page-top-space = 0\mm
    before-title-space = 0\mm
    left-margin = 0.0\mm
    right-margin = 0.0\mm
    %head-separation = 4\mm %default is 4\mm
    %foot-separation = 0\mm %default is 4\mm
  

  oddFooterMarkup=##f
  oddHeaderMarkup=##f

  %% First system indent
  noindent = 0\mm
  smallindent = 5\mm
  largeindent = 20\mm
  indent = \noindent

  print-page-number = ##f % default is ##f

  bookTitleMarkup =\markup {
    \column {
      \fill-line {
        \fontsize #6.5
        \fromproperty #'header:titleUC
      }
      \fill-line {
        \fontsize #0.5
        \fromproperty #'header:subtitleText
      }
      \fill-line {
        \column {
          \left-align {
            \fontsize #-1.0
            \fromproperty #'header:usulname
          }
          \left-align {
            \fontsize #-1.0
            \fromproperty #'header:tempopiece
            \fromproperty #'header:musicsource
          }
        }
        \column {
          \right-align {
            \fontsize #-1.0
            \fromproperty #'header:composername
          }
          \right-align {
            \fontsize #-1.0
            \fromproperty #'header:poetname \hspace #0
          }
        }
      }
    }
  }

  oddHeaderMarkup = \markup
  \fill-line
  {
    \line {
      \on-the-fly #not-first-page \fromproperty #'header:titleLC
      \on-the-fly #not-first-page "- p"
      \on-the-fly #not-first-page \fromproperty #'page:page-number-string
      \on-the-fly #skip-two-page-numbers \fromproperty #'page:page-number-string
    }
  }

  evenHeaderMarkup = \markup
  \fill-line
  {
    \line {
      \on-the-fly #skip-two-page-numbers \fromproperty #'page:page-number-string
      \on-the-fly #not-first-page \fromproperty #'header:titleLC "- p" \fromproperty #'page:page-number-string
    }
  }
}

\header {
  tagline = ""
  copyright = ""
}

%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%