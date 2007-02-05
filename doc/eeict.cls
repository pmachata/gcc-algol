%%
%% This is file `eeict.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% eeict.dtx  (with options: `class')
%% 
%% 

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{eeict}%
    [2002/11/11 Dokumentarni trida pro prispevky do sborniku EEICT]
\usepackage[T1]{fontenc}
\LoadClass[a4paper,onecolumn,12pt]{article}
\setlength{\hoffset}{-0.7cm}
\setlength{\voffset}{-1.5cm}
\setlength{\textheight}{23.0cm}
\setlength{\textwidth}{15.2cm}
\usepackage{mathptm}
\renewcommand\rmdefault{ptm}
\renewcommand\ttdefault{pcr}

\ifx\pdfoutput\undefined  % nejedeme pod pdftexem
  \usepackage[czech,english]{babel}
  \main@language{english}
  \usepackage[latin2]{inputenc}
  \usepackage{graphics}
  \usepackage{epsfig}
\else % je to pdftex !
  \usepackage[czech,english]{babel}
  \main@language{english}
  \usepackage[latin2]{inputenc}
  \usepackage[pdftex]{graphicx}
\fi
\pagestyle{empty}
\setlength{\parindent}{1cm}
\let\@afterindentfalse\@afterindenttrue
\@afterindenttrue
\newcommand\supervisor[1]{\def\@skolitel{Supervised by: #1}}
\renewcommand\maketitle{\begin{center}
  \vspace*{8mm}
  {\Large \textbf{\MakeUppercase{\@title}} }\\
  \vspace{15mm}
  \@author \\
[5mm]
  \@skolitel
  \vspace{.6cm}
\end{center}}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\normalsize\bfseries\MakeUppercase}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries\MakeUppercase}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\MakeUppercase}}
\renewcommand\abstract[1]{
  \section*{\abstractname}
  {#1}
}
  \def\sktitle{Supervised by: }
  \def\@skolitel{the work has no suppervisor}
\def\uv#1{\iflanguage{english}{``#1''}%
                              {\quotedblbase #1\textquotedblleft}}%
\catcode`\"=\active
\def"{\bgroup
    \iflanguage{english}%
        {``\def"{''\egroup}}%
        {\quotedblbase\def"{\textquotedblleft\egroup}}}
\endinput
%%
%% End of file `eeict.cls'.
