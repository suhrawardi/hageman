module Scales (
    scale,
    otherScales
  ) where

import Euterpea
import HSoM


scale :: Int -> [PitchClass]
scale 1 = [C, Cs,        E,    Fs, G,     A, As   ]
scale 2 = [C,     D,     E,    Fs, G, Gs,    As   ]
scale 3 = [C,     D,     E, F,     G, Gs,        B]
scale 4 = [C,        Ds, E,    Fs, G,     A,     B]
scale 5 = [C,     D,     E, F,     G,     A,     B]
scale 6 = [C, Cs,        E, F, Fs,    Gs,        B]
scale 7 = [C, Cs,    Ds,    F, Fs,    Gs,    As   ]
scale 8 = [C,     D,     E, F, Fs,    Gs,    As   ]
scale 9 = [C, Cs,           F,     G, Gs          ]
scale _ = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]


otherScales :: [String]
otherScales = ["None",
               "Mela Ramapriya",
               "Mela Rhisabhapriya",
               "Mela Sarasangi",
               "Mela Kosalam",
               "Raga Bageshri",
               "Persian",
               "Arabic",
               "Arabian",
               "Balinese Pelog"]
