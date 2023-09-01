debug tallying ([] [('a1 -> 'a2, 'a0) ('a4, 'a2) (42, 'a4) ('a3 & Int, 'a4) ('a3 & Int, 'a5) (Any -> Bool, 'a5 -> 'a6) ('a6, Bool) ('a1, 'a3)]);;
debug subtype ((Empty -> 42) (Any\Int -> 42));;


C1: ($1 -> $2) ≤ $0
C2: $4 ≤ $2
C3: 42 ≤ $4
C5: $3 ∩ int ≤ $4
C6: $3 ∩ int ≤ $5
C7: (any -> bool) ≤ ($5 -> $6)
C8: $6 ≤ bool
C9: $1 ≤ $3

Normalized constraint sets (NCi, ordered by variable ordering, implicit bounds added):

with:
norm( (any -> bool) ∩ ! ($5 -> $6) ) = [[$5 ≤ ∅], [bool ≤ $6]]


NC1: ($1 -> $2) ≤ $0 ≤ any
NC2:          ∅ ≤ $1 ≤ $3
NC3:         $4 ≤ $2 ≤ any
NC4:          ∅ ≤ $3 ≤ $4 U !int
NC5:          ∅ ≤ $3 ≤ $5 U !int
NC6:         42 ≤ $4 ≤ any
NC7:          ∅ ≤ $6 ≤ bool
NC8: [∅ ≤ $5 ≤ ∅], [bool ≤ $6 ≤ any]

meet over all normalized constraint sets produces two possible solutions so far (because of NC8, phase 1 of tally)


Cs1:

($1 -> $2) ≤ $0 ≤ any
         ∅ ≤ $1 ≤ $3
        $4 ≤ $2 ≤ any
         ∅ ≤ $3 ≤ $4 U !int
         ∅ ≤ $3 ≤ $5 U !int
        42 ≤ $4 ≤ any
         ∅ ≤ $5 ≤ ∅
         ∅ ≤ $6 ≤ bool


Cs2:

($1 -> $2) ≤ $0 ≤ any
         ∅ ≤ $1 ≤ $3
        $4 ≤ $2 ≤ any
         ∅ ≤ $3 ≤ $4 U !int
         ∅ ≤ $3 ≤ $5 U !int
        42 ≤ $4 ≤ any
         ∅ ≤ $6 ≤ bool
      bool ≤ $6 ≤ any

Merge and saturate Cs1 and Cs2: MCs1 and MCs2

with

($4 U !int) ∩ ($5 U !int) = ($4 ∩ $5) U ($4 ∩ !int) U (!int) U (!int ∩ $5) = ($4 ∩ $5) U (!int)

MCs1:


($1 -> $2) ≤ $0 ≤ any
         ∅ ≤ $1 ≤ $3
        $4 ≤ $2 ≤ any
         ∅ ≤ $3 ≤ ($4 ∩ $5) U !int
        42 ≤ $4 ≤ any
         ∅ ≤ $5 ≤ ∅
         ∅ ≤ $6 ≤ bool

MCs2 (with norm(bool ∩ !bool) = [[]]):

($1 -> $2) ≤ $0 ≤ any
         ∅ ≤ $1 ≤ $3
        $4 ≤ $2 ≤ any
         ∅ ≤ $3 ≤ ($4 ∩ $5) U !int
        42 ≤ $4 ≤ any
      bool ≤ $6 ≤ bool

Solve both sets of equations S1 and S2:

S1: 

$0 = ($1 -> $2) U β0
$1 = β1 ∩ $3
$2 = $4 U β2
$3 = β3 ∩ (($4 ∩ $5) U !int)
$4 = 42 U β4
$5 = ∅
$6 = β6 ∩ bool

S2:

$0 = ($1 -> $2) U β0
$1 = β1 ∩ $3
$2 = $4 U β2
$3 = β3 ∩ (($4 ∩ $5) U !int)
$4 = 42 U β4
$6 = bool


Sol1:
$6 = β6 ∩ bool
$5 = ∅
$4 = 42 U β4
$3 = β3 ∩ !int
$2 = 42 U β4 U β2
$1 = β1 ∩ β3 ∩ !int
$0 = (β1 ∩ β3 ∩ !int -> 42 U β4 U β2) U β0


Cleaned Sol1:
$6 = ∅
$5 = ∅
$4 = 42
$3 = ∅
$2 = 42
$1 = ∅
$0 = !int -> 42

Sol2:
$6 = bool
$4 = 42 U β4
$3 = β3 ∩ (((42 U β4) ∩ β5) U !int)
$2 = 42 U β4 U β2
$1 = β1 ∩ β3 ∩ (((42 U β4) ∩ β5) U !int)
$0 = ((β1 ∩ β3 ∩ (((42 U β4) ∩ β5) U !int)) -> 42 U β4 U β2) U β0

Cleaned Sol2:
$6 = bool
$4 = 42
$3 = ∅
$2 = 42
$1 = ∅
$0 = ((42 U β4) U !int) -> 42 U β4

$6 ~~> bool
$5 ~~> int
$4 ~~> int
$3 ~~> any
$2 ~~> int
$1 ~~> any
$0 ~~> (any -> int)

Result:
Sol1
 'a6:=Bool & 'a6a6; 
 'a5:=Empty; 
 'a4:=42 | 'a4a4 & 'a2a2; 
 'a3:='a3a3 \ Int; 
 'a2:=42 | 'a2a2; 
 'a1:=('a1a1 & 'a3a3) \ Int; 
 'a0:=(('a1a1 & 'a3a3) \ Int -> 42 | 'a2a2) | 'a0a0

Cleaned
'a6:=Empty; 
'a5:=Empty; 
'a4:=42; 
'a3:=Empty; 
'a2:=42; 
'a1:=Empty; 
'a0:=!Int -> 42;


Sol2
 'a6:=Bool;
 'a4:=42 | 'a4a4 & 'a2a2; 
 'a3:='a3a3 \ Int | Int & 'a5 & 'a3a3 & 'a4a4 & 'a2a2 | 42 & 'a5 & 'a3a3; 
 'a2:=42 | 'a2a2;
 'a1:=('a1a1 & 'a3a3) \ Int | Int & 'a5 & 'a1a1 & 'a3a3 & 'a4a4 & 'a2a2 | 42 & 'a5 & 'a1a1 & 'a3a3;
 'a0:=(('a1a1 & 'a3a3) \ Int | Int & 'a5 & 'a1a1 & 'a3a3 & 'a4a4 & 'a2a2 | 42 & 'a5 & 'a1a1 & 'a3a3 -> 42 | 'a2a2) | 'a0a0

Cleaned 
'a6:=Bool; 
'a4:=42; 
'a3:=Empty; 
'a2:=42; 
'a1:=Empty; 
'a0:=Any \ ( *--41 | 43--*) | Int & 'a2a2 -> 42 | 'a2a2




