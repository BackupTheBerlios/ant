
;; operators

declare_infix_left  9 !!;
declare_infix_right 9 o;
declare_infix_right 5 ++;
declare_infix_left  1 >>;
declare_infix_right 0 $;

;; the usual examples faculty and fibonacci

;; fib 0 := 1;
;; fib 1 := 1;
;; fib n := fib (n-2) + fib (n-1);

fac n & n <= 1 := 1;
fac n          := n * fac (n-1);

fib n := iter 0 1 n
where
  iter i k n := if n <= 0 then
                  i
                else
                  iter k (i + k) (n - 1)
                end
end;

;; list functions

;; head : [a] -> a
;; tail : [a] -> [a]
;; last : [a] -> a
;; init : [a] -> [a]

head [x:_]  := x;
tail [_:xs] := xs;

last [x]    := x;
last [_:xs] := last xs;

init [x]    := [];
init [x:xs] := [x : init xs];

;; null        : [a] -> bool
;; list_length : [a] -> int

null []    := True;
null [_:_] := False;

list_length []     := 0;
list_length [x:xs] := 1 + list_length xs;

;; reverse : [a] -> [a]

reverse list := rev_iter list []
where
  rev_iter []     res := res;
  rev_iter [x:xs] res := rev_iter xs [x:res];
end;

;; (!!) : [a] -> int -> a

[x:_]  !! 0         := x;
[_:xs] !! n & n > 0 := xs !! (n - 1);
_      !! _         := error "!!: invalid index";

;; map    : (a -> b) -> [a] -> [b]
;; filter : (a -> bool) -> [a] -> [a]

map _ []     := [];
map f [x:xs] := [f x : map f xs];

filter _ []           := [];
filter p [x:xs] & p x := [x : filter p xs];
filter p [x:xs]       := filter p xs;

;; foldl   : (a -> b -> a) -> a -> [b] -> a
;; foldl_1 : (a -> a -> a) -> [a] -> a
;; foldr   : (a -> b -> b) -> b -> [a] -> b
;; foldr_1 : (a -> a -> a) -> [a] -> a

foldl _ e []     := e;
foldl f e [x:xs] := foldl f (f e x) xs;

foldl_1 f [x:xs] := foldl f x xs;

foldr _ e []     := e;
foldr f e [x:xs] := f x (foldr f e xs);

foldr_1 _ [x]    := x;
foldr_1 f [x:xs] := f x (foldr_1 f xs);

;; (++)   : [a] -> [a] -> [a]
;; concat : [[a]] -> [a]

[]     ++ ys := ys;
[x:xs] ++ ys := [x : xs ++ ys];

concat := foldr (++) [];

;; scanl   : (a -> b -> a) -> a -> [b] -> [a]
;; scanl_1 : (a -> a -> a) -> [a] -> [a]
;; scanr   : (a -> b -> b) -> b -> [a] -> [b]
;; scanr_1 : (a -> a -> a) -> [a] -> [a]

scanl f q []     := [q];
scanl f q [x:xs] := [q : scanl f (f q x) xs];

scanl_1 _ []     := [];
scanl_1 f [x:xs] := scanl f x xs;

scanr f q []     := [q];
scanr f q [x:xs] := [f x p : ps]
                    where ps=[p:_] := scanr f q xs end;

scanr_1 f []     := [];
scanr_1 f [x]    := [x];
scanr_1 f [x:xs] := [f x q : qs]
                    where qs=[q:_] := scanr_1 f xs end;

;; iterate   : (a -> a) -> a -> [a]
;; repeat    : a -> [a]
;; replicate : int -> a -> [a]
;; cycle     : [a] -> [a]

iterate f x   := [x : iterate f (f x)];

repeat x      := xs where xs := [x:xs] end;

replicate n x := take n (repeat x);

cycle []      := error "cycle: empty list";
cycle xs      := ys where ys := xs ++ ys end;

;; take       : int -> [a] -> [a]
;; drop       : int -> [a] -> [a]
;; split_at   : int -> [a] -> ([a], [a])
;; take_while : (a -> bool) -> [a] -> [a]
;; drop_while : (a -> bool) -> [a] -> [a]
;; span       : (a -> bool) -> [a] -> ([a], [a])
;; break      : (a -> bool) -> [a] -> ([a], [a])

take _ []              := [];
take n [x:xs] & n <= 0 := [];
take n [x:xs]          := [x : take (n-1) xs];

drop _ []              := [];
drop n [x:xs] & n <= 0 := [x:xs];
drop n [x:xs]          := drop (n-1) xs;

split_at _ []              := ([], []);
split_at n ls     & n <= 0 := ([], ls);
split_at n [x:xs]          := ([x:ys], zs) where (ys, zs) := split_at (n-1) xs end;

take_while _ []           := [];
take_while p [x:xs] & p x := [x : take_while p xs];
take_while _ _            := [];

drop_while _ []           := [];
drop_while p [x:xs] & p x := drop_while p xs;
drop_while _ ys           := ys;

span p []           := ([], []);
span p [x:xs] & p x := ([x:ys], zs) where (ys, zs) := span p xs end;
scan p ls           := ([], ls);

break p []             := ([], []);
break p ls=[x:_] & p x := ([], ls);
break p [x:xs]         := ([x:ys], zs) where (ys, zs) := break p xs end;

;; lines   : string -> [string]
;; words   : string -> [string]
;; unlines : [string] -> string
;; unwords : [string] -> string

lines ""   := [];
lines s    := local (l, r) := break ('\n' ==) s;
              [l : match r with
                   { []    := []
                   | [_:t] := lines t
                   }];

;;words s    := match drop_while is_space s with
;;              { "" := []
;;              | x  := [w : words y]
;;                      where (w, y) := break is_space x end
;;              };

unlines []      := [];
unlines [l:ls]  := l ++ ['\n' : unlines ls];

unwords []     := "";
unwords [w]    := w;
unwords [w:ws] := w ++ [' ' : unwords ws];

;; and : [bool] -> bool
;; or  : [bool] -> bool
;; any : (a -> bool) -> [a] -> bool
;; all : (a -> bool) -> [a] -> bool

and        := foldr (&&) True;
or         := foldr (||) False;
any p      := or  o map p;
all p      := and o map p;

;; elem     : a -> [a] -> bool
;; not_elem : a -> [a] -> bool

elem       := any o (==);
not_elem   := all o (<>);

;; lookup : a -> [(a,b)] -> option b

lookup k []                   := None;
lookup k [(x,y) : _] & k == x := y;
lookup k [_ : xys]            := lookup k xys;

;; sum     : [a] -> a
;; product : [a] -> a
;; maximum : [a] -> a
;; minimum : [a] -> a

sum     := foldl (+) 0;
product := foldl (*) 1;
maximum := foldl_1 max;
minimum := foldl_1 min;

;; concat_map : (a -> [b]) -> [a] -> [b]

concat_map f := concat o map f;

;; zip        : [a] -> [b] -> [(a, b)]
;; zip3       : [a] -> [b] -> [c] -> [(a, b, c)]
;; zip_with   : (a->b->c) -> [a]->[b]->[c]
;; zip_with_3 : (a->b->c->d) -> [a]->[b]->[c]->[d]
;; unzip      : [(a,b)] -> ([a],[b])
;; unzip_3    : [(a,b,c)] -> ([a],[b],[c])

zip  := zip_with   { a b   := (a, b)    };
zip3 := zip_with_3 { a b c := (a, b, c) };

zip_with z [a:as] [b:bs] := [z a b : zip_with z as bs];
zip_with _ _      _      := [];

zip_with_3 z [a:as] [b:bs] [c:cs] := [z a b c : zip_with_3 z as bs cs];
zip_with_3 _ _ _ _                := [];

unzip   := foldr { (a,b)   (as,bs)    := ([a:as], [b:bs])         } ([],[]);
unzip_3 := foldr { (a,b,c) (as,bs,cs) := ([a:as], [b:bs], [c:cs]) } ([],[],[]);

;; integer lists

enum_from x           := [x : enum_from (x + 1)];
enum_from_step x step := [x : enum_from (x + step)];
enum_from_to min max  & min > max := [];
enum_from_to min max              := [min : enum_from_to (min+1) max];
enum_from_to_step min max step & min > max := [];
enum_from_to_step min max step             := [min : enum_from_to_step (min+step) max step];

;; arithmetic

sign x & x >  0 := 1;
sign x & x == 0 := 0;
sign x & x <  0 := ~1;

gcd 0 0 := error "gcd: gcd 0 0 is undefined";
gcd x y := gcd_iter (abs x) (abs y)
           where
             gcd_iter x 0 := x;
             gcd_iter x y := gcd_iter y (x mod y);
           end;

lcm _ 0 := 0;
lcm 0 _ := 0;
lcm x y := abs (quot x (gcd x y) * y);

;; misc

;; fst : (a, b) -> a
;; snd : (a, b) -> b

fst (x, _) := x;
snd (_, y) := y;

;; curry   : ((a, b) -> c) -> (a -> b -> c)
;; uncurry : (a -> b -> c) -> ((a, b) -> c)

curry   f x y    := f (x, y);
uncurry f (x, y) := f x y;

;; id    : a -> a
;; const : a -> b -> a

id    x   := x;
const k _ := k;

;; ($)  : (a -> b) -> a -> b
;; (o)  : (b -> c) -> (a -> b) -> (a -> c)
;; (>>) : (a -> b) -> (b -> c) -> (a -> c)
;; flip : (a -> b -> c) -> b -> a -> c

f $ x := f x;

f o g  := { x := f (g x) };
f >> g := { x := g (f x) };

;; (>>) f g x := g (f x);

flip f x y := f y x;

;; until : (a -> bool) -> (a -> a) -> a -> a

until p f x & p x := x;
until p f x       := until p f (f x);

;;path []     := [];
;;path [x:xs] := iter (start_path x) xs
;;where
;;  start_path (Point,(x,y)) := p_make_path x y;
;;  start_path _             := error "path specification must start with a point!";
;;
;;  conv_splines [] := [];
;;  conv_splines [(x_0,y_0,x_1,y_1,x_2,y_2,x_3,y_3) : zs] :=
;;    [((x_0,y_0),(x_1,y_1),(x_2,y_2),(x_3,y_3)) : conv_splines zs];
;;
;;  iter spec []                  := conv_splines (p_close_path spec False);
;;  iter spec [Cycle]             := conv_splines (p_close_path spec True);
;;  iter spec [(InDir, a)   : zs] := p_add_in_dir spec a;
;;  iter spec [(InCurl, c)  : zs] := p_add_in_curl spec c;
;;  iter spec [(InTens, t)  : zs] := p_add_in_tension spec t;
;;  iter spec [(OutDir, a)  : zs] := p_add_out_dir spec a;
;;  iter spec [(OutCurl, c) : zs] := p_add_out_curl spec c;
;;  iter spec [(OutTens, t) : zs] := p_add_out_tension spec t;
;;  iter spec [(Point, (x, y))                   : zs] := p_add_point spec x y;
;;  iter spec [(Control, (x_1, y_1), (x_2, y_2)) : zs] := p_add_control_points spec x_1 y_1 x_2 y_2;
;;end;

;; test := foldl (+) 1 (map (mod 2) (map (+1) (enum_from_to 0 1000000)));

;; vim:set ft=al:
