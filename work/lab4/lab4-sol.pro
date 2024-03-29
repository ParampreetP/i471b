f(1,2,z) = f(_,_, X).
head(a, tail(z, B), Y) = head(_, tail(X, _), _). 
cons(a, cons(b, cons(c, z))) = cons(_, cons(_, cons(_, X))).


% proper listers are [a, b], and [a|[b]]
car(X, [X|R]).
cddr(X,[A, X|B, X|C]).
caddr(X,[A,B,X,C]). 
cdar(X, [[A|X], B, C]).


quadratic_roots(A, B, C, Z) :-
  Discr is sqrt(B*B - 4*A*C),
  Z is (-B + Discr)/(2 * A).
  
quadratic_roots(A, B, C, Z) :-
  Discr is sqrt(B*B - 4*A*C),
  Z is (-B - Discr)/(2 * A).
  
quadratic_roots2(A, B, C, Z) :-
  Discr is sqrt(B*B - 4*A*C),
  quadratic_roots2(Discr, A, B, C, Z).

quadratic_roots2(Discr, A, B, _C, Z) :-
  Z is (-B + Discr)/(2 * A).
  
quadratic_roots2(Discr, A, B, _C, Z) :-
  Z is (-B - Discr)/(2 * A).

sum_lengths([], 0).
sum_lengths([X|Xs], Z):-
  length(X, LX),
  sum_lengths(Xs, XsLen),
  Z is LX + XsLen.


sum_areas([], 0).
sum_areas([Shape|Shapes], Sum):-
  area(Shape, ShapeArea),
  sum_areas(Shapes, ShapesArea),
  Sum is ShapeArea + ShapesArea.

area(rectangle(W, H), Area):-
  Area is W*H.
area(square(W), Area):-
  Area is W*W.
area(circle(R), Area):-
  Area is pi*R*R.
