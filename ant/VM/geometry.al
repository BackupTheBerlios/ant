
;; constructors and accessors

;; points and vectors

geo_make_polar rad angle := (rad * sind angle, rad * cosd angle);

;; lines and segments

geo_make_line point=(_,_) dir=(_,_)   := (Line, point, dir);

geo_make_segment x y := (Segment, x, y);

geo_line_of_segment (Segment, x, y)   := (Line, x, y - x);

geo_direction (Line, _, d)    := d;
geo_direction (Segment, x, y) := y - x;

geo_intersection (Line, p_1, d_1) (Line, p_2, d_2) :=
  local q in
  begin
    q = p_1 + _ * d_1,
    q = p_2 + _ * d_2,
    q
  end;

;; conics

geo_make_circle centre rad=(r_x, r_y) := (Ellipse,   centre, (r_x, r_y), (~r_y, r_x));
geo_make_ellipse   centre rad_a rad_b := (Ellipse,   centre, rad_a, rad_b);
geo_make_parabola  centre rad_a rad_b := (Parabola,  centre, rad_a, rad_b);
geo_make_hyperbola centre rad_a rad_b := (Hyperbola, centre, rad_a, rad_b);

geo_major_axis (Ellipse,   _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Ellipse,   _, r_a, r_b) := min (abs r_a) (abs r_b);
geo_major_axis (Parabola,  _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Parabola,  _, r_a, r_b) := min (abs r_a) (abs r_b);
geo_major_axis (Hyperbola, _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Hyperbola, _, r_a, r_b) := min (abs r_a) (abs r_b);

geo_eccentricity (Ellipse,   _, r_a, r_b) := sqrt (1 - (r_b * r_b) / (r_a * r_a));
geo_eccentricity (Parabola,  _, r_a, r_b) := 1;
geo_eccentricity (Hyperbola, _, r_a, r_b) := sqrt (1 + (r_b * r_b) / (r_a * r_a));

;; polygons

geo_make_triangle a b c := (Triangle, a, b, c);

geo_make_polygon vertices := (Polygon, vertices);

;; distance:
;;   point point
;;   point line
;;   line  point

geo_distance p_1=(_,_) p_2=(_,_) := geo_length (p_2 - p_1);

geo_distance x=(_,_) (Line, p, dir=(d_x, d_y)) :=
  abs ((~d_y, d_x) * (x - p) / abs dir);

geo_distance (Line, p, dir=(d_x, d_y)) x=(_,_) :=
  abs ((~d_y, d_x) * (x - p) / abs dir);

;; length
;;   point
;;   segment

geo_length p=(_,_) := abs p;

geo_length (Segment, x, y) := geo_length (y - x);

;; angle
;;   vector
;;   line
;;   segment
;;   conic
;;   vector, vector
;;   point, point, point

geo_angle (x,y) :=
  if y > 0 then
    90 - arctand (x / y)
  elseif y < 0 then
    270 - arctand (x / y)
  elseif x >= 0 then
    0
  else
    180
  end;

geo_angle (Line, _, d)             := geo_angle d;
geo_angle (Segment, x, y)          := geo_angle (y - x);
geo_angle (Ellipse,   _, rad_a, _) := geo_angle rad_a;
geo_angle (Parabola,  _, rad_a, _) := geo_angle rad_a;
geo_angle (Hyperbola, _, rad_a, _) := geo_angle rad_a;

geo_angle_between_vectors v_1=(_,_) v_2=(_,_) :=
  geo_angle v_1 - geo_angle v_2;

geo_angle_between_points p_1=(_,_) p_2=(_,_) p_3=(_,_) :=
  geo_angle (p_1 - p_2) - geo_angle (p_3 - p_2);

;; geo_centre

geo_centre (Segment, x, y) := 1/2[x,y];
geo_centre (Ellipse,   c, _, _) := c;
geo_centre (Parabola,  c, _, _) := c;
geo_centre (Hyperbola, c, _, _) := c;

;; geo_point

geo_point s (Line, p, d)    := p + s * d / abs d;
geo_point s (Segment, x, y) := s[x,y];

geo_point s (Ellipse,   c, r_a, r_b) := c + cosd t * r_a + sind t * r_b
                                        where t := 360s end;
geo_point s (Parabola,  c, r_a, r_b) := c + s * r_a + s^2 * r_b;
geo_point s (Hyperbola, c, r_a, r_b) := c - r_a/cosd t + tand t * r_b
                                        where t := 360s end;

;; geo_argument

geo_argument (Line, x, d) p :=
  local s in
  begin
    p = x + s * d / abs d,
    s
  end;
geo_argument (Segment, x, y) p :=
  local s in
  begin
    p = s[x,y],
    s
  end;
geo_argument (Ellipse, c, r_a, r_b) p :=
  geo_angle (c + r_a) c p / 360;
geo_argument (Parabola, c, r_a, r_b) p :=
  local s in
  begin
    p = c + s * r_a + _ * r_b,
    s
  end;
geo_argument (Hyperbola, c, r_a, r_b) p :=
  local s in
  begin
    p = c + _ * r_a + s * r_b,
    arctand s / 360
  end;

;; vim:set ft=al:
