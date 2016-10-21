/// PDF file generation
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynPdfArc;
{
   This file is part of Synopse framework.

   Synopse framework. Copyright (C) 2016 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

 *** BEGIN LICENSE BLOCK *****
 Version: MPL 1.1/GPL 2.0/LGPL 2.1

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with
 the License. You may obtain a copy of the License at
 http://www.mozilla.org/MPL

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 for the specific language governing rights and limitations under the License.

 The Original Code is Synopse framework.

 The Initial Developer of the Original Code is Friedrich Westermann.

 Portions created by the Initial Developer are Copyright (C) 2016
 the Initial Developer. All Rights Reserved.

 Alternatively, the contents of this file may be used under the terms of
 either the GNU General Public License Version 2 or later (the "GPL"), or
 the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 in which case the provisions of the GPL or the LGPL are applicable instead
 of those above. If you wish to allow use of your version of this file only
 under the terms of either the GPL or the LGPL, and not to allow others to
 use your version of this file under the terms of the MPL, indicate your
 decision by deleting the provisions above and replace them with the notice
 and other provisions required by the GPL or the LGPL. If you do not delete
 the provisions above, a recipient may use your version of this file under
 the terms of any one of the MPL, the GPL or the LGPL.

 ***** END LICENSE BLOCK *****

 Version 1.0
 - first public release

}
interface

type
  //  Result types for use in SynPDF
  tcaRes = (caMoveto, caLine, EaRcurve);
  teaDrawtype = record
    res: tcaRes;
    pts: array[0..2] of record x, y: single;
    end; //tPoint2d;
  end;
  teaDrawArray = array of teaDrawtype;
  (*
   function doArc(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer; aCounterClock : boolean; isPieSlice: Boolean; var res: teaDrawArray): Boolean;
    centerx, centery : Center of the Rectangle around the Arc
    W, H : With and Height of the Box
    Sx, Sy, Ex, Ey : Start and Endpoit of the Arc
  *)
function doArc(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer; aClockWise: boolean; arctype: integer; var res:
  teaDrawArray): Boolean;

implementation

uses
  Math;

const
 acArc = 0;
 acArcTo = 1;
 acArcAngle = 2;
 acPie = 3;
 acChoord = 4;
type
  tcoeffArray = array[0..1, 0..3, 0..3] of double;
  tSafetyArray = array[0..3] of double;

const
  twoPi = 2 * PI;
  { (* }
  // coefficients for error estimation
  // while using cubic Bézier curves for approximation
  // 0 < b/a < 1/4
  coeffsLow: tcoeffArray = (((3.85268, -21.229, -0.330434, 0.0127842), (-1.61486, 0.706564, 0.225945, 0.263682),
    (-0.910164, 0.388383, 0.00551445, 0.00671814), (-0.630184, 0.192402, 0.0098871, 0.0102527)),
    ((-0.162211, 9.94329, 0.13723, 0.0124084), (-0.253135, 0.00187735, 0.0230286, 0.01264), (-0.0695069, -0.0437594,
    0.0120636,
    0.0163087), (-0.0328856, -0.00926032, -0.00173573, 0.00527385)));

  // coefficients for error estimation
  // while using cubic Bézier curves for approximation
  // 1/4 <= b/a <= 1
  coeffsHigh: tcoeffArray = (((0.0899116, -19.2349, -4.11711, 0.183362), (0.138148, -1.45804, 1.32044, 1.38474),
    (0.230903, -0.450262, 0.219963, 0.414038), (0.0590565, -0.101062, 0.0430592, 0.0204699)),
    ((0.0164649, 9.89394, 0.0919496, 0.00760802), (0.0191603, -0.0322058, 0.0134667, -0.0825018), (0.0156192,
    -0.017535, 0.00326508,
    -0.228157), (-0.0236752, 0.0405821, -0.0173086, 0.176187)));

  // safety factor to convert the "best" error approximation
  // into a "max bound" error
  safety3: tSafetyArray = (0.001, 4.98, 0.207, 0.0067);

type
  tCalcArc = class
  private
    //  center of the ellipse.
    fcx, fcy: double;
    // Semi-major axis.
    faRad, fbRad: double;
    //  Start End angle of the arc.
    feta1, feta2: double;

    //start point.
    fx1, fy1: double;
    // EndPoint
    fx2, fy2: double;
    // leftmost point of the arc.
    fxLeft, fyUp: double;

    fwidth, //  Horizontal width of the arc.
    fheight: double; //  Vertical height of the arc.

    //Indicator for center to endpoints line inclusion.
    fArctype: integer;

    function estimateError(etaA, etaB: double): double;

  public
    constructor CreateArcI(clockwise: Boolean; centerx, centery, W, H, Sx, Sy, Ex, Ey: integer; arctype: integer);
    procedure buildPathIterator(var res: teaDrawArray);
  end;

function tCalcArc.estimateError(etaA, etaB: double): double;
var
  c0: double;
  c1: double;
  coeffs: tcoeffArray;
  cos2: double;
  cos4: double;
  cos6: double;
  dEta: double;
  eta: double;
  safety: tSafetyArray;
  x: double;

  function rationalFunction(x: double; c: array of double): double;
  begin
    assert(high(c) >= 3);
    result := (x * (x * c[0] + c[1]) + c[2]) / (x + c[3]);
  end;

begin
  eta := 0.5 * (etaA + etaB);
  x := fbRad / faRad;
  dEta := etaB - etaA;
  cos2 := cos(2 * eta);
  cos4 := cos(4 * eta);
  cos6 := cos(6 * eta);
  // select the right coeficients set according to degree and b/a
  if (x < 0.25) then
    coeffs := coeffsLow
  else
    coeffs := coeffsHigh;
  safety := safety3;

  c0 := rationalFunction(x, coeffs[0][0]) + cos2 * rationalFunction(x, coeffs[0][1]) + cos4 * rationalFunction(x,
    coeffs[0][2]) + cos6 *
    rationalFunction(x, coeffs[0][3]);

  c1 := rationalFunction(x, coeffs[1][0]) + cos2 * rationalFunction(x, coeffs[1][1]) + cos4 * rationalFunction(x,
    coeffs[1][2]) + cos6 *
    rationalFunction(x, coeffs[1][3]);
  result := rationalFunction(x, safety) * faRad * exp(c0 + c1 * dEta);
end;

(*  Build an approximation of the instance outline.
   return a array with path information in res
*)

procedure tCalcArc.buildPathIterator(var res: teaDrawArray);
var
  alpha: double;
  found: Boolean;
  n: integer;
  dEta, etaB, etaA: double;

  cosEtaB, sinEtaB, aCosEtaB, bSinEtaB, aSinEtaB, bCosEtaB, xB, yB, xBDot, yBDot: double;
  I: integer;
  t: double;
  xA: double;
  xADot: double;
  yA: double;
  yADot: double;

  size: integer; // Size for result Array
  resindex: integer; // Index var for result Array
  emptyres: teaDrawtype;
  lstartx, lstarty : double;  // Start From

const
  defaultFlatness = 0.5; // half a pixel

begin
  setlength(res, 0);
  fillchar(emptyres, sizeof(emptyres), 0);
  // find the number of Bézier curves needed
  found := false;
  n := 1;

  while ((not found) and (n < 1024)) do begin
    dEta := (feta2 - feta1) / n;
    if (dEta <= 0.5 * PI) then begin
      etaB := feta1;
      found := true;
      for I := 0 to n - 1 do begin
        etaA := etaB;
        etaB := etaB + dEta;
        found := (estimateError(etaA, etaB) <= defaultFlatness);
        if not found then
          break;
      end;
    end;
    // if not found then
    n := n shl 1;
  end;

  dEta := (feta2 - feta1) / n;
  etaB := feta1;
  cosEtaB := cos(etaB);
  sinEtaB := sin(etaB);
  aCosEtaB := faRad * cosEtaB;
  bSinEtaB := fbRad * sinEtaB;
  aSinEtaB := faRad * sinEtaB;
  bCosEtaB := fbRad * cosEtaB;
  xB := fcx + aCosEtaB;
  yB := fcy + bSinEtaB;
  xBDot := -aSinEtaB;
  yBDot := +bCosEtaB;
  lstartx := xB;
  lstarty := yB;
  // calculate and reserve Space for the result
  size := n; // Res Size
  case fArctype of
   acArc :  inc(size,1); // Res Size n+1 because first move;
   acArcTo :inc(size,1); // Res Size n+1 because first Line;
   acArcAngle : inc(size,1); // Res Size n+1 because first move;
   acPie : inc(size, 3); // for first and last Line if wie want a pie
   acChoord : inc(size,2);
  end;

  setlength(res, size);
  resindex := 0;
  case fArctype of
   acArc : begin   // Start with moveto
             res[resindex].res := caMoveto;
             res[resindex].pts[0].x := xB;
             res[resindex].pts[0].y := yB;
             inc(resindex);
           end;
   acArcTo : begin   // Start with moveto
             res[resindex].res := caMoveto;
             res[resindex].pts[0].x := xB;
             res[resindex].pts[0].y := yB;
             inc(resindex);
           end;
   acArcAngle :;
   acPie : begin
    res[resindex] := emptyres;
    res[resindex].res := caMoveto;
    res[resindex].pts[0].x := fcx;
    res[resindex].pts[0].y := fcy;
    inc(resindex);
    res[resindex] := emptyres;
    res[resindex].res := caLine;
    res[resindex].pts[0].x := xB;
    res[resindex].pts[0].y := yB;
    inc(resindex);
           end;
   acChoord : begin
                res[resindex] := emptyres;
    res[resindex].res := caMoveto;
    res[resindex].pts[0].x := xB;
    res[resindex].pts[0].y := yB;
    inc(resindex);
              end;
  end;

  t := tan(0.5 * dEta);
  alpha := sin(dEta) * (sqrt(4 + 3 * t * t) - 1) / 3;

  for I := 0 to n - 1 do begin
    xA := xB;
    yA := yB;
    xADot := xBDot;
    yADot := yBDot;
    etaB := etaB + dEta;
    cosEtaB := cos(etaB);
    sinEtaB := sin(etaB);
    aCosEtaB := faRad * cosEtaB;
    bSinEtaB := fbRad * sinEtaB;
    aSinEtaB := faRad * sinEtaB;
    bCosEtaB := fbRad * cosEtaB;
    xB := fcx + aCosEtaB;
    yB := fcy + bSinEtaB;
    xBDot := -aSinEtaB;
    yBDot := bCosEtaB;
    res[resindex] := emptyres;
    res[resindex].res := EaRcurve;
    res[resindex].pts[0].x := (xA + alpha * xADot);
    res[resindex].pts[0].y := (yA + alpha * yADot);
    res[resindex].pts[1].x := (xB - alpha * xBDot);
    res[resindex].pts[1].y := (yB - alpha * yBDot);
    res[resindex].pts[2].x := xB;
    res[resindex].pts[2].y := yB;
    inc(resindex);
  end; // Loop

  if fArctype = acPie then begin
    res[resindex] := emptyres;
    res[resindex].res := caLine;
    res[resindex].pts[0].x := fcx;
    res[resindex].pts[0].y := fcy;
  end
  else
  if fArctype = acChoord then begin
    res[resindex] := emptyres;
    res[resindex].res := caLine;
    res[resindex].pts[0].x := lstartx;
    res[resindex].pts[0].y := lstarty;
  end;
end;

constructor tCalcArc.CreateArcI(clockwise: Boolean; centerx, centery, W, H, Sx, Sy, Ex, Ey: integer; arctype: integer);
  var lambda1, lambda2 : double;
begin
  inherited create;
  fcx := centerx;
  fcy := centery;
  faRad := (W) / 2;
  fbRad := H / 2;
  fArctype := arctype;
  // Calculate Rotation at Start and EndPoint
  if clockwise then begin
    lambda1 := ArcTan2(Sy - fcy, Sx - fcx);
    lambda2 := ArcTan2(Ey - fcy, Ex - fcx);
  end else begin
    lambda2 := ArcTan2(Sy - fcy, Sx - fcx);
    lambda1 := ArcTan2(Ey - fcy, Ex - fcx);
  end;
    feta1 := ArcTan2(sin(lambda1) / fbRad,
      cos(lambda1) / faRad);
   feta2 := ArcTan2(sin(lambda2) / fbRad,
      cos(lambda2) / faRad);
  // make sure we have eta1 <= eta2 <= eta1 + 2 PI
  feta2 := feta2 - (twoPi * floor((feta2 - feta1) / twoPi));
  // the preceding correction fails if we have exactly et2 - eta1 = 2 PI
  // it reduces the interval to zero length
  if SameValue(feta1, feta2) then
    feta2 := feta2 + twoPi;

  // start point
  fx1 := fcx + (faRad * cos(feta1));
  fy1 := fcy + (fbRad * sin(feta1));

  // end point
  fx2 := fcx + (faRad * cos(feta2));
  fy2 := fcy + (fbRad * sin(feta2));
  // Dimensions
  fxLeft := min(fx1, fx2);
  fyUp := min(fy1, fy2);
  fwidth := max(fx1, fx2) - fxLeft;
  fheight := max(fy1, fy2) - fyUp;
end;

function doArc(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer; aClockWise: boolean; arctype: integer; var res: teaDrawArray): Boolean;
var
  test: tCalcArc;
begin
  setlength(res, 0);
  test := tCalcArc.CreateArcI(aClockWise, centerx, centery, W, H, Sx, Sy, Ex, Ey, arctype);
  test.buildPathIterator(res);
  test.free;
  result := length(res) > 1;
end;

end.

