/// efficient double to text conversion using the GRISU-1 algorithm
// - as a complement to SynCommons, which tended to increase too much
// - licensed under a MPL/GPL/LGPL tri-license; version 1.18

{
    Implement 64-bit floating point (double) to ASCII conversion using the
    GRISU-1 efficient algorithm.

    Original Code in flt_core.inc flt_conv.inc flt_pack.inc from FPC RTL.
    Copyright (C) 2013 by Max Nazhalov
    Licenced with LGPL 2 with the linking exception.
    If you don't agree with these License terms, disable this feature
    by undefining DOUBLETOSHORT_USEGRISU in Synopse.inc

    GRISU Original Algorithm
    Copyright (c) 2009 Florian Loitsch

    We extracted a double-to-ascii only cut-down version of those files,
    and made a huge refactoring to reach the best performance, especially tuning
    the x86_64 target with some dedicated asm.

    Warning: this unit is not fully debuggged on Delphi - it gives incorrect
    answers - at least for the regression tests. Also note that UInt64/QWord
    support was broken on oldest versions of the Delphi compiler.
    Therefore, SynCommons currently use this faster d2a() only on FPC x86_64.

}

const
  // special text returned if the double is not a number
  C_STR_INF: string[3] = 'Inf';
  C_STR_QNAN: string[3] = 'Nan';

  // min_width parameter special value, as used internally by FPC for str(d,s)
  // - d2a() only accept C_NO_MIN_WIDTH or 0 for min_width: space trailing has
  // been removed in this cut-down version
  C_NO_MIN_WIDTH = -32767;


/// main function to convert 64-bit double into a shortstring, stored in str
// - currently, SynCommnon only set min_width=0 (for DoubleToShortNoExp to avoid
// any scientific notation ) or min_width=C_NO_MIN_WIDTH (for DoubleToShort to
// force the scientific notation when the double cannot be represented as
// a simple fractinal number)
procedure d2a(min_width, frac_digits: integer; const v: double; str: PAnsiChar);


  // Controls printing of NaN-sign.
  // Undefine to print NaN sign during float->ASCII conversion.
  // IEEE does not interpret the sign of a NaN, so leave it defined.
  {$define GRISU1_F2A_NAN_SIGNLESS}

  // Controls rounding of generated digits when formatting with narrowed
  // width (either fixed or exponential notation).
  // Traditionally, FPC and BP7/Delphi use "roundTiesToAway" mode.
  // Undefine to use "roundTiesToEven" approach.
  {$define GRISU1_F2A_HALF_ROUNDUP}

  // This one is a hack against Grusu sub-optimality.
  // It may be used only strictly together with GRISU1_F2A_HALF_ROUNDUP.
  // It does not violate most general rules due to the fact that it is
  // applicable only when formatting with narrowed width, where the fine
  // view is more desirable, and the precision is already lost, so it can
  // be used in general-purpose applications.
  // Refer to its implementation.
  {$define GRISU1_F2A_AGRESSIVE_ROUNDUP} // Defining this fixes several tests.

  // Undefine to enable SNaN support.
  // Note: IEEE [754-2008, page 31] requires (1) to recognize "SNaN" during
  // ASCII->float, and (2) to generate the "invalid FP operation" exception
  // either when SNaN is printed as "NaN", or "SNaN" is evaluated to QNaN,
  // so it would be preferable to undefine these settings,
  // but the FPC RTL is not ready for this right now..
  {$define GRISU1_F2A_NO_SNAN}

  {$ifndef FPC}

  // those functions are intrinsics with FPC :)

  function BSRdword(c: cardinal): cardinal;
  asm
      {$ifdef CPU64}
      .noframe
      mov eax, c
      {$endif}
      bsr eax, eax
  end;

  function BSRqword(const q: qword): cardinal;
  asm
      {$ifdef CPU32}
      bsr eax, [esp + 8]
      jz  @1
      add eax, 32
      ret
  @1: bsr eax, [esp + 4]
      jnz @2
      mov eax, 255
  @2: {$else}
      .noframe
      mov rax, q
      bsr rax, rax
      {$endif}
  end;


  {$endif FPC}

  const
    // TFloatFormatProfile for double
    nDig_mantissa = 17;
    nDig_exp10 = 3;

  type
      // "Do-It-Yourself Floating Point" structures
    TDIY_FP = record
      f: qword;
      e: integer;
    end;

    TDIY_FP_Power_of_10 = record
      c: TDIY_FP;
      e10: integer;
    end;
    PDIY_FP_Power_of_10 = ^TDIY_FP_Power_of_10;

  const
    ROUNDER = $80000000;

  {$ifdef CPUINTEL} // our faster version using 128-bit x86_64 multiplication

  procedure diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean;
    out result: TDIY_FP); {$ifdef HASINLINE} inline; {$endif}
  var
    p: THash128Rec;
  begin
    mul64x64(x.f, y.f, p); // fast x86_64 / i386 asm
    if (p.c1 and ROUNDER) <>  0 then
      inc(p.h);
    result.f := p.h;
    result.e := x.e + y.e + 64;
    if normalize then
      if (PQWordRec(@result.f)^.h and ROUNDER) = 0 then
      begin
        result.f := result.f * 2;
        dec(result.e);
      end;
  end;

  {$else} // regular Grisu method - optimized for 32-bit CPUs

  procedure diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean; out result: TDIY_FP);
  var
    _x: TQWordRec absolute x;
    _y: TQWordRec absolute y;
    r: TQWordRec absolute result;
    ac, bc, ad, bd, t1: TQWordRec;
  begin
    ac.v := qword(_x.h) * _y.h;
    bc.v := qword(_x.l) * _y.h;
    ad.v := qword(_x.h) * _y.l;
    bd.v := qword(_x.l) * _y.l;
    t1.v := qword(ROUNDER) + bd.h + bc.l + ad.l;
    result.f := ac.v + ad.h + bc.h + t1.h;
    result.e := x.e + y.e + 64;
    if normalize then
      if (r.h and ROUNDER) = 0 then
      begin
        inc(result.f, result.f);
        dec(result.e);
      end;
  end;

  {$endif CPUINTEL}

  const
    // alpha =-61; gamma = 0
    // full cache: 1E-450 .. 1E+432, step = 1E+18
    // sparse = 1/10
    C_PWR10_DELTA = 18;
    C_PWR10_COUNT = 50;

  type
    TDIY_FP_Cached_Power10 = record
      base:         array [ 0 .. 9 ] of TDIY_FP_Power_of_10;
      factor_plus:  array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
      factor_minus: array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
      // extra mantissa correction [ulp; signed]
      corrector:    array [ 0 .. C_PWR10_COUNT - 1 ] of shortint;
    end;

  const
    CACHED_POWER10: TDIY_FP_Cached_Power10 = (
      base: (
          ( c: ( f: qword($825ECC24C8737830); e: -362 ); e10:  -90 ),
          ( c: ( f: qword($E2280B6C20DD5232); e: -303 ); e10:  -72 ),
          ( c: ( f: qword($C428D05AA4751E4D); e: -243 ); e10:  -54 ),
          ( c: ( f: qword($AA242499697392D3); e: -183 ); e10:  -36 ),
          ( c: ( f: qword($9392EE8E921D5D07); e: -123 ); e10:  -18 ),
          ( c: ( f: qword($8000000000000000); e:  -63 ); e10:    0 ),
          ( c: ( f: qword($DE0B6B3A76400000); e:   -4 ); e10:   18 ),
          ( c: ( f: qword($C097CE7BC90715B3); e:   56 ); e10:   36 ),
          ( c: ( f: qword($A70C3C40A64E6C52); e:  116 ); e10:   54 ),
          ( c: ( f: qword($90E40FBEEA1D3A4B); e:  176 ); e10:   72 )
      );
      factor_plus: (
          ( c: ( f: qword($F6C69A72A3989F5C); e:   534 ); e10:  180 ),
          ( c: ( f: qword($EDE24AE798EC8284); e:  1132 ); e10:  360 )
      );
      factor_minus: (
          ( c: ( f: qword($84C8D4DFD2C63F3B); e:  -661 ); e10: -180 ),
          ( c: ( f: qword($89BF722840327F82); e: -1259 ); e10: -360 )
      );
      corrector: (
          0,  0,  0,  0,  1,  0,  0,  0,  1, -1,
          0,  1,  1,  1, -1,  0,  0,  1,  0, -1,
          0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         -1,  0,  0, -1,  0,  0,  0,  0,  0, -1,
          0,  0,  0,  0,  1,  0,  0,  0, -1,  0
      ));
    CACHED_POWER10_MIN10 = -90 -360;
    // = ref.base[low(ref.base)].e10 + ref.factor_minus[high(ref.factor_minus)].e10

  // return normalized correctly rounded approximation of the power of 10
  // scaling factor, intended to shift a binary exponent of the original number
  // into selected [ alpha .. gamma ] range
  procedure diy_fp_cached_power10(exp10: integer; out factor: TDIY_FP_Power_of_10);
  var
    i, xmul: integer;
    A, B: PDIY_FP_Power_of_10;
    cx: PtrInt;
    ref: ^TDIY_FP_Cached_Power10;
  begin
    ref := @CACHED_POWER10; // much better code generation on PIC/x86_64
    // find non-sparse index
    if exp10 <= CACHED_POWER10_MIN10 then
      i := 0
    else
    begin
      i := (exp10 - CACHED_POWER10_MIN10) div C_PWR10_DELTA;
      if i * C_PWR10_DELTA + CACHED_POWER10_MIN10 <> exp10 then
        inc(i); // round-up
      if i > C_PWR10_COUNT - 1 then
        i := C_PWR10_COUNT - 1;
    end;
      // generate result
    A := @ref.base[i mod length(ref.base)];
    xmul := (i div length(ref.base)) - length(ref.factor_minus);
    if xmul = 0 then
    begin
      // base
      factor := A^;
      exit;
    end;
    // surrogate
    if xmul > 0 then
      dec(xmul)
    else
      xmul := -(xmul + 1);
    B := @ref.factor_plus[xmul];
    factor.e10 := A.e10 + B.e10;
    if A.e10 = 0 then
      // exact
      factor.c := B.c
    else
    begin
      diy_fp_multiply(A.c, B.c, true, factor.c);
      // adjust mantissa
      cx := ref.corrector[i];
      if cx <> 0 then
        inc(PInt64(@factor.c.f)^, cx);
    end;
  end;


  procedure unpack_float(const f: double; out minus: boolean; out result: TDIY_FP);
    {$ifdef HASINLINE} inline;{$endif}
  type
    TSplitFloat = packed record
      case byte of
        0: (f: double);
        1: (b: array[0..7] of byte);
        2: (w: array[0..3] of word);
        3: (d: array[0..1] of cardinal);
        4: (l: qword);
    end;
  var
    doublebits: TSplitFloat;
  begin
  {$ifdef FPC_DOUBLE_HILO_SWAPPED}
    // high and low cardinal are swapped when using the arm fpa
    doublebits.d[0] := TSplitFloat(f).d[1];
    doublebits.d[1] := TSplitFloat(f).d[0];
  {$else not FPC_DOUBLE_HILO_SWAPPED}
    doublebits.f := f;
  {$endif FPC_DOUBLE_HILO_SWAPPED}
  {$ifdef endian_big}
    minus := (doublebits.b[0] and $80 <> 0);
    result.e := (doublebits.w[0] shr 4) and $7FF;
  {$else endian_little}
    minus := (doublebits.b[7] and $80 <> 0);
    result.e := (doublebits.w[3] shr 4) and $7FF;
  {$endif endian}
    result.f := doublebits.l and $000FFFFFFFFFFFFF;
  end;

  const
    C_FRAC2_BITS = 52;
    C_EXP2_BIAS = 1023;
    C_DIY_FP_Q = 64;
    C_GRISU_ALPHA = -61;
    C_GRISU_GAMMA = 0;

    C_EXP2_SPECIAL = C_EXP2_BIAS * 2 + 1;
    C_MANT2_INTEGER = qword(1) shl C_FRAC2_BITS;

  type
    TAsciiDigits = array[0..39] of byte;
    PAsciiDigits = ^TAsciiDigits;

  // convert unsigned integers into decimal digits

  function gen_digits_32(buf: PAsciiDigits; x: dword;
    pad_9zero: boolean = false): PtrInt;
  const
    digits: array[0..9] of cardinal = (
      0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
  var
    n: PtrInt;
    m, z: cardinal;
  begin
    // Calculate amount of digits
    if x = 0 then
      n := 0  // emit nothing if padding is not required
    else
    begin
      n := integer((BSRdword(x) + 1) * 1233) shr 12;
      if x >= digits[n] then
        inc(n);
    end;
    if pad_9zero and (n < 9) then
      n := 9;
    result := n;
    if n = 0 then
      exit;
    // Emit digits
    dec(PByte(buf));
    m := x;
    repeat
      if m <> 0 then
      begin
        z := m div 10;
        buf[n] := m - z * 10;
        m := z;
        dec(n);
        if n = 0 then
          exit;
        continue;
      end
      else
      repeat
        buf[n] := 0;
        dec(n);
      until n = 0;
      break;
    until false;
  end;

  function gen_digits_64(buf: PAsciiDigits; const x: qword;
    pad_19zero: boolean = false): PtrInt;
  var
    n_digits: PtrInt;
    temp: qword;
    splitl, splitm, splith: cardinal;
  begin
    // Split X into 3 unsigned 32-bit integers; lower two should be < 10 digits long
    splith := 0;
    splitm := 0;
    if x < 1000000000 then
      splitl := x
    else
    begin
      temp := x div 1000000000;
      splitl := x - temp * 1000000000;
      if temp < 1000000000 then
        splitm := temp
      else
      begin
        splith := temp div 1000000000;
        splitm := cardinal(temp) - splith * 1000000000;
      end;
    end;
    // Generate digits
    n_digits := gen_digits_32(buf, splith, false);
    if pad_19zero and (n_digits = 0) then
    begin
        // at most 18 digits expected from splitm and splitl, so add one more
      buf[0] := 0;
      n_digits := 1;
    end;
    inc(n_digits, gen_digits_32(@buf[n_digits], splitm, n_digits <> 0));
    inc(n_digits, gen_digits_32(@buf[n_digits], splitl, n_digits <> 0));
    result := n_digits;
  end;

  // Performs digit sequence rounding, returns decimal point correction
  function round_digits(var buf: TAsciiDigits; var n_current: integer;
    n_max: PtrInt; half_round_to_even: boolean = true): PtrInt;
  var
    n: PtrInt;
    dig_round, dig_sticky: byte;
    {$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
    i: PtrInt;
    {$endif}
  begin
    result := 0;
    n := n_current;
    n_current := n_max;
    // Get round digit
    dig_round := buf[n_max];
  {$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
    // Detect if rounding-up the second last digit turns the "dig_round"
    // into "5"; also make sure we have at least 1 digit between "dig_round"
    // and the second last.
    if not half_round_to_even then
      if (dig_round = 4) and (n_max < n - 3) then
        if buf[n - 2] >= 8 then // somewhat arbitrary...
        begin
          // check for only "9" are in between
          i := n - 2;
          repeat
            dec(i);
          until (i = n_max) or (buf[i] <> 9);
          if i = n_max then
            // force round-up
            dig_round := 9; // any value ">=5"
        end;
  {$endif GRISU1_F2A_AGRESSIVE_ROUNDUP}
    if dig_round < 5 then
      exit;
    // Handle "round half to even" case
    if (dig_round = 5) and half_round_to_even and
       ((n_max = 0) or (buf[n_max - 1] and 1 = 0)) then
    begin
      // even and a half: check if exactly the half
      dig_sticky := 0;
      while (n > n_max + 1) and (dig_sticky = 0) do
      begin
        dec(n);
        dig_sticky := buf[n];
      end;
      if dig_sticky = 0 then
        exit; // exactly a half -> no rounding is required
    end;
    // Round-up
    while n_max > 0 do
    begin
      dec(n_max);
      inc(buf[n_max]);
      if buf[n_max] < 10 then
      begin
        // no more overflow: stop now
        n_current := n_max + 1;
        exit;
      end;
      // continue rounding
    end;
    // Overflow out of the 1st digit, all n_max digits became 0
    buf[0] := 1;
    n_current := 1;
    result := 1;
  end;


  // format the number in the fixed-point representation
  procedure return_fixed(str: PAnsiChar; minus: boolean; var digits: TAsciiDigits;
    n_digits_have, fixed_dot_pos, frac_digits: integer);
  var
    p: PAnsiChar;
    d: PByte;
    cut_digits_at, n_before_dot, n_before_dot_pad0, n_after_dot_pad0,
    n_after_dot, n_tail_pad0: integer;
  begin
    // Round digits if necessary
    cut_digits_at := fixed_dot_pos + frac_digits;
    if cut_digits_at < 0 then
      // zero
      n_digits_have := 0
    else if cut_digits_at < n_digits_have then
      // round digits
      inc(fixed_dot_pos, round_digits(digits, n_digits_have, cut_digits_at
        {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
    // Before dot: digits, pad0
    if (fixed_dot_pos <= 0) or (n_digits_have = 0) then
    begin
      n_before_dot := 0;
      n_before_dot_pad0 := 1;
    end
    else if fixed_dot_pos > n_digits_have then
    begin
      n_before_dot := n_digits_have;
      n_before_dot_pad0 := fixed_dot_pos - n_digits_have;
    end
    else
    begin
      n_before_dot := fixed_dot_pos;
      n_before_dot_pad0 := 0;
    end;
    // After dot: pad0, digits, pad0
    if fixed_dot_pos < 0 then
      n_after_dot_pad0 := -fixed_dot_pos
    else
      n_after_dot_pad0 := 0;
    if n_after_dot_pad0 > frac_digits then
      n_after_dot_pad0 := frac_digits;
    n_after_dot := n_digits_have - n_before_dot;
    n_tail_pad0 := frac_digits - n_after_dot - n_after_dot_pad0;
    p := str + 1;
    // Sign
    if minus then
    begin
      p^ := '-';
      inc(p);
    end;
    // Integer significant digits
    d := @digits;
    if n_before_dot > 0 then
      repeat
        p^ := AnsiChar(d^ + ord('0'));
        inc(p);
        inc(d);
        dec(n_before_dot);
      until n_before_dot = 0;
    // Integer 0-padding
    if n_before_dot_pad0 > 0 then
      repeat
        p^ := '0';
        inc(p);
        dec(n_before_dot_pad0);
      until n_before_dot_pad0 = 0;
    //
    if frac_digits <> 0 then
    begin
      // Dot
      p^ := '.';
      inc(p);
      // Pre-fraction 0-padding
      if n_after_dot_pad0 > 0 then
        repeat
          p^ := '0';
          inc(p);
          dec(n_after_dot_pad0);
        until n_after_dot_pad0 = 0;
      // Fraction significant digits
      if n_after_dot > 0 then
        repeat
          p^ := AnsiChar(d^ + ord('0'));
          inc(p);
          inc(d);
          dec(n_after_dot);
        until n_after_dot = 0;
      // Tail 0-padding
      if n_tail_pad0 > 0 then
        repeat
          p^ := '0';
          inc(p);
          dec(n_tail_pad0);
        until n_tail_pad0 = 0;
    end;
    // Store length
    str[0] := AnsiChar(p - str - 1);
  end;

  // formats the number as exponential representation
  procedure return_exponential(str: PAnsiChar; minus: boolean;
    var digits: TAsciiDigits; n_digits_have, n_digits_req, d_exp: PtrInt);
  var
    p, exp: PAnsiChar;
    c: AnsiChar;
    j: PtrInt;
  begin
    p := str + 1;
    // Sign
    if minus then
    begin
      p^ := '-';
      inc(p);
    end;
    // Integer part
    if n_digits_have > 0 then
      p^ := AnsiChar(digits[0] + ord('0'))
    else
      p^ := '0';
    inc(p);
    // Dot
    if n_digits_req > 1 then
    begin
      p^ := '.';
      inc(p);
    end;
    // Fraction significant digits
    j := 1;
    if n_digits_req < n_digits_have then
      n_digits_have := n_digits_req;
    while j < n_digits_have do
    begin
      c := AnsiChar(digits[j]);
      inc(c, ord('0'));
      p^ := c;
      inc(p);
      inc(j);
    end;
    // Exponent designator
    p^ := 'E';
    inc(p);
    // Exponent sign
    if d_exp < 0 then
    begin
      p^ := '-';
      d_exp := -d_exp;
    end
    else
      p^ := '+';
    inc(p);
    // Exponent digits
    exp := pointer(SmallUInt32UTF8[d_exp]);
    PCardinal(p)^ := PCardinal(exp)^;
    inc(p, PStrLen(exp - _STRLEN)^);
    // Store length
    str[0] := AnsiChar(p - str - 1);
  end;

  /// set one of special results with proper sign
  procedure return_special(str: PAnsiChar; sign: integer; const spec: shortstring);
  begin
    // Compute length
    str[0] := spec[0];
    if sign <> 0 then
      inc(str[0]);
    inc(str);
    // Sign
    if sign <> 0 then
    begin
      if sign > 0 then
        str^ := '+'
      else
        str^ := '-';
      inc(str);
    end;
    // Special text (3 chars)
    PCardinal(str)^ := PCardinal(@spec[1])^;
  end;


  // Calculates the exp10 of a factor required to bring the binary exponent
  // of the original number into selected [ alpha .. gamma ] range:
  // result := ceiling[ ( alpha - e ) * log10(2) ]
  function k_comp(e, alpha{, gamma}: integer): integer;
  var
    dexp: double;
  const
    D_LOG10_2: double = 0.301029995663981195213738894724493027; // log10(2)
  var
    x, n: integer;
  begin
    x := alpha - e;
    dexp := x * D_LOG10_2;
    // ceil( dexp )
    n := trunc(dexp);
    if x > 0 then
      if dexp <> n then
        inc(n); // round-up
    result := n;
  end;


// main procedure d2a(min_width, frac_digits: integer; const v: double; str: PAnsiChar);
var
  w, D: TDIY_FP;
  c_mk: TDIY_FP_Power_of_10;
  n, mk, dot_pos, n_digits_need, n_digits_have: integer;
  n_digits_req, n_digits_sci: integer;
  minus: boolean;
  fl, one_maskl: qword;
  one_e: integer;
  {$ifdef CPU32}
  one_mask, f: cardinal; // run a 2nd loop with 32-bit range
  {$endif CPU32}
  buf: TAsciiDigits;
begin
  // Limit parameters
  if frac_digits > 216 then
    frac_digits := 216; // Delphi compatible
  if min_width <= C_NO_MIN_WIDTH then
    min_width := -1 // no minimal width
  else if min_width < 0 then
    min_width := 0; // minimal width is as short as possible
  // Format profile: select "n_digits_need" (and "n_digits_exp")
  n_digits_req := nDig_mantissa;
  // number of digits to be calculated by Grisu
  n_digits_need := nDig_mantissa;
  if n_digits_req < n_digits_need then
    n_digits_need := n_digits_req;
  // number of mantissa digits to be printed in exponential notation
  if min_width < 0 then
    n_digits_sci := n_digits_req
  else
  begin
    n_digits_sci := min_width -1 {sign} -1 {dot} -1 {E} -1 {E-sign} - nDig_exp10;
    if n_digits_sci < 2 then
      n_digits_sci := 2; // at least 2 digits
    if n_digits_sci > n_digits_req then
      n_digits_sci := n_digits_req; // at most requested by real_type
  end;
  // Float -> DIY_FP
  unpack_float(v, minus, w);
  // Handle Zero
  if (w.e = 0) and (w.f = 0) then
  begin
    if frac_digits >= 0 then
      return_fixed(str, minus, buf, 0, 1, frac_digits)
    else
      return_exponential(str, minus, buf, 0, n_digits_sci, 0);
    exit;
  end;
  // Handle specials
  if w.e = C_EXP2_SPECIAL then
  begin
    n := 1 - ord(minus) * 2; // default special sign [-1|+1]
    if w.f = 0 then
      return_special(str, n, C_STR_INF)
    else
    begin
      // NaN [also pseudo-NaN, pseudo-Inf, non-normal for floatx80]
      {$ifdef GRISU1_F2A_NAN_SIGNLESS}
      n := 0;
      {$endif}
      {$ifndef GRISU1_F2A_NO_SNAN}
      if (w.f and (C_MANT2_INTEGER shr 1)) = 0 then
        return_special(str, n, C_STR_SNAN)
      else
      {$endif GRISU1_F2A_NO_SNAN}
        return_special(str, n, C_STR_QNAN);
    end;
    exit;
  end;
  // Handle denormals
  if w.e <> 0 then
  begin
    // normal
    w.f := w.f or C_MANT2_INTEGER;
    n := C_DIY_FP_Q - C_FRAC2_BITS - 1;
  end
  else
  begin
    // denormal
    n := 63 - BSRqword(w.f);
    inc(w.e);
  end;
  // Final normalization
  w.f := w.f shl n;
  dec(w.e, C_EXP2_BIAS + n + C_FRAC2_BITS);
  // 1. Find the normalized "c_mk = f_c * 2^e_c" such that
  //    "alpha <= e_c + e_w + q <= gamma"
  // 2. Define "V = D * 10^k": multiply the input number by "c_mk", do not
  //    normalize to land into [ alpha .. gamma ]
  // 3. Generate digits ( n_digits_need + "round" )
  if (C_GRISU_ALPHA <= w.e) and (w.e <= C_GRISU_GAMMA) then
  begin
    // no scaling required
    D := w;
    c_mk.e10 := 0;
  end
  else
  begin
    mk := k_comp(w.e, C_GRISU_ALPHA{, C_GRISU_GAMMA} );
    diy_fp_cached_power10(mk, c_mk);
    // Let "D = f_D * 2^e_D := w (*) c_mk"
    if c_mk.e10 = 0 then
      D := w
    else
      diy_fp_multiply(w, c_mk.c, false, D);
  end;
  // Generate digits: integer part
  n_digits_have := gen_digits_64(@buf, D.f shr (-D.e));
  dot_pos := n_digits_have;
  // Generate digits: fractional part
  {$ifdef CPU32}
  f := 0; // "sticky" digit
  {$endif CPU32}
  if D.e < 0 then
    repeat
      // MOD by ONE
      one_e := D.e;
      one_maskl := qword(1) shl (-D.e) - 1;
      fl := D.f and one_maskl;
      // 64-bit loop (very efficient on x86_64, slower on i386)
      while {$ifdef CPU32} (one_e < -29) and {$endif}
            (n_digits_have < n_digits_need + 1) and (fl <> 0) do
      begin
        // f := f * 5;
        inc(fl, fl shl 2);
        // one := one / 2
        one_maskl := one_maskl shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := fl shr (-one_e);
        // MOD by one
        fl := fl and one_maskl;
        // next
        inc(n_digits_have);
      end;
      {$ifdef CPU32}
      if n_digits_have >= n_digits_need + 1 then
      begin
        // only "sticky" digit remains
        f := ord(fl <> 0);
        break;
      end;
      one_mask := cardinal(one_maskl);
      f := cardinal(fl);
      // 32-bit loop
      while (n_digits_have < n_digits_need + 1) and (f <> 0) do
      begin
        // f := f * 5;
        inc(f, f shl 2);
        // one := one / 2
        one_mask := one_mask shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := f shr (-one_e);
        // MOD by one
        f := f and one_mask;
        // next
        inc(n_digits_have);
      end;
      {$endif CPU32}
    until true;
  {$ifdef CPU32}
  // Append "sticky" digit if any
  if (f <> 0) and (n_digits_have >= n_digits_need + 1) then
  begin
    // single "<>0" digit is enough
    n_digits_have := n_digits_need + 2;
    buf[n_digits_need + 1] := 1;
  end;
  {$endif CPU32}
  // Round to n_digits_need using "roundTiesToEven"
  if n_digits_have > n_digits_need then
    inc(dot_pos, round_digits(buf, n_digits_have, n_digits_need));
  // Generate output
  if frac_digits >= 0 then
  begin
    return_fixed(str, minus, buf, n_digits_have, dot_pos - c_mk.e10, frac_digits);
    exit;
  end;
  if n_digits_have > n_digits_sci then
    inc(dot_pos, round_digits(buf, n_digits_have, n_digits_sci
      {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
  return_exponential(str, minus, buf, n_digits_have, n_digits_sci,
    dot_pos - c_mk.e10 - 1);

end;

