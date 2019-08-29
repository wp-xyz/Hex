unit Real48Utils;

{$mode objfpc}{$H+}

interface

type
  { Over 32 bits does not work }
  //TBit52 = 0..$FFFFFFFFFFFFF; { (1 shl 52) - 1 }
  //TBit40 = 0..$FFFFFFFFFF;    { (1 shl 40) - 1 }
  //TBit39 = 0..$7FFFFFFFFF;    { (1 shl 39) - 1 }
  TBit32 = 0..$FFFFFFFF;      { (1 shl 32) - 1 }
  TBit20 = 0..(1 shl 20) - 1;
  TBit11 = 0..(1 shl 11) - 1;
  TBit07 = 0..(1 shl 07) - 1;
  TBit01 = 0..(1 shl 01) - 1;

  //Double
  //S1 E11[Bias $3FF] F52
  TDoubleRec = bitpacked record
    { F:TBit52; }
    F2:TBit20;
    F1:TBit32;
    E:TBit11;
    S:TBit01;
  end;
  PDoubleRec = ^TDoubleRec;


  //Real48
  //S1 F39 E8[Bias 129]
  TReal48Rec = bitpacked record
    E:Byte;
    { F:TBit39; }
    F2:TBit07;
    F1:TBit32;
    S:TBit01;
  end;
  PReal48Rec = ^TReal48Rec;

function Double2Real(d : double) : real48;

operator := (d:double) r:real48; inline;
operator +(const r1:Real48;const r2:Real48) r:Real48;inline;
operator -(const r1:Real48;const r2:Real48) r:Real48;inline;
operator *(const r1:Real48;const r2:Real48) r:Real48;inline;
operator /(const r1:Real48;const r2:Real48) r:Real48;inline;
operator =(const r1:Real48;const r2:Real48) r:boolean;inline;
operator <(const r1:Real48;const r2:Real48) r:boolean;inline;
operator >(const r1:Real48;const r2:Real48) r:boolean;inline;
operator >=(const r1:Real48;const r2:Real48) r:boolean;inline;
operator <=(const r1:Real48;const r2:Real48) r:boolean;inline;

implementation

function Double2Real(d : double) : real48;
var
   res : array[0..5] of byte;
   rrec:TReal48Rec absolute res;
   drec:TDoubleRec absolute d;
begin
  { copy mantissa }
  rrec.F1 := drec.F1;
  rrec.F2 := drec.F2 shr 13;

  { copy exponent }
  { correct exponent: }
  rrec.E := drec.E - 1023 + 129;

  { set sign }
  rrec.S := drec.S;
  double2real:=res;
end;

operator := (d:double) r:real48; inline;
begin
 r := Double2Real(d);
end;

operator +(const r1:Real48;const r2:Real48) r:Real48;inline;
begin
 r := Double(r1)+Double(r2);
end;

operator -(const r1:Real48;const r2:Real48) r:Real48;inline;
begin
 r := Double(r1)-Double(r2);
end;

operator *(const r1:Real48;const r2:Real48) r:Real48;inline;
begin
 r := Double(r1)*Double(r2);
end;

operator /(const r1:Real48;const r2:Real48) r:Real48;inline;
begin
 r := Double(r1)/Double(r2);
end;

operator =(const r1:Real48;const r2:Real48) r:boolean;inline;
begin
 r := Double(r1)=Double(r2);
end;

operator <(const r1:Real48;const r2:Real48) r:boolean;inline;
begin
 r := Double(r1)<Double(r2);
end;

operator >(const r1:Real48;const r2:Real48) r:boolean;inline;
begin
 r := Double(r1)>Double(r2);
end;

operator >=(const r1:Real48;const r2:Real48) r:boolean;inline;
begin
 r := Double(r1)>=Double(r2);
end;

operator <=(const r1:Real48;const r2:Real48) r:boolean;inline;
begin
 r := Double(r1)<=Double(r2);
end;

end.

