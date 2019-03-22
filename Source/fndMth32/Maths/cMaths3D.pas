{                                                                              }
{                           3D Mathematics v3.03                               }
{                                                                              }
{      This unit is copyright © 1999-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is c3DMath.pas                     }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   1999/10/05  0.01  T3DPoint                                                 }
{   2002/06/01  0.02  Created cRational unit from cMaths.                      }
{   2003/02/16  3.03  Revised for Fundamentals 3.                              }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cMaths3D;

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cMatrix;



{                                                                              }
{ 3D Point                                                                     }
{                                                                              }
type
  T3DPointRec = record
    Case Boolean of
      False : (X, Y, Z, S : Extended);
      True  : (Vector     : Array[0..3] of Extended);
  end;
  P3DPointRec = ^T3DPointRec;

procedure Init3DPoint(var Point: T3DPointRec; const X, Y, Z: Extended);
procedure Init3DVector(var Point: T3DPointRec; const X, Y, Z: Extended);
procedure RotateX(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
procedure RotateY(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
procedure RotateZ(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
procedure RotateXYZ(const Point: P3DPointRec; const Count: Integer;
          const XAngle, YAngle, ZAngle: Extended);
procedure RotateAroundVector(const Point: P3DPointRec; const Count: Integer;
          const NX, NY, NZ, Angle: Extended);
procedure Homogenize3DPoint(var Point: T3DPointRec);
procedure Scale3DPoint(var Point: T3DPointRec;
          const XScale, YScale, ZScale: Extended);
procedure Move3DPoint(var Point: T3DPointRec;
          const XOffset, YOffset, ZOffset: Extended);




{                                                                              }
{ T3DPoint                                                                     }
{  Stores a (x,y,z)-value which can represent a point or a vector in 3D        }
{    space.                                                                    }
{  Internally it inherits from TFloatVector so all the vector operations are   }
{    available.                                                                }
{  A point is represented as [x,y,z,1] and a vector as [x,y,z,0]. The 4th      }
{    element is needed when multiplying with transformation matrices (see      }
{    "3D Transformation matrices") to preserve scale.                          }
{                                                                              }
type
  E3DPoint = class(Exception);
  T3DPoint = class
  protected
    FX, FY, FZ, FS : Extended;

  public
    procedure CrossProduct(const P: T3DPoint);

    { Parallel projections                                                     }
    { Angle typically 30 or 45                                                 }
    procedure CavalierProject(const Angle: Extended; var X, Y: Extended);    // (x,y)=(x+z*cos(Angle),y+z*sin(Angle))
    procedure CabinetProject(const Angle: Extended; var X, Y: Extended);     // (x,y)=(x+z/2*cos(Angle),y+z/2*sin(Angle))

    { Perspective projections                                                  }
    { Zv = distance from origin of z-axis vanishing point                      }
    { Xv = distance from origin of x-axis vanishing point                      }
    procedure OnePointPerspectiveProject(const Angle, Zv: Extended; var X, Y: Extended);
    procedure TwoPointPerspectiveProject(const Angle, Xv, Zv: Extended; var X, Y: Extended);

    property  X: Extended read FX write FX;
    property  Y: Extended read FY write FY;
    property  Z: Extended read FZ write FZ;
  end;



{                                                                              }
{ 3D Transformation matrices                                                   }
{   Multiply with a T3DPoint to transform. Transform matrices can also be      }
{   multiplied with each other before being applied to a T3DPoint.             }
{   All are 4x4 matrices.                                                      }
{                                                                              }
function OriginAndScaleTransform(const TX, TY, TZ, SX, SY, SZ: Extended): TMatrix;
{ Translates origin with (TX, TY, TZ) and scale by (SX, SY, SZ)                }
function XRotateTransform(const Angle: Extended): TMatrix;
function YRotateTransform(const Angle: Extended): TMatrix;
function ZRotateTransform(const Angle: Extended): TMatrix;
function XYZRotateTransform(const XAngle, YAngle, ZAngle: Extended): TMatrix;
{ Rotate around x, y and z-axis                                                }



implementation

uses
  { Delphi }
  Math,

  { Fundamentals }
  cMaths;



{                                                                              }
{ T3DPointRec                                                                  }
{                                                                              }
procedure Init3DPoint(var Point: T3DPointRec; const X, Y, Z: Extended);
begin
  Point.X := X;
  Point.Y := Y;
  Point.Z := Z;
  Point.S := 1.0;
end;

procedure Init3DVector(var Point: T3DPointRec; const X, Y, Z: Extended);
begin
  Point.X := X;
  Point.Y := Y;
  Point.Z := Z;
  Point.S := 0.0;
end;

procedure RotateX(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
var S, C : Extended;
    Y, Z : Extended;
    I    : Integer;
    P    : P3DPointRec;
begin
  SinCos(Angle, S, C);
  P := Point;
  For I := 1 to Count do
    begin
      Y := P^.Y;
      Z := P^.Z;
      P^.Y := C * Y - S * Z;
      P^.Z := S * Y + C * Z;
      Inc(P);
    end;
end;

procedure RotateY(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
var S, C : Extended;
    X, Z : Extended;
    I    : Integer;
    P    : P3DPointRec;
begin
  SinCos(Angle, S, C);
  P := Point;
  For I := 1 to Count do
    begin
      X := P^.X;
      Z := P^.Z;
      P^.X := C * X - S * Z;
      P^.Z := S * X + C * Z;
      Inc(P);
    end;
end;

procedure RotateZ(const Point: P3DPointRec; const Count: Integer; const Angle: Extended);
var S, C : Extended;
    X, Y : Extended;
    I    : Integer;
    P    : P3DPointRec;
begin
  SinCos(Angle, S, C);
  P := Point;
  For I := 1 to Count do
    begin
      X := P^.X;
      Y := P^.Y;
      P^.X := C * X - S * Y;
      P^.Y := S * X + C * Y;
      Inc(P);
    end;
end;

procedure RotateXYZ(const Point: P3DPointRec; const Count: Integer; const XAngle, YAngle, ZAngle: Extended);
var SX, CX, SY, CY, SZ, CZ : Extended;
    F1, F2, YCX, ZSX       : Extended;
    X, Y, Z                : Extended;
    P                      : P3DPointRec;
    I                      : Integer;
begin
  SinCos(XAngle, SX, CX);
  SinCos(YAngle, SY, CY);
  SinCos(ZAngle, SZ, CZ);
  P := Point;
  For I := 1 to Count do
    begin
      X := P^.X;
      Y := P^.Y;
      Z := P^.Z;
      F2 := Y * SX + Z * CX;
      F1 := X * CY + SY * F2;
      YCX := Y * CX;
      ZSX := Z * SX;
      P^.X := CZ * F1 + SZ * (ZSX - YCX);
      P^.Y := SZ * F1 + CZ * (YCX - ZSX);
      P^.Z := CY * F2 - X * SY;
      Inc(P);
    end;
end;

procedure RotateAroundVector(const Point: P3DPointRec; const Count: Integer; const NX, NY, NZ, Angle: Extended);
var S, C        : Extended;
    X, Y, Z, F1 : Extended;
    P           : P3DPointRec;
    I           : Integer;
begin
  SinCos(Angle, S, C);
  P := Point;
  For I := 1 to Count do
    begin
      X := P^.X;
      Y := P^.Y;
      Z := P^.Z;
      F1 := (1.0 - C) * (X * NX + Y * Y * NY + Z * Z * NZ);
      P^.X := NX * F1 + C * X + S * (Y * NZ - Z * NY);
      P^.Y := NY * F1 + C * Y + S * (Z * NX - X * NZ);
      P^.Z := NZ * F1 + C * Z + S * (X * NY - Y * NX);
      Inc(P);
    end;
end;

procedure Homogenize3DPoint(var Point: T3DPointRec);
begin
  With Point do
    begin
      if S = 0.0 then
        raise E3DPoint.Create('Not a point');
      X := X / S;
      Y := Y / S;
      Z := Z / S;
      S := 1.0;
    end;
end;

procedure Scale3DPoint(var Point: T3DPointRec; const XScale, YScale, ZScale: Extended);
begin
  With Point do
    begin
      X := X * XScale;
      Y := Y * YScale;
      Z := Z * ZScale;
    end;
end;

procedure Move3DPoint(var Point: T3DPointRec; const XOffset, YOffset, ZOffset: Extended);
begin
  With Point do
    begin
      X := X + XOffset;
      Y := Y + YOffset;
      Z := Z + ZOffset;
    end;
end;



{                                                                              }
{ T3DPoint                                                                     }
{                                                                              }
procedure T3DPoint.CrossProduct(const P: T3DPoint);
var X, Y, Z, BX, BY, BZ : Extended;
begin
  X := FX;
  Y := FY;
  Z := FZ;
  BX := P.FX;
  BY := P.FY;
  BZ := P.FZ;
  FX := Y * BZ - Z * BY;
  FY := Z * BX - X * BZ;
  FZ := X * BY - Y * BX;
end;

procedure CavalierProjection(const Angle, X1, Y1, Z1: Extended; var X, Y: Extended);
var S, C : Extended;
begin
  SinCos(Angle * RadPerDeg, S, C);
  X := X1 + Z1 * C;
  Y := Y1 + Z1 * S;
end;

procedure T3DPoint.CavalierProject(const Angle: Extended; var X, Y: Extended);
begin
  CavalierProjection(Angle, FX, FY, FZ, X, Y);
end;

procedure T3DPoint.CabinetProject(const Angle: Extended; var X, Y: Extended);
begin
  CavalierProjection(Angle, FX, FY, 0.5 * FZ, X, Y);
end;

function ClipPerspectiveProjection(const P, V: Extended): Extended;
begin
  if V > 0 then
    Result := Min(P, V) else
    Result := Max(P, V);
end;

procedure T3DPoint.OnePointPerspectiveProject(const Angle, Zv: Extended; var X, Y: Extended);
var Z, ZF : Extended;
begin
  Z := ClipPerspectiveProjection(FZ, Zv);
  ZF := (Zv - Z) / Zv;
  CavalierProjection(Angle, FX * ZF, FY * ZF, Z, X, Y);
end;

procedure T3DPoint.TwoPointPerspectiveProject(const Angle, Xv, Zv: Extended; var X, Y: Extended);
var XP, ZP, ZF, XF : Extended;
begin
  XP := ClipPerspectiveProjection(FX, Xv);
  ZP := ClipPerspectiveProjection(FZ, Zv);
  XF := (Xv - XP) / Xv;
  ZF := (Zv - ZP) / Zv;
  CavalierProjection(Angle, XP * ZF, FY * XF * ZF, ZP, X, Y);
end;



{                                                                              }
{ 3D Transformation matrices                                                   }
{                                                                              }
function OriginAndScaleTransform(const TX, TY, TZ, SX, SY, SZ: Extended): TMatrix;
begin
  Result := TMatrix.CreateSquare(4);
  Result.SetRow(0, [ SX, 0.0, 0.0, -TX]);
  Result.SetRow(1, [0.0,  SY, 0.0, -TY]);
  Result.SetRow(2, [0.0, 0.0,  SZ, -TZ]);
  Result.SetRow(3, [0.0, 0.0, 0.0, 1.0]);
end;

function XRotateTransform(const Angle: Extended): TMatrix;
var S, C : Extended;
begin
  SinCos(Angle, S, C);
  Result := TMatrix.CreateSquare(4);
  Result.SetRow(0, [1.0, 0.0, 0.0, 0.0]);
  Result.SetRow(1, [0.0, C  , -S , 0.0]);
  Result.SetRow(2, [0.0, S  , C  , 0.0]);
  Result.SetRow(3, [0.0, 0.0, 0.0, 1.0]);
end;

function YRotateTransform(const Angle: Extended): TMatrix;
var S, C : Extended;
begin
  SinCos(Angle, S, C);
  Result := TMatrix.CreateSquare(4);
  Result.SetRow(0, [C  , 0.0, -S , 0.0]);
  Result.SetRow(1, [0.0, 1.0, 0.0, 0.0]);
  Result.SetRow(2, [S  , 0.0, C  , 0.0]);
  Result.SetRow(3, [0.0, 0.0, 0.0, 1.0]);
end;

function ZRotateTransform(const Angle: Extended): TMatrix;
var S, C : Extended;
begin
  SinCos(Angle, S, C);
  Result := TMatrix.CreateSquare(4);
  Result.SetRow(0, [C  , -S , 0.0, 0.0]);
  Result.SetRow(1, [S  , C  , 0.0, 0.0]);
  Result.SetRow(2, [0.0, 0.0, 1.0, 0.0]);
  Result.SetRow(3, [0.0, 0.0, 0.0, 1.0]);
end;

function XYZRotateTransform(const XAngle, YAngle, ZAngle: Extended): TMatrix;
var SX, CX, SY, CY, SZ, CZ : Extended;
    SXSY, CXSY : Extended;
begin
  Result := TMatrix.CreateSquare(4);
  SinCos(XAngle, SX, CX);
  SinCos(YAngle, SY, CY);
  SinCos(ZAngle, SZ, CZ);
  SXSY := SX * SY;
  CXSY := CX * SY;
  Result.SetRow(0, [        CY*CZ,         CY*SZ,   -SY, 0.0]);
  Result.SetRow(1, [SXSY*CZ-CX*SZ, SXSY*SZ+CX*CZ, SX*CY, 0.0]);
  Result.SetRow(2, [CXSY*CZ+SX*SZ, CXSY*SZ-SX*CZ, CX*CY, 0.0]);
  Result.SetRow(3, [          0.0,           0.0,   0.0, 1.0]);
end;



end.

