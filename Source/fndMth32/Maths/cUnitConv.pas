{                                                                              }
{                           Unit conversions v3.02                             }
{                                                                              }
{      This unit is copyright © 2002-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cUnitConv.pas                    }
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
{   2002/06/01  3.01  Created cUnitConv unit from cMaths.                      }
{   2003/02/16  3.02  Revised for Fundamentals 3.                              }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cUnitConv;

interface



{                                                                              }
{ Unit Conversion                                                              }
{                                                                              }
const
  // SI prefixes
  Centi = 1e-2;
  Milli = 1e-3;
  Micro = 1e-6;
  Nano  = 1e-9;
  Pico  = 1e-12;

  Kilo  = 1e3;
  Mega  = 1e6;
  Giga  = 1e9;
  Terra = 1e12;

  // Distance
  Meter_per_Inch    = 0.0254;
  Meter_per_Foot    = 0.3048;
  Meter_per_Yard    = 0.9144;
  Meter_per_Rod     = 5.029;    // Also refered to as a Pole or a Perch
  Meter_per_Furlong = 201.168;
  Meter_per_Mile    = 1609.4;
  Meter_per_League  = 4830;

// Temperature
function  KelvinToFahrenheit(const T: Extended): Extended;
function  FahrenheitToKelvin(const T: Extended): Extended;
function  CelsiusToKelvin(const T: Extended): Extended;
function  KelvinToCelsius(const T: Extended): Extended;
function  CelsiusToFahrenheit(const T: Extended): Extended;
function  FahrenheitToCelsius(const T: Extended): Extended;



implementation



{                                                                              }
{ Unit Conversion                                                              }
{                                                                              }
function KelvinToFahrenheit(const T: Extended): Extended;
begin
  Result := ((9 / 5) * (T - 273.15)) + 32;
end;

function FahrenheitToKelvin(const T: Extended): Extended;
begin
  Result := ((5 / 9) * (T - 32)) + 273.15;
end;

function CelsiusToKelvin(const T: Extended): Extended;
begin
  Result := T + 273.15;
end;

function KelvinToCelsius(const T: Extended): Extended;
begin
  Result := T - 273.15;
end;

function CelsiusToFahrenheit(const T: Extended): Extended;
begin
  Result := ((9 / 5) * T) + 32;
end;

function FahrenheitToCelsius(const T: Extended): Extended;
begin
  Result := (5 / 9) * (T - 32);
end;



end.
