program DataSculptor;

uses
  Forms,
  Main in 'Main.pas' {DataSetDesigner},
  UnitAttrEntero in 'UnitAttrEntero.pas' {FormEntero},
  UnitAttrReal in 'UnitAttrReal.pas' {FormReal},
  UnitAttrSymbolic in 'UnitAttrSymbolic.pas' {FormSimbolico},
  UnitAnalyzer in 'UnitAnalyzer.pas' {FormAnalyzer},
  cMatrix in 'fndMth32\Maths\cMatrix.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataSetDesigner, DataSetDesigner);
  Application.CreateForm(TFormEntero, FormEntero);
  Application.CreateForm(TFormReal, FormReal);
  Application.CreateForm(TFormSimbolico, FormSimbolico);
  Application.CreateForm(TFormAnalyzer, FormAnalyzer);
  Application.Run;
end.
