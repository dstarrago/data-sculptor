program DataSculptor;

uses
  Forms,
  Main in 'Main.pas' {DataSetDesigner},
  SymbolicAttr in 'SymbolicAttr.pas' {SymbolicForm},
  Analyzer in 'Analyzer.pas' {AnalyzerForm},
  cMatrix in 'fndMth32\Maths\cMatrix.pas',
  NumericAttr in 'NumericAttr.pas' {NumericForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataSetDesigner, DataSetDesigner);
  Application.CreateForm(TSymbolicForm, SymbolicForm);
  Application.CreateForm(TAnalyzerForm, AnalyzerForm);
  Application.CreateForm(TNumericForm, NumericForm);
  Application.Run;
end.
