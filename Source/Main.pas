unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, ActnList, Menus, ComCtrls, ToolWin,
  ImgList;

type
  TLinker = class;
  TDataSet = class;
  TAttribute = class
    public
      Name: string;
      Linker: TLinker;
      DataSet: TDataSet;
      constructor Create(const aName: string; aDataSet: TDataSet); virtual;
      destructor Destroy; override;
      function Print: string;  virtual; abstract;
      function Export: string;  virtual; abstract;
      function StrSample: string; virtual; abstract;
      function Sample: extended; virtual; abstract;
      function FetchData(const x: integer): extended;
      function GetData(const Val: extended): string;  virtual; abstract;
      procedure SaveToStream(S: TStream); virtual;
      procedure LoadFromStream(S: TStream); virtual;
      function ClassVal(const Ind: integer): string;  virtual;  abstract;  // Comienza en 0
      function DataClass(const Val: extended): string;  virtual; abstract;
  end;

  TDistribution = (diUniform, diNormal);
  TNumericAttr = class(TAttribute)
    private
      fDistribution: TDistribution;
      fMinConstrained: boolean;
      fMaxConstrained: boolean;
      fDiscretizationLevel: integer;
      procedure SetMaxConstrained(const Value: boolean);
      procedure SetMinConstrained(const Value: boolean);
    protected
      fMinValue: extended;
      fMaxValue: extended;
      fMinSample: extended;
      fMaxSample: extended;
    public
      Interval: extended;
      function SetInterval: string;
      procedure SetVal(const V: extended);  virtual; abstract;
      procedure SaveToStream(S: TStream); override;
      procedure LoadFromStream(S: TStream); override;
      function ClassVal(const Ind: integer): string;  override;
      function DataClass(const Val: extended): string;  override;
      property MinConstrained: boolean read fMinConstrained write SetMinConstrained;
      property MaxConstrained: boolean read fMaxConstrained write SetMaxConstrained;
      property Distribution: TDistribution read fDistribution write fDistribution;
      property MinValue: extended read fMinValue write fMinValue;
      property MaxValue: extended read fMaxValue write fMaxValue;
      property MinSample: extended read fMinSample write fMinSample;
      property MaxSample: extended read fMaxSample write fMaxSample;
      property DiscretizationLevel: integer read fDiscretizationLevel;
  end;

  TRealAttr = class(TNumericAttr)
    public
      constructor Create(const aName: string; aDataSet: TDataSet);  override;
      function Print: string;  override;
      function Export: string; override;
      function StrSample: string; override;
      function Sample: extended; override;
      procedure SetVal(const V: extended);  override;
      function GetData(const Val: extended): string;  override;
  end;

  TIntegerAttr = class(TNumericAttr)
    private
      function GetMaxValue: integer;
      function GetMinValue: integer;
      procedure SetMaxValue(const Value: integer);
      procedure SetMinValue(const Value: integer);
      function GetMaxSample: integer;
      function GetMinSample: integer;
      procedure SetMaxSample(const Value: integer);
      procedure SetMinSample(const Value: integer);
    public
      constructor Create(const aName: string; aDataSet: TDataSet);  override;
      function Print: string;  override;
      function Export: string; override;
      function StrSample: string; override;
      function Sample: extended; override;
      procedure SetVal(const V: extended);  override;
      function GetData(const Val: extended): string;  override;
      property MinValue: integer read GetMinValue write SetMinValue;
      property MaxValue: integer read GetMaxValue write SetMaxValue;
      property MinSample: integer read GetMinSample write SetMinSample;
      property MaxSample: integer read GetMaxSample write SetMaxSample;
  end;

  TSymbolicAttr = class(TAttribute)
    private
      FSymbols: TStringList;
      FNumberOfSymbols: integer;
      function GetNumberOfSymbols: integer;
      procedure SetNumberOfSymbols(const Value: integer);
    public
      constructor Create(const aName: string; aDataSet: TDataSet);  override;
      destructor Destroy;  override;
      procedure UseExplicitSymbols;
      function GetSymbol(const i: integer): string;  // Comienza en 0
      procedure AddSymbol(const S: string);
      function PosSymbol(const S: string): integer;
      function Print: string;  override;
      function Export: string; override;
      function StrSample: string; override;
      function Sample: extended; override;
      function ClassVal(const Ind: integer): string;  override;
      function DataClass(const Val: extended): string;  override;
      function GetData(const Val: extended): string;  override;
      function GetDataAt(const i: integer): string;
      procedure SaveToStream(S: TStream); override;
      procedure LoadFromStream(S: TStream); override;
      property NumberOfSymbols: integer read GetNumberOfSymbols write SetNumberOfSymbols;
  end;

  TWeights = array of integer;
  TLinker = class
    private
      fInputAttrs: TList;
      fOutputAttr: TAttribute;
      fWeights: TWeights;
      fNormWeights: array of extended;
      fSamples: array of extended;
      function NumToNum(AttrIn, AttrOut: TNumericAttr; const x: integer): extended;
      function SymToNum(AttrIn: TSymbolicAttr; AttrOut: TNumericAttr; const x: integer): extended;
      function NumToSym(AttrIn: TNumericAttr; AttrOut: TSymbolicAttr; const x: integer): extended;
      function SymToSym(AttrIn, AttrOut: TSymbolicAttr; const x: integer): extended;
      procedure Convert(const x: integer);
      function Average: extended;
      procedure RemoveWeight(const index: integer);
      procedure NormalizeWeights;
      function GetInputCount: integer;
      function GetInput(const i: integer): TAttribute;
      function GetWeight(const i: integer): string;
      function GetIndependent: boolean;
      function GetNoise: integer;
      procedure SetNoise(const Value: integer);
    public
      constructor Create(OutputAttr: TAttribute);
      destructor  Destroy; override;
      function Cycle(Attr: TAttribute): boolean;
      procedure LinkAttr(Attr: TAttribute; const Value: integer);
      procedure UnlinkAttr(Attr: TAttribute);
      procedure RemoveLink(const index: integer);
      procedure SetWeight(Attr: TAttribute; const Value: integer);
      function ReadDependence(const x: integer): extended;
      function IsInput(Attr: TAttribute): boolean;
      procedure ClearInputs;
      function Print: string;
      procedure SaveToStream(S: TStream);
      procedure LoadFromStream(S: TStream);
      property InputCount: integer read GetInputCount;
      property Input[const i: integer]: TAttribute read GetInput;
      property Weight[const i: integer]: string read GetWeight;
      property Independent: boolean read GetIndependent;
      property Noise: integer read GetNoise write SetNoise;
  end;

  TDataStore = array of array of extended;
  TIntArray = array of Integer;
  TDataSet = class
    private
      fDataStore: TDataStore;
      fInstanceNumber: integer;
      Insufflated: boolean;
      fAttributes: TList;
      AttrGenNumber: integer;
      FName: string;
      function IndexOfMax(const A: TIntArray): integer;
      procedure SetInstanceNumber(const Value: integer);
      function GetAttributesCount: integer;
      function GetAttribute(const Ai: integer): TAttribute;
      procedure FinalizeDataStore;
      function GetData(const Ai, Xi: integer): string;
      function GetClassAttribute: TAttribute;
      function GetClassCardinality: integer;
      function GetDataStore(const Ai, Xi: integer): extended;
      procedure SetDataStore(const Ai, Xi: integer; const Value: extended);
      procedure SetName(const Value: string);
    public
      constructor Create;
      destructor  Destroy;  override;
      procedure Insufflate;
      procedure UnknownValuesProcess;
      procedure BinarizationProcess;
      procedure GenerateAttr(Ai: integer);
      function NewAttrName: string;
      function AddRealAttribute: TRealAttr;
      function AddIntegerAttribute: TIntegerAttr;
      function AddSymbolicAttribute: TSymbolicAttr;
      procedure Delete(Attr: TAttribute);
      function DataClass(const Attr, Item: integer): string;
      function AttrCardinality(const Attr: integer): integer;
      function ClassValue(const Attr, Ind: integer): string;
      function FetchData(Attr: TAttribute; const x: integer): extended;
      function PosAttr(Attr: TAttribute): integer;
      function FindAttr(const AttrName: string): TAttribute;
      procedure SaveToStream(S: TStream);
      procedure LoadFromStream(S: TStream);
      function BinAttrNumber: integer;
      property InstanceNumber: integer read FInstanceNumber write SetInstanceNumber;
      property AttributesCount: integer read GetAttributesCount;
      property Attribute[const Ai: integer]: TAttribute read GetAttribute;  //Basado en 0
      property Data[const Ai, Xi: integer]: string read GetData;
      property ClassAttribute: TAttribute read GetClassAttribute;
      property ClassCardinality: integer read GetClassCardinality;
      property DataStore[const Ai, Xi: integer]: extended read GetDataStore write SetDataStore;
      property Name: string read FName write SetName;
  end;

  TParser = class
    private
      FDataSet: TDataSet;
      Line, Ind: integer;
      FToken: string;
      Text: TStrings;
      TextLine: string;
      EOF: boolean;
      LineBreak: boolean;
      procedure GoAhead;
      function NextToken: string;
      function ParseRelation: boolean;
      function ParseAttributes: boolean;
      function ParseData: boolean;
      function ParseArffData: boolean;
      function ParseInteger(var fval: extended): boolean;
      function ParseReal(var fval: extended): boolean;
      function ParseSymbolic(Attr: TSymbolicAttr; var fval: extended): boolean;
      function ParseIdent(var Ident: string): boolean;
      procedure ParseSymbolList(const AName: string);
    public
      constructor Create(Dataset: TDataSet);
      procedure Restart;
      procedure Init(aText: TStrings);
      function ParseStrings(aText: TStrings): boolean;
      function ParseXNames(aText: TStrings): boolean;
      function ParseXData(aText: TStrings): boolean;
      property Token: string read FToken;
      property DataSet: TDataSet read FDataSet;
  end;

  TDataSetDesigner = class(TForm)
    ActionList1: TActionList;
    NewReal: TAction;
    NewInt: TAction;
    NewSymbolic: TAction;
    EditAttr: TAction;
    Delete: TAction;
    PopupMenu1: TPopupMenu;
    Nuevo1: TMenuItem;
    Editar1: TMenuItem;
    Eliminar1: TMenuItem;
    Real1: TMenuItem;
    Entero1: TMenuItem;
    Simbolico1: TMenuItem;
    NewDB: TAction;
    Generate: TAction;
    Splitter1: TSplitter;
    PanelLeft: TPanel;
    HeaderLeft: TPanel;
    ListAttr: TListBox;
    PanelRight: TPanel;
    HeaderRight: TPanel;
    MemData: TMemo;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    Panel1: TPanel;
    EditInstancesNumber: TSpinEdit;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    AnalyzeAll: TAction;
    ToolButton12: TToolButton;
    MainMenu1: TMainMenu;
    Archivo1: TMenuItem;
    Edicin1: TMenuItem;
    Anlisis1: TMenuItem;
    Generar1: TMenuItem;
    NuevaBD1: TMenuItem;
    Abrir1: TMenuItem;
    Guardar1: TMenuItem;
    GuardarComo1: TMenuItem;
    Salir1: TMenuItem;
    N1: TMenuItem;
    ImportarBD1: TMenuItem;
    ExportarBD1: TMenuItem;
    N2: TMenuItem;
    NuevoReal1: TMenuItem;
    NuevoEntero1: TMenuItem;
    NuevoSimblico1: TMenuItem;
    N3: TMenuItem;
    EditarAtributo1: TMenuItem;
    Eliminar2: TMenuItem;
    GenerarBD1: TMenuItem;
    MedidasdeTeoradelaInformacin1: TMenuItem;
    MedidasdeConjuntosAproximados1: TMenuItem;
    MedidasEstadsticas1: TMenuItem;
    CaracteristicasBsicas1: TMenuItem;
    N4: TMenuItem;
    MostrarHojadeAnlisis1: TMenuItem;
    odaslasmedidas1: TMenuItem;
    BasicAnalysis: TAction;
    InformationTheoreticAnalysis: TAction;
    RoughSetsAnalysis: TAction;
    StatisticAnalysis: TAction;
    ViewAnalysisSheet: TAction;
    CleanAnalysisSheet: TAction;
    LimpiarHojadeAnlisis1: TMenuItem;
    Export: TAction;
    SaveAs: TAction;
    OpenDialog1: TOpenDialog;
    Open: TAction;
    Save: TAction;
    Import: TAction;
    Preprocesamiento: TAction;
    Preprocesamiento2: TMenuItem;
    ValoresDesconocidos1: TMenuItem;
    Binarizacin1: TMenuItem;
    UnknownVal: TAction;
    Binarization: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GenerateExecute(Sender: TObject);
    procedure ListAttrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    function GetFileName(const RawFileName: string): string;
    // Edición de BD
    procedure NewRealExecute(Sender: TObject);
    procedure NewIntExecute(Sender: TObject);
    procedure NewSymbolicExecute(Sender: TObject);
    procedure EditAttrExecute(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure EditInstancesNumberChange(Sender: TObject);
    // Manejo de archivos
    procedure NewDataBase;
    function CanProceed: boolean;  //verifica que no se pierdan datos antes de proseguir
    procedure NewDBExecute(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure OpenExecute(Sender: TObject);
    procedure ExportExecute(Sender: TObject);
    procedure ImportExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    // Analisis de datos
    procedure BasicAnalysisExecute(Sender: TObject);
    procedure InformationTheoreticAnalysisExecute(Sender: TObject);
    procedure RoughSetsAnalysisExecute(Sender: TObject);
    procedure StatisticAnalysisExecute(Sender: TObject);
    procedure AnalyzeAllExecute(Sender: TObject);
    procedure ViewAnalysisSheetExecute(Sender: TObject);
    procedure CleanAnalysisSheetExecute(Sender: TObject);
    procedure UnknownValExecute(Sender: TObject);
    procedure BinarizationExecute(Sender: TObject);
  private
    DataSet: TDataSet;
    fFileName: string;
    fFileModified: boolean;
    fProceed: boolean;
    procedure UpdateDisplay;
    procedure ListAttributes(List: TStrings; Attr: TAttribute);
    procedure ListDependences(List: TStrings; Attr: TAttribute);
    procedure UpdateDependences(List: TStrings; Attr: TAttribute);
    procedure ImportXDataFile(Parser: TParser);
  public
    { Public declarations }
    procedure EditReal(Attr: TRealAttr);
    procedure EditInteger(Attr: TIntegerAttr);
    procedure EditSymbolic(Attr: TSymbolicAttr);
    procedure UpDateListAttr(const index: integer);
    procedure ExportDataSet(Media: TStrings);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  end;

var
  DataSetDesigner: TDataSetDesigner;

procedure SetUnknown(var V: extended);
function IsUnknown(const V: extended): boolean;


implementation

uses UnitAttrReal, UnitAttrSymbolic, UnitAttrEntero, Math, UnitAnalyzer, cMatrix;

type
  Int32 = -2147483648..2147483647;
  PInt32 = ^Int32;

const
  SYMBOLS = 'ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvxyz1234567890';
  MainFormTitle = 'Diseñador de Datos';
  DefaultInstancesNumber = 10;
  Comment = '%';
  Separators = [' ', ',', Comment, #9, ':'];
  Operators = ['{', '}'];
  UNKNOWN = '?';
  SAMPLESPERCELL = 10;

{$R *.dfm}

procedure SetUnknown(var V: extended);
  begin
    PInt32(@V)^ := $7FFFFFFF;
  end;

function IsUnknown(const V: extended): boolean;
  begin
    Result := PInt32(@V)^ = $7FFFFFFF;
  end;

{ TDataSet }

function TDataSet.AddIntegerAttribute: TIntegerAttr;
begin
  Result := TIntegerAttr.Create(NewAttrName, self);
  fAttributes.Add(Result);
  Insufflated := false;
end;

function TDataSet.AddRealAttribute: TRealAttr;
begin
  Result := TRealAttr.Create(NewAttrName, self);
  fAttributes.Add(Result);
  Insufflated := false;
end;

function TDataSet.AddSymbolicAttribute: TSymbolicAttr;
begin
  Result := TSymbolicAttr.Create(NewAttrName, self);
  fAttributes.Add(Result);
  Insufflated := false;
end;

function TDataSet.AttrCardinality(const Attr: integer): integer;
begin
  if Attribute[Attr] is TSymbolicAttr
    then Result := TSymbolicAttr(Attribute[Attr]).NumberOfSymbols
    else Result := TNumericAttr(Attribute[Attr]).DiscretizationLevel;  // Revisar OJO
end;

procedure TDataSet.BinarizationProcess;
var
  l: integer;  // Variable de control de los atributos
  i: integer;  // Variable de control de los datos iniciales
  j: integer;  // Variable de control de nuevos atributos
  k: integer;  // Variable de control de instancias
  NewAtNo: integer;
  FormerAttr, Attr: TSymbolicAttr;
  NewDataStore: TDataStore;
begin
  // Preprocesamiento de atributos nominales con más de 2 valores
  l := 0;
  SetLength(NewDataStore, AttributesCount);
  for i := 0 to AttributesCount - 2 do   // No se binariza la clase
  //while i < AttributesCount do
    begin
      if (Attribute[l] is TSymbolicAttr)
        and (TSymbolicAttr(Attribute[l]).NumberOfSymbols > 2) then
        begin
          FormerAttr := TSymbolicAttr(Attribute[l]);
          NewAtNo := FormerAttr.NumberOfSymbols - 1;
          // Insertar NewAtNo nuevos atributos binarios
          for j := pred(NewAtNo) downto 0 do
            begin
              Attr := TSymbolicAttr.Create(Attribute[l].Name + '_' + ClassValue(l, j), self);
              Attr.UseExplicitSymbols;
              Attr.AddSymbol('no');
              Attr.AddSymbol('yes');
              fAttributes.Insert(succ(l), Attr);
            end;
          // Inicializar las variables
          SetLength(NewDataStore, AttributesCount - 1);
          for j := 0 to pred(NewAtNo) do
            SetLength(NewDataStore[l + j], InstanceNumber);
          // Transcribir datos
          for k := 0 to pred(InstanceNumber) do
            if IsUnknown(fDataStore[i, k])
              then
                for j := 0 to pred(NewAtNo) do
                  SetUnknown(NewDataStore[l + j, k])
              else
                if fDataStore[i, k] < NewAtNo
                  then NewDataStore[l + Trunc(fDataStore[i, k]), k] := 1;
          // Eliminar Atributo
          fAttributes.Delete(l);
          Finalize(fDataStore[i]);
          inc(l, NewAtNo);
        end
      else
        begin
          NewDataStore[l] := fDataStore[i];
          inc(l);
        end;
    end;
  NewDataStore[l] := fDataStore[High(fDataStore)];  // Copia los valores de clase
  Finalize(FDataStore);
  FDataStore := NewDataStore;
end;

function TDataSet.BinAttrNumber: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to fAttributes.Count - 2 do
    if AttrCardinality(i) = 2
      then inc(Result);
end;

function TDataSet.ClassValue(const Attr, Ind: integer): string;
begin
  Result := Attribute[Attr].ClassVal(Ind);
  {
  if Attribute[Attr] is TSymbolicAttr
    then Result := SYMBOLS[succ(Ind)]
    else Result := IntToStr(Ind);
  }
end;

constructor TDataSet.Create;
begin
  fAttributes := TList.Create;
  Randomize;
end;

function TDataSet.DataClass(const Attr, Item: integer): string;
{
var
  C: integer;}
begin
  Result := Attribute[Attr].DataClass(DataStore[Attr, Item]);
  {
  if Attribute[Attr] is TSymbolicAttr
    then Result := SYMBOLS[Trunc(DataStore[Attr, Item])] else
    begin
      C := Trunc((DataStore[Attr, Item] - TNumericAttr(Attribute[Attr]).fMinSample)) div TNumericAttr(Attribute[Attr]).Interval;
      Result := IntToStr(C);
    end
  }
end;

procedure TDataSet.Delete(Attr: TAttribute);
begin
  fAttributes.Remove(Attr);
  Attr.Free;
  Insufflated := false;
end;

destructor TDataSet.Destroy;
  var
    i: integer;
begin
  inherited;
  for i := 0 to pred(fAttributes.Count) do
    TAttribute(fAttributes[i]).free;
  fAttributes.Free;
  if Assigned(fDataStore) then FinalizeDataStore;
end;

function TDataSet.FetchData(Attr: TAttribute; const x: integer): extended;
begin
  Result := DataStore[fAttributes.IndexOf(Attr), x];
end;

procedure TDataSet.FinalizeDataStore;
var
  i: integer;
begin
  for i := 0 to High(fDataStore) do
    Finalize(fDataStore[i]);
  Finalize(fDataStore);
end;

function TDataSet.FindAttr(const AttrName: string): TAttribute;
var
  i: integer;
begin
  Result := nil;
  i := 0;
  while (i < fAttributes.Count) and (Attribute[i].Name <> AttrName) do
    inc(i);
  if i < fAttributes.Count
    then Result := Attribute[i];
end;

procedure TDataSet.GenerateAttr(Ai: integer);
var
  Xi: integer;
begin
  SetLength(fDataStore[Ai], InstanceNumber);
  if Attribute[Ai].Linker.Independent
    then
      for Xi := 0 to pred(InstanceNumber) do
        fDataStore[Ai, Xi] := Attribute[Ai].Sample
    else
      for Xi := 0 to pred(InstanceNumber) do
        fDataStore[Ai, Xi] := Attribute[Ai].Linker.ReadDependence(Xi);
  if Attribute[Ai] is TNumericAttr
    then TNumericAttr(Attribute[Ai]).SetInterval;
end;

function TDataSet.GetAttribute(const Ai: integer): TAttribute;
begin
  Result := fAttributes.Items[Ai];
end;

function TDataSet.GetAttributesCount: integer;
begin
  Result := fAttributes.Count;
end;

function TDataSet.GetClassAttribute: TAttribute;
begin
  Result := Attribute[pred(AttributesCount)];
end;

function TDataSet.GetClassCardinality: integer;
begin
  Result := AttrCardinality(pred(AttributesCount));
end;

function TDataSet.GetData(const Ai, Xi: integer): string;
begin
  Result := Attribute[Ai].GetData(DataStore[Ai, Xi]);
  {
  if Attribute[Ai] is TSymbolicAttr
    then Result := SYMBOLS[Trunc(DataStore[Ai, Xi])] else
  if Attribute[Ai] is TIntegerAttr
    then Result := IntToStr(Trunc(DataStore[Ai, Xi]))
    else Result := FloatToStr(DataStore[Ai, Xi])
  }
end;

function TDataSet.GetDataStore(const Ai, Xi: integer): extended;
begin
  Result := fDataStore[Ai, Xi];
end;

function TDataSet.IndexOfMax(const A: TIntArray): integer;
var
  Max, i: integer;
begin
  Max := 0;
  Result := 0;
  for i := 0 to High(A) do
    if A[i] > Max then
      begin
        Max := A[i];
        Result := i;
      end;
end;

procedure TDataSet.Insufflate;
var
  Ai: integer;
begin
  SetLength(fDataStore, fAttributes.Count);
  for Ai := 0 to pred(fAttributes.Count) do
    GenerateAttr(Ai);
  Insufflated := true;
end;

procedure TDataSet.LoadFromStream(S: TStream);
var
  i: integer;
  ACount, NLength: integer;
  AClass: string;
  A: TAttribute;
begin
  S.ReadBuffer(fInstanceNumber, SizeOf(fInstanceNumber));
  S.ReadBuffer(AttrGenNumber, SizeOf(AttrGenNumber));
  S.ReadBuffer(Insufflated, SizeOf(Insufflated));
  S.ReadBuffer(ACount, SizeOf(ACount));          // Número de atributos
  for i := 0 to pred(ACount) do
    begin
      S.ReadBuffer(NLength, SizeOf(NLength));    // Longitud del nombre de la clase
      SetString(AClass, nil, NLength);
      S.ReadBuffer(Pointer(AClass)^, NLength);
      if AClass = 'TRealAttr'
        then A := TRealAttr.Create('', self) else
      if AClass = 'TIntegerAttr'
        then A := TIntegerAttr.Create('', self) else
             A := TSymbolicAttr.Create('', self);
      fAttributes.Add(A);
      A.LoadFromStream(S);
    end;
  if Insufflated then
    begin
      SetLength(fDataStore, ACount);
      for i := 0 to pred(ACount) do
        begin
          SetLength(fDataStore[i], fInstanceNumber);
          S.ReadBuffer(Pointer(fDataStore[i])^, fInstanceNumber * SizeOf(extended));
        end
      //S.ReadBuffer(DataStore, fInstanceNumber * ACount * SizeOf(extended));
    end;
end;

function TDataSet.NewAttrName: string;
begin
  Result := 'A' + IntToStr(AttrGenNumber);
  inc(AttrGenNumber);
end;

function TDataSet.PosAttr(Attr: TAttribute): integer;
begin
  Result := fAttributes.IndexOf(Attr);
end;

procedure TDataSet.SaveToStream(S: TStream);
var
  i: integer;
begin
  S.WriteBuffer(fInstanceNumber, SizeOf(fInstanceNumber));
  S.WriteBuffer(AttrGenNumber, SizeOf(AttrGenNumber));
  S.WriteBuffer(Insufflated, SizeOf(Insufflated));
  S.WriteBuffer(fAttributes.Count, SizeOf(fAttributes.Count));
  for i := 0 to pred(fAttributes.Count) do
    Attribute[i].SaveToStream(S);
  if Insufflated then
    for i := 0 to pred(fAttributes.Count) do
      S.WriteBuffer(Pointer(fDataStore[i])^, fInstanceNumber * SizeOf(extended));
  //  S.WriteBuffer(Pointer(DataStore)^, fInstanceNumber * AttributesCount * SizeOf(extended));
end;

procedure TDataSet.SetDataStore(const Ai, Xi: integer;
  const Value: extended);
begin
  fDataStore[Ai, Xi] := Value;
end;

procedure TDataSet.SetInstanceNumber(const Value: integer);
begin
  FInstanceNumber := Value;
  Insufflated := false;
end;

procedure TDataSet.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDataSet.UnknownValuesProcess;
var
  i, j, C: integer;
  M: extended;
  Moda: TIntArray;
begin
  // Preprocesamiento de los valores desconocidos en los datos
  for i := 0 to pred(AttributesCount) do
    begin
    // Si Attribute[i] es numérico : Calcular la media de Attribute[i]
      if Attribute[i] is TNumericAttr then
        begin
          C := 0;
          M := 0;
          for j := 0 to pred(InstanceNumber) do
            if not IsUnknown(DataStore[i, j]) then
              begin
                M := M + DataStore[i, j];
                inc(C);
              end;
          if C > 0
            then
              begin
                M := M / C;
                if Attribute[i] is TIntegerAttr
                  then M := Round(M);
              end
            else SetUnknown(M);
        end else
    // Si Attribute[i] es simbólico : Calcular la moda de Attribute[i]
        begin
          SetLength(Moda, TSymbolicAttr(Attribute[i]).NumberOfSymbols);
          for j := 0 to pred(InstanceNumber) do
            if not IsUnknown(DataStore[i, j]) then
              begin
                inc(Moda[trunc(DataStore[i, j])]);
                M := IndexOfMax(Moda);
              end;
        end;
    // recorrer fDataStore
    // Si Val es desconocido : sustituir por media o moda
      for j := 0 to pred(InstanceNumber) do
        if IsUnknown(DataStore[i, j])
          then DataStore[i, j] := M;
    end;
end;

{ TSymbolicAttr }

procedure TSymbolicAttr.AddSymbol(const S: string);
begin
  FSymbols.Add(S);
  FNumberOfSymbols := FSymbols.Count;
end;

function TSymbolicAttr.ClassVal(const Ind: integer): string;
begin
  if Assigned(FSymbols)
    then Result := GetSymbol(Ind)
    else Result := SYMBOLS[succ(Ind)]
end;

constructor TSymbolicAttr.Create(const aName: string; aDataSet: TDataSet);
begin
  inherited;
  NumberOfSymbols := 2;
end;

function TSymbolicAttr.DataClass(const Val: extended): string;
begin
  Result := GetData(Val); 
end;

destructor TSymbolicAttr.Destroy;
begin
  if Assigned(FSymbols) then FSymbols.Free;
  inherited;
end;

function TSymbolicAttr.Export: string;
var
  i: integer;
begin
  Result := '@attribute ' + Name + ' {';
  for i := 1 to pred(NumberOfSymbols) do
    Result := Result + GetDataAt(i) + ', ';
  Result := Result + GetDataAt(NumberOfSymbols) + '}';
end;

function TSymbolicAttr.GetData(const Val: extended): string;
begin
  if IsUnknown(Val)
    then Result := UNKNOWN
    else
      if Assigned(FSymbols)
        then Result := GetSymbol(Trunc(Val))
        else Result := SYMBOLS[Trunc(Val)];
end;

function TSymbolicAttr.GetDataAt(const i: integer): string;
begin
  if Assigned(FSymbols)
    then Result := GetSymbol(pred(i))
    else Result := SYMBOLS[i];
end;

function TSymbolicAttr.GetNumberOfSymbols: integer;
begin
  Result := FNumberOfSymbols;
end;

function TSymbolicAttr.GetSymbol(const i: integer): string;
begin
  Result := FSymbols.Strings[i];
end;

procedure TSymbolicAttr.LoadFromStream(S: TStream);
begin
  inherited;
  S.ReadBuffer(FNumberOfSymbols, SizeOf(NumberOfSymbols));
end;

function TSymbolicAttr.PosSymbol(const S: string): integer;
begin
  Result := FSymbols.IndexOf(S);
end;

function TSymbolicAttr.Print: string;
var
  i: integer;
begin
  Result := Name + ': Simbólico ( ';
  for i := 1 to pred(NumberOfSymbols) do
    Result := Result + GetDataAt(i) + ', ';
  Result := Result + GetDataAt(NumberOfSymbols) + ')';
  Result := Result + Linker.Print;
end;

function TSymbolicAttr.StrSample: string;
begin
  Result := GetSymbol(Trunc(Sample));
end;

procedure TSymbolicAttr.SaveToStream(S: TStream);
var
  CN: string;
  L: integer;
begin
  CN := ClassName;
  L := Length(CN);
  S.WriteBuffer(L, SizeOf(L));
  S.WriteBuffer(Pointer(CN)^, L);
  inherited;
  S.WriteBuffer(FNumberOfSymbols, SizeOf(NumberOfSymbols));
end;

procedure TSymbolicAttr.SetNumberOfSymbols(const Value: integer);
begin
  FNumberOfSymbols := Value;
end;

procedure TSymbolicAttr.UseExplicitSymbols;
begin
  FSymbols := TStringList.Create;
end;

function TSymbolicAttr.Sample: extended;
begin
  Result := Random(NumberOfSymbols);
  if not Assigned(FSymbols)
    then Result := Result + 1;
end;

{ TAttribute }

constructor TAttribute.Create(const aName: string; aDataSet: TDataSet);
begin
  Name := aName;
  Linker := TLinker.Create(self);
  DataSet := aDataSet;
end;

destructor TAttribute.Destroy;
begin
  Linker.Free;
  inherited;
end;

function TAttribute.FetchData(const x: integer): extended;
begin
  Result := DataSet.FetchData(Self, x);
end;

procedure TAttribute.LoadFromStream(S: TStream);
var
  L: integer;
begin
  S.ReadBuffer(L, SizeOf(L));
  SetString(Name, nil, L);
  S.ReadBuffer(Pointer(Name)^, L);
  Linker.LoadFromStream(S);
end;

procedure TAttribute.SaveToStream(S: TStream);
var
  L: integer;
begin
  L := Length(Name);
  S.WriteBuffer(L, SizeOf(L));
  S.WriteBuffer(Pointer(Name)^, L);
  Linker.SaveToStream(S);
end;

{ TRealAttr }

constructor TRealAttr.Create(const aName: string; aDataSet: TDataSet);
begin
  inherited;
  MinConstrained := false;
  MaxConstrained := false;
  MinSample := MaxValue;
  MaxSample := MinValue;
end;

function TRealAttr.Export: string;
begin
  Result := '@attribute ' + Name + ' Real';
end;

function TRealAttr.GetData(const Val: extended): string;
begin
  if IsUnknown(Val)
    then Result := UNKNOWN
    else Result := FloatToStrF(Val, ffGeneral, 2, 2);
end;

function TRealAttr.Print: string;
begin
  Result := Name + ': Real';
  if Distribution = diUniform
    then Result := Result + '   U( '
    else Result := Result + '   N( ';  if MinConstrained
    then Result := Result + FloatToStr(MinValue) + ' ; '
    else Result := Result + '-Inf ; ';
  if MaxConstrained
    then Result := Result + FloatToStr(MaxValue) + ' )'
    else Result := Result + '+Inf )';
  Result := Result + Linker.Print;
end;

function TRealAttr.StrSample: string;
begin
    Result := FloatToStr(Sample);
end;

procedure TRealAttr.SetVal(const V: extended);
begin
  if not IsNAN(V) then
    begin
      if V < MinSample then MinSample := V;
      if V > MaxSample then MaxSample := V;
    end;
end;

function TRealAttr.Sample: extended;
var
  M, S: extended;
begin
  if Distribution = diUniform
    then Result := MinValue + Random(Abs(Trunc(MaxValue) - Trunc(MinValue))) + Random
    else
      begin
        M := Abs(MaxValue - MinValue)/2;
        S := Sqrt(M);
        Result := MinValue + RandG(M, S);
      end;
    SetVal(Result);
end;

{ TIntegerAttr }

constructor TIntegerAttr.Create(const aName: string; aDataSet: TDataSet);
begin
  inherited;
  MinConstrained := false;
  MaxConstrained := false;
  MinSample := MaxValue;
  MaxSample := MinValue;
end;

function TIntegerAttr.Export: string;
begin
  Result := '@attribute ' + Name + ' Integer';
end;

function TIntegerAttr.GetData(const Val: extended): string;
begin
  if IsUnknown(Val)
    then Result := UNKNOWN
    else Result := IntToStr(Trunc(Val));
end;

function TIntegerAttr.GetMaxSample: integer;
begin
  Result := Trunc(fMaxSample);
end;

function TIntegerAttr.GetMaxValue: integer;
begin
  Result := Trunc(fMaxValue);
end;

function TIntegerAttr.GetMinSample: integer;
begin
  Result := Trunc(fMinSample);
end;

function TIntegerAttr.GetMinValue: integer;
begin
  Result := Trunc(fMinValue);
end;

function TIntegerAttr.Print: string;
begin
  Result := Name + ': Entero';
  if Distribution = diUniform
    then Result := Result + '   U( '
    else Result := Result + '   N( ';
  if MinConstrained
    then Result := Result + IntToStr(MinValue) + ' ; '
    else Result := Result + '-Inf ; ';
  if MaxConstrained
    then Result := Result + IntToStr(MaxValue) + ' )'
    else Result := Result + '+Inf )';
  Result := Result + Linker.Print;
end;

procedure TDataSetDesigner.FormCreate(Sender: TObject);
begin
  DataSet := TDataSet.Create;
  DataSet.InstanceNumber := EditInstancesNumber.Value;
  DecimalSeparator := '.';
  Application.UpdateFormatSettings := false;
end;

procedure TDataSetDesigner.FormDestroy(Sender: TObject);
begin
  DataSet.Free;
end;

procedure TDataSetDesigner.EditReal(Attr: TRealAttr);
begin
  with FormReal do
    begin
      if Attr.MinConstrained
        then
          begin
            RadioMinVal.Checked := true;
            EditRealValMin.Text := FloatToStr(Attr.MinValue);
          end
        else RadioMenosInf.Checked := true;
      if Attr.MaxConstrained
        then
          begin
            RadioMaxVal.Checked := true;
            EditRealValMax.Text := FloatToStr(Attr.MaxValue);
          end
        else RadioMasInf.Checked := true;
      ComboDistribution.ItemIndex := integer(Attr.Distribution);
      RuidoEdit.Value := Attr.Linker.Noise;
      ListAttributes(VarList.Items, Attr);
      ListDependences(DepenList.Items, Attr);
      SetTitle(Attr.Name);
      if ShowModal = mrOK then
        begin
          if RadioMinVal.Checked
            then
              begin
                Attr.MinConstrained := true;
                Attr.MinValue := StrToFloat(EditRealValMin.Text);
              end
            else Attr.MinConstrained := false;
          if RadioMaxVal.Checked
            then
              begin
                Attr.MaxConstrained := true;
                Attr.MaxValue := StrToFloat(EditRealValMax.Text);
              end
            else Attr.MaxConstrained := false;
          Attr.Distribution := TDistribution(ComboDistribution.ItemIndex);
          UpdateDependences(DepenList.Items, Attr);
          Attr.Linker.Noise := RuidoEdit.Value;
          DataSet.Insufflated := false;
        end;
    end;
end;

procedure TDataSetDesigner.UpDateListAttr(const index: integer);
var
  Attr: TAttribute;
begin
  Attr := TAttribute(ListAttr.Items.Objects[ListAttr.ItemIndex]);
  ListAttr.Items.Strings[index] := Attr.Print;
end;

procedure TDataSetDesigner.EditInteger(Attr: TIntegerAttr);
begin
  with FormEntero do
    begin
      if Attr.MinConstrained
        then
          begin
            RadioMinVal.Checked := true;
            EditEnteroValMin.Value := Attr.MinValue;
          end
        else RadioMenosInf.Checked := true;
      if Attr.MaxConstrained
        then
          begin
            RadioMaxVal.Checked := true;
            EditEnteroValMax.Value := Attr.MaxValue;
          end
        else RadioMasInf.Checked := true;
      ComboDistribution.ItemIndex := integer(Attr.Distribution);
      RuidoEdit.Value := Attr.Linker.Noise;
      ListAttributes(VarList.Items, Attr);
      ListDependences(DepenList.Items, Attr);
      SetTitle(Attr.Name);
      if ShowModal = mrOK then
        begin
          if RadioMinVal.Checked
            then
              begin
                Attr.MinConstrained := true;
                Attr.MinValue := EditEnteroValMin.Value;
              end
            else Attr.MinConstrained := false;
          if RadioMaxVal.Checked
            then
              begin
                Attr.MaxConstrained := true;
                Attr.MaxValue := EditEnteroValMax.Value;
              end
            else Attr.MaxConstrained := false;
          Attr.Distribution := TDistribution(ComboDistribution.ItemIndex);
          UpdateDependences(DepenList.Items, Attr);
          Attr.Linker.Noise := RuidoEdit.Value;
          DataSet.Insufflated := false;
        end;
    end;
end;

procedure TDataSetDesigner.EditSymbolic(Attr: TSymbolicAttr);
begin
  with FormSimbolico do
    begin
      EditSimbolico.Value :=Attr.NumberOfSymbols;
      RuidoEdit.Value := Attr.Linker.Noise;
      ListAttributes(VarList.Items, Attr);
      ListDependences(DepenList.Items, Attr);
      SetTitle(Attr.Name);
      if ShowModal = mrOK then
        begin
          Attr.NumberOfSymbols := EditSimbolico.Value;
          UpdateDependences(DepenList.Items, Attr);
          Attr.Linker.Noise := RuidoEdit.Value;
          DataSet.Insufflated := false;
        end;
    end;
end;

function TIntegerAttr.StrSample: string;
begin
  Result := IntToStr(Trunc(Sample));
end;

procedure TDataSetDesigner.EditInstancesNumberChange(Sender: TObject);
begin
  if EditInstancesNumber.text <> '' then
    if EditInstancesNumber.Value >= 0
      then
        begin
          DataSet.InstanceNumber := EditInstancesNumber.Value;
          fFileModified := true;
        end
      else EditInstancesNumber.Value := DataSet.InstanceNumber;
end;

procedure TDataSetDesigner.NewDBExecute(Sender: TObject);
begin
  if CanProceed then NewDataBase;
end;

procedure TDataSetDesigner.GenerateExecute(Sender: TObject);
begin
  DataSet.Insufflate;
  ExportDataSet(MemData.Lines);
end;

procedure TDataSetDesigner.NewRealExecute(Sender: TObject);
var
  Attr: TAttribute;
begin
  Attr := DataSet.AddRealAttribute;
  ListAttr.AddItem(Attr.Print, Attr);
  fFileModified := true;
end;

procedure TDataSetDesigner.NewIntExecute(Sender: TObject);
var
  Attr: TAttribute;
begin
  Attr := DataSet.AddIntegerAttribute;
  ListAttr.AddItem(Attr.Print, Attr);
  fFileModified := true;
end;

procedure TDataSetDesigner.NewSymbolicExecute(Sender: TObject);
var
  Attr: TAttribute;
begin
  Attr := DataSet.AddSymbolicAttribute;
  ListAttr.AddItem(Attr.Print, Attr);
  fFileModified := true;
end;

procedure TDataSetDesigner.EditAttrExecute(Sender: TObject);
var
  Attr: TAttribute;
begin
  if ListAttr.ItemIndex >= 0 then
    begin
      Attr := TAttribute(ListAttr.Items.Objects[ListAttr.ItemIndex]);
      if Attr is TRealAttr
        then EditReal(Attr as TRealAttr)
        else
          if Attr is TIntegerAttr
            then EditInteger(Attr as TIntegerAttr)
            else EditSymbolic(Attr as TSymbolicAttr);
      UpDateListAttr(ListAttr.ItemIndex);
      fFileModified := true;
    end;
end;

procedure TDataSetDesigner.ListAttrMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  ListAttr.ItemIndex := ListAttr.ItemAtPos(P, true);
end;

procedure TDataSetDesigner.ExportDataSet(Media: TStrings);
var
  Ai, Xi: integer;
  Row: string;
begin
  if DataSet.AttributesCount > 0 then
    begin
      Media.Clear;
      Media.Add('@relation ' + DataSet.Name);
      Media.Add('');
      for Ai := 0 to pred(DataSet.AttributesCount) do
        Media.Add(DataSet.Attribute[Ai].Export);
      Media.Add('');
      Media.Add('@data');
      with DataSet do
        for Xi := 0 to pred(InstanceNumber) do
          begin
            Row := '';
            for Ai := 0 to AttributesCount - 2 do
              Row := Row + Data[Ai, Xi] + ', ';
            Row := Row + Data[pred(AttributesCount), Xi];
            Media.Add(Row);
          end;
    end;
end;

procedure TDataSetDesigner.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  if DataSet.Insufflated
    then MemData.Font.Color := clBlack
    else MemData.Font.Color := clRed;
end;

procedure TDataSetDesigner.DeleteExecute(Sender: TObject);
var
  Attr: TAttribute;
begin
  if ListAttr.ItemIndex >= 0 then
    begin
      Attr := TAttribute(ListAttr.Items.Objects[ListAttr.ItemIndex]);
      ListAttr.DeleteSelected;
      DataSet.Delete(Attr);
      fFileModified := true;
    end;
end;

procedure TDataSetDesigner.ExportExecute(Sender: TObject);
begin
  SaveDialog1.Title := 'Exportar';
  SaveDialog1.Filter := 'Weka File';
  SaveDialog1.DefaultExt := 'arff';
  if SaveDialog1.Execute
    then MemData.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TDataSetDesigner.AnalyzeAllExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      FormAnalyzer.Memo.Clear;
      FormAnalyzer.Execute(DataSet);
      FormAnalyzer.Show;
    end;
end;

procedure TIntegerAttr.SetMaxSample(const Value: integer);
begin
  fMaxSample := Value;
end;

procedure TIntegerAttr.SetMaxValue(const Value: integer);
begin
  fMaxValue := Value;
end;

procedure TIntegerAttr.SetMinSample(const Value: integer);
begin
  fMinSample := Value;
end;

procedure TIntegerAttr.SetMinValue(const Value: integer);
begin
  fMinValue := Value;
end;

procedure TIntegerAttr.SetVal(const V: extended);
begin
  if not IsNAN(V) then
    begin
      if V < MinSample then MinSample := Trunc(V);
      if V > MaxSample then MaxSample := Trunc(V);
    end;
end;

function TIntegerAttr.Sample: extended;
var
  M, S: extended;
begin
  if Distribution = diUniform
    then Result := MinValue + Random(Abs(MaxValue - MinValue) + 1)
    else
      begin
        M := Abs(MaxValue - MinValue)/2;
        S := Sqrt(M);
        Result := MinValue + Trunc(RandG(M, S));
      end;
  SetVal(Result);
end;

{ TNumericAttr }

function TNumericAttr.ClassVal(const Ind: integer): string;
begin
  Result := IntToStr(Ind);
end;

function TNumericAttr.DataClass(const Val: extended): string;
var
  C: integer;
begin
  if IsUnknown(Val)
    then Result := UNKNOWN
    else
      begin
        if Val = fMinSample
          then C := 0
          else C := Pred(Ceil((Val - fMinSample) / Interval));
        Result := IntToStr(C);
      end;
end;

procedure TNumericAttr.LoadFromStream(S: TStream);
begin
  inherited;
  S.ReadBuffer(fDistribution, SizeOf(fDistribution));
  S.ReadBuffer(fMinConstrained, SizeOf(fMinConstrained));
  S.ReadBuffer(fMaxConstrained, SizeOf(fMaxConstrained));
  S.ReadBuffer(fMinValue, SizeOf(fMinValue));
  S.ReadBuffer(fMaxValue, SizeOf(fMaxValue));
  S.ReadBuffer(fMinSample, SizeOf(fMinSample));
  S.ReadBuffer(fMaxSample, SizeOf(fMaxSample));
  S.ReadBuffer(Interval, SizeOf(Interval));
end;

procedure TNumericAttr.SaveToStream(S: TStream);
var
  L: integer;
  CN: string;
begin
  CN := ClassName;
  L := Length(CN);
  S.WriteBuffer(L, SizeOf(L));
  S.WriteBuffer(Pointer(CN)^, L);
  inherited;
  S.WriteBuffer(fDistribution, SizeOf(fDistribution));
  S.WriteBuffer(fMinConstrained, SizeOf(fMinConstrained));
  S.WriteBuffer(fMaxConstrained, SizeOf(fMaxConstrained));
  S.WriteBuffer(fMinValue, SizeOf(fMinValue));
  S.WriteBuffer(fMaxValue, SizeOf(fMaxValue));
  S.WriteBuffer(fMinSample, SizeOf(fMinSample));
  S.WriteBuffer(fMaxSample, SizeOf(fMaxSample));
  S.WriteBuffer(Interval, SizeOf(Interval));
end;

function TNumericAttr.SetInterval: string;
var
  Range: extended;
begin
  //SetRoundMode(rmUp);
  Range := fMaxSample - fMinSample;
  Interval := Range * SAMPLESPERCELL / DataSet.InstanceNumber;
  fDiscretizationLevel := Round(Range / Interval);
  //if Interval = 0 then Interval := 1;
  Result := FloatToStr(Interval);
  {
  if Range mod DataSet.InstanceNumber > 0
    then inc(Interval);
  }
end;

procedure TNumericAttr.SetMaxConstrained(const Value: boolean);
begin
 fMaxConstrained := Value;
 fMaxValue := High(integer) / 2;
end;

procedure TNumericAttr.SetMinConstrained(const Value: boolean);
begin
  fMinConstrained := Value;
  fMinValue := Low(integer) / 2;
end;

{ TLinker }

function TLinker.Cycle(Attr: TAttribute): boolean;
var
  i: integer;
begin
  if Attr = fOutputAttr
    then Result := true
    else
      begin
        Result := false;
        if not Attr.Linker.Independent
          then
            begin
              i := 0;
              while (i < Attr.Linker.InputCount) and not Result do
                begin
                  Result := Cycle(Attr.Linker.Input[i]);
                  inc(i);
                end;
            end;
      end;
end;

constructor TLinker.Create(OutputAttr: TAttribute);
begin
  fInputAttrs := TList.Create;
  fOutputAttr := OutputAttr;
  SetLength(fWeights, 1);
  fWeights[0] := 100;  // Peso inicial del atributo por defecto: atributo de salida
end;

destructor TLinker.Destroy;
begin
  fInputAttrs.Free;
  fWeights := nil;
  if Assigned(fNormWeights) then Finalize(fNormWeights);
  if Assigned(fSamples) then Finalize(fSamples);
  inherited;
end;

function TLinker.ReadDependence(const x: integer): extended;
begin
  Convert(x);
  Result := Average;
  {
  if fOutputAttr is TRealAttr
    then Result := FloatToStr(Average) else
  if fOutputAttr is TIntegerAttr
    then Result := IntToStr(Round(Average))
    else Result := SYMBOLS[Round(Average)];
  }
end;

function TLinker.Average: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(fSamples) do
    Result := Result + fNormWeights[i] * fSamples[i];
  if not (fOutputAttr is TRealAttr)
    then
      begin
        SetRoundMode(rmNearest);
        Result := Round(Result);
      end;
end;

procedure TLinker.SetWeight(Attr: TAttribute; const Value: integer);
var
  i: integer;
begin
  i := fInputAttrs.IndexOf(Attr);
  if i >= 0 then fWeights[succ(i)] := Value;
  NormalizeWeights;
end;

function TLinker.SymToSym(AttrIn, AttrOut: TSymbolicAttr; const x: integer): extended;
begin
  Result := Trunc(1 + (AttrIn.FetchData(x) - 1)
   * (AttrOut.NumberOfSymbols) / (AttrIn.NumberOfSymbols));
end;

procedure TLinker.UnlinkAttr(Attr: TAttribute);
begin
  RemoveLink(fInputAttrs.IndexOf(Attr));
end;

procedure TLinker.LinkAttr(Attr: TAttribute; const Value: integer);
begin
  fInputAttrs.Add(Attr);
  SetLength(fWeights, succ(Length(fWeights)));
  fWeights[High(fWeights)] := Value;
  NormalizeWeights;
end;

procedure TLinker.RemoveWeight(const index: integer);
var
  w: TWeights;
  i: integer;
begin
  if (Length(fWeights) > 0) and (index >= 0) and (index <= High(fWeights)) then
    begin
      w := Copy(fWeights, 0, index);
      SetLength(w, pred(Length(fWeights)));
      for i := succ(index) to High(fWeights) do
        w[pred(i)] := fWeights[i];
      Finalize(fWeights);
      fWeights := w;
    end;
end;

procedure TLinker.Convert(const x: integer);
var
  i: integer;
  Attr: TAttribute;
begin
  SetLength(fSamples, succ(fInputAttrs.Count));
  fSamples[0] := fOutputAttr.Sample;
  if fOutputAttr is TNumericAttr
    then
      for i := 1 to fInputAttrs.Count do
        begin
          Attr := fInputAttrs.Items[pred(i)];
          if Attr is TNumericAttr
            then fSamples[i] := NumToNum(TNumericAttr(Attr), TNumericAttr(fOutputAttr), x)
            else fSamples[i] := SymToNum(TSymbolicAttr(Attr), TNumericAttr(fOutputAttr), x);
        end
    else // fOutputAttr is TSymbolicAttr
      for i := 1 to fInputAttrs.Count do
        begin
          Attr := fInputAttrs.Items[pred(i)];
          if Attr is TNumericAttr
            then fSamples[i] := NumToSym(TNumericAttr(Attr), TSymbolicAttr(fOutputAttr), x)
            else fSamples[i] := SymToSym(TSymbolicAttr(Attr), TSymbolicAttr(fOutputAttr), x);
        end
end;

procedure TLinker.NormalizeWeights;
var
  i: integer;
  TotalWeight: extended;
begin
  SetLength(fNormWeights, Length(fWeights));
  TotalWeight := 0;
  for i := 0 to High(fWeights) do
    TotalWeight := TotalWeight + fWeights[i];

  // Si el atributo es independiente entonces la influencia de su muestreo
  // no puede ser nula sino completa
  if TotalWeight = 0
    then
      begin
        fWeights[0] := 100;
        TotalWeight := 100;
      end;

  for i := 0 to High(fNormWeights) do
    fNormWeights[i] := fWeights[i] / TotalWeight;
end;

procedure TDataSetDesigner.ListAttributes(List: TStrings; Attr: TAttribute);
var
  i: integer;
begin
  List.Clear;
  for i := 0 to pred(DataSet.PosAttr(Attr)) do
    if not Attr.Linker.IsInput(DataSet.Attribute[i])
      then List.AddObject(TAttribute(DataSet.Attribute[i]).Name, DataSet.Attribute[i]);
end;

procedure TDataSetDesigner.ListDependences(List: TStrings; Attr: TAttribute);
var
  i: integer;
  A: TAttribute;
begin
  List.Clear;
  if Assigned(Attr.Linker) then
    for i := 0 to pred(Attr.Linker.InputCount) do
      begin
        A := Attr.Linker.Input[i];
        List.AddObject(Attr.Linker.Weight[i] + '    ' + A.Name, A);
      end;
end;

procedure TDataSetDesigner.UpdateDependences(List: TStrings; Attr: TAttribute);
var
  i: integer;
begin
  Attr.Linker.ClearInputs;
  for i := 0 to pred(List.Count) do
    if not Attr.Linker.IsInput(TAttribute(List.Objects[i])) then
       Attr.Linker.LinkAttr(TAttribute(List.Objects[i]),
         StrToInt(copy(List.Strings[i], 1, pred(pos(' ', List.Strings[i])))));
end;

function TLinker.GetInputCount: integer;
begin
  Result := fInputAttrs.Count;
end;

function TLinker.GetInput(const i: integer): TAttribute;
begin
  Result := fInputAttrs[i];
end;

function TLinker.GetWeight(const i: integer): string;
begin
  Result := FloatToStr(fWeights[succ(i)]);
end;

function TLinker.IsInput(Attr: TAttribute): boolean;
var
  i: integer;
begin
  i := 0;
  while (i < fInputAttrs.Count) and (Attr <> fInputAttrs[i])
    do inc(i);
  Result := i < fInputAttrs.Count;
end;

procedure TLinker.RemoveLink(const index: integer);
begin
  fInputAttrs.Delete(index);
  RemoveWeight(index);
  NormalizeWeights;
end;

function TLinker.GetIndependent: boolean;
begin
  Result := fInputAttrs.Count = 0;
end;

function TLinker.NumToNum(AttrIn, AttrOut: TNumericAttr; const x: integer): extended;
begin
  Result := AttrOut.MinValue + (AttrIn.FetchData(x) - AttrIn.MinValue)
   * (AttrOut.MaxValue - AttrOut.MinValue) / (AttrIn.MaxValue - AttrIn.MinValue);
end;

function TLinker.SymToNum(AttrIn: TSymbolicAttr; AttrOut: TNumericAttr; const x: integer): extended;
begin
  Result := AttrOut.MinValue + (AttrIn.FetchData(x) - 1)
   * (AttrOut.MaxValue - AttrOut.MinValue) / (AttrIn.NumberOfSymbols);
end;

function TLinker.NumToSym(AttrIn: TNumericAttr; AttrOut: TSymbolicAttr; const x: integer): extended;
begin
  Result := Trunc(1 + (AttrIn.FetchData(x) - AttrIn.MinValue)
   * (AttrOut.NumberOfSymbols) / (AttrIn.MaxValue - AttrIn.MinValue));
end;

function TLinker.GetNoise: integer;
begin
  Result := fWeights[0];
end;

procedure TLinker.SetNoise(const Value: integer);
begin
  fWeights[0] := Value;
  NormalizeWeights;
end;

procedure TLinker.ClearInputs;
begin
  fInputAttrs.Clear;
  SetLength(fWeights, 1);
end;

function TLinker.Print: string;
var
  i: integer;
begin
  if InputCount = 0
    then Result := ''
    else
      begin
        Result := '     Depend:';
        for i := 0 to pred(InputCount) do
          Result :=  Result + '   ' + Weight[i] + '%  ' + Input[i].Name;
        Result := Result + '     Ruido: ' + IntToStr(fWeights[0]) + '%';
      end;
end;

procedure TDataSetDesigner.InformationTheoreticAnalysisExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      FormAnalyzer.Memo.Clear;
      FormAnalyzer.InformationTheoryAnalysis(DataSet);
      FormAnalyzer.Show;
    end;
end;

procedure TDataSetDesigner.RoughSetsAnalysisExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      FormAnalyzer.Memo.Clear;
      FormAnalyzer.RoughSetAnalysis(DataSet);
      FormAnalyzer.Show;
    end;
end;

procedure TDataSetDesigner.ViewAnalysisSheetExecute(Sender: TObject);
begin
  ViewAnalysisSheet.Checked := not ViewAnalysisSheet.Checked;
  if ViewAnalysisSheet.Checked
    then
      begin
        FormAnalyzer.WindowState := wsMaximized;
        FormAnalyzer.Show;
      end
    else FormAnalyzer.Hide;
end;

procedure TDataSetDesigner.CleanAnalysisSheetExecute(Sender: TObject);
begin
  FormAnalyzer.Memo.Clear;
end;

procedure TDataSetDesigner.SaveToFile(const FileName: string);
var
  S: TFilestream;
begin
  S := TFilestream.Create(FileName, fmCreate);
  DataSet.SaveToStream(S);
  S.Free;
end;

procedure TLinker.SaveToStream(S: TStream);
var
  i, L: integer;
  N: string;
begin
  S.WriteBuffer(fInputAttrs.Count, SizeOf(fInputAttrs.Count));
  for i := 0 to pred(fInputAttrs.Count) do
    begin
      N := TAttribute(fInputAttrs.Items[i]).Name;
      L := Length(N);
      S.WriteBuffer(L, SizeOf(L));
      S.WriteBuffer(Pointer(N)^, L);
    end;
  S.WriteBuffer(Pointer(fWeights)^, Length(fWeights) * SizeOf(integer));
end;

procedure TDataSetDesigner.LoadFromFile(const FileName: string);
var
  S: TFilestream;
begin
  NewDBExecute(Self);
  S := TFilestream.Create(FileName, fmOpenRead);
  DataSet.LoadFromStream(S);
  S.Free;
  UpdateDisplay;
  if DataSet.Insufflated
    then ExportDataSet(MemData.Lines);
end;

procedure TLinker.LoadFromStream(S: TStream);
var
  C, i, L: integer;
  N: string;
begin
  S.ReadBuffer(C, SizeOf(C));
  for i := 0 to pred(C) do
    begin
      S.ReadBuffer(L, SizeOf(L));
      SetString(N, nil, L);
      S.ReadBuffer(Pointer(N)^, L);
      fInputAttrs.Add(fOutputAttr.DataSet.FindAttr(N));
    end;
  SetLength(fWeights, succ(C));
  SetLength(fNormWeights, succ(C));
  S.ReadBuffer(Pointer(fWeights)^, Length(fWeights) * SizeOf(integer));
  NormalizeWeights;
  //S.ReadBuffer(Pointer(fNormWeights)^, Length(fNormWeights) * SizeOf(extended));
end;

procedure TDataSetDesigner.SaveAsExecute(Sender: TObject);
begin
  SaveDialog1.Title := 'Guardar como';
  SaveDialog1.Filter := 'DataScultor File|*.dsf';
  SaveDialog1.DefaultExt := 'dsf';
  if SaveDialog1.Execute then
      begin
        SaveToFile(SaveDialog1.FileName);
        fFileName := SaveDialog1.FileName;
        Caption := MainFormTitle + ' - ' + GetFileName(fFileName);
        fFileModified := false;
        fProceed := true;
      end;
end;

procedure TDataSetDesigner.OpenExecute(Sender: TObject);
begin
  OpenDialog1.Title := 'Abrir';
  OpenDialog1.Filter := 'DataSculptor|*.dsf|Todos|*.*';
  OpenDialog1.DefaultExt := 'dsf';
  if OpenDialog1.Execute and CanProceed
    then
      begin
        NewDataBase;
        LoadFromFile(OpenDialog1.FileName);
        fFileModified := false;
        fFileName := OpenDialog1.FileName;
        Caption := MainFormTitle + ' - ' + GetFileName(fFileName);
      end;
end;

procedure TDataSetDesigner.UpdateDisplay;
var
  i: integer;
begin
  ListAttr.Clear;
  for i := 0 to pred(DataSet.AttributesCount) do
    ListAttr.AddItem(DataSet.Attribute[i].Print, DataSet.Attribute[i]);
  if DataSet.InstanceNumber = 0
    then DataSet.InstanceNumber := DefaultInstancesNumber
    else
      begin
        EditInstancesNumber.OnChange := nil;
        EditInstancesNumber.Value := DataSet.InstanceNumber;
        EditInstancesNumber.OnChange := EditInstancesNumberChange;
      end;
end;

procedure TDataSetDesigner.SaveExecute(Sender: TObject);
begin
  if fFileName = ''
    then SaveAsExecute(Self)
    else
      begin
        SaveToFile(fFileName);
        fFileModified := false;
        fProceed := true;
      end;
end;

function TDataSetDesigner.GetFileName(const RawFileName: string): string;
var
  S: string;
  C: integer;
begin
  S := ExtractFileName(RawFileName);
  C := LastDelimiter('.', S);
  if C = 0
    then Result := S
    else Result := copy(S, 1, pred(C));
end;

procedure TDataSetDesigner.NewDataBase;
begin
  DataSet.Free;
  DataSet := TDataSet.Create;
  EditInstancesNumber.Value := DefaultInstancesNumber;
  DataSet.InstanceNumber := DefaultInstancesNumber;
  ListAttr.Clear;
  MemData.Clear;
  fFilename := '';
  Caption := MainFormTitle;
  fFileModified := false;
end;

function TDataSetDesigner.CanProceed: boolean;
var
  MR: word;
begin
  // Si se ha comenzado un diseño, preguntar si no desea guardarlo
  fProceed := false;
  if fFileModified
    then MR := MessageDlg('¿Desea guardar la base de datos actual?', mtConfirmation, mbYesNoCancel, 0)
    else MR := mrNo;
  if MR = mrCancel
    then Result := false
    else
      if MR = mrYes
        then
          begin
            SaveExecute(Self);
            Result := fProceed;
          end
        else Result := true;
end;

procedure TDataSetDesigner.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if CanProceed
    then CanClose := true
    else CanClose := false;
end;

procedure TDataSetDesigner.BasicAnalysisExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      FormAnalyzer.Memo.Clear;
      FormAnalyzer.BasicMeasures(DataSet);
      FormAnalyzer.Show;
    end;
end;

procedure TDataSetDesigner.StatisticAnalysisExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      FormAnalyzer.Memo.Clear;
      FormAnalyzer.StatisticAnalysis(DataSet);
      FormAnalyzer.Show;
    end;
end;

procedure TDataSetDesigner.ImportExecute(Sender: TObject);
var
  Parser: TParser;
begin
  OpenDialog1.Title := 'Importar';
  OpenDialog1.Filter := 'Weka File|*.arff|C4.5 File|*.names|Todos|*.*';
  OpenDialog1.DefaultExt := 'arff';
  if OpenDialog1.Execute and CanProceed
    then
      begin
        NewDataBase;
        MemData.Lines.LoadFromFile(OpenDialog1.FileName);
        fFileModified := false;
        fFileName := OpenDialog1.FileName;
        Caption := MainFormTitle + ' - ' + GetFileName(fFileName);
        Parser := TParser.Create(DataSet);
        if ExtractFileExt(fFileName) = '.arff'
          then Parser.ParseStrings(MemData.Lines)
          else ImportXDataFile(Parser);
        Parser.Free;
        UpdateDisplay;
      end;
end;

{ TParser }

constructor TParser.Create(Dataset: TDataSet);
begin
  FDataSet := DataSet;
  Ind := 1;
end;

procedure TParser.GoAhead;
begin
  if TextLine[Ind] = Comment
    then Ind := succ(Length(TextLine))
    else inc(Ind);
  LineBreak := false;
  while (Ind > Length(TextLine)) and (Line < Text.Count) do
    begin
      Ind := 1;
      inc(Line);
      if Line < Text.count
        then TextLine := Trim(Text[Line])
        else TextLine := '';
      LineBreak := true;
    end;
  if Line = Text.count
    then EOF := true;
end;

procedure TParser.Init(aText: TStrings);
begin
  Text := aText;
  TextLine := Trim(Text[Line]);
end;

function TParser.NextToken: string;
begin
  while not EOF and (TextLine[Ind] in Separators) do GoAhead;
  Result := '';
  if not EOF then
  if TextLine[Ind] in Operators then
    begin
      Result := TextLine[Ind];
      GoAhead;
    end else
    repeat
      Result := Result + TextLine[Ind];
      GoAhead;
    until EOF or LineBreak or ((TextLine[Ind] in (Separators + Operators)));
  FToken := Result;
end;

function TParser.ParseArffData: boolean;
begin
  if Lowercase(Token) = '@data'
    then Result := ParseData
    else Result := false;
end;

function TParser.ParseAttributes: boolean;
var
  AttrName: string;
  Attr: TAttribute;
begin
  Result := false;
  while Lowercase(Token) = '@attribute' do
    begin
      NextToken;
      Result := ParseIdent(AttrName);
      if Result then
        if Lowercase(Token) = 'real' then
          begin
            Attr := FDataSet.AddRealAttribute;
            Attr.Name := AttrName;
            NextToken;
          end else
        if Lowercase(Token) = 'integer' then
          begin
            Attr := FDataSet.AddIntegerAttribute;
            Attr.Name := AttrName;
            NextToken;
          end else
        if Token = '{' then
          begin
            NextToken;
            Attr := FDataSet.AddSymbolicAttribute;
            Attr.Name := AttrName;
            TSymbolicAttr(Attr).UseExplicitSymbols;
            while Token <> '}' do
              begin
                TSymbolicAttr(Attr).AddSymbol(Token);
                NextToken;
              end;
            if TSymbolicAttr(Attr).NumberOfSymbols > 0
              then NextToken
              else Result := false
          end else Result := false;
    end;
end;

function TParser.ParseData: boolean;
var
  i, j: integer;
  val: extended;
begin
  NextToken;
  j := 0;
  Result := true;
  SetLength(FDataSet.fDataStore, FDataSet.AttributesCount);
  while Result and not EOF do
    begin
      for i := 0 to pred(FDataSet.AttributesCount) do
        begin
          if FDataSet.Attribute[i] is TRealAttr
            then Result := ParseReal(val) else
          if FDataSet.Attribute[i] is TIntegerAttr
            then Result := ParseInteger(val) else
          if FDataSet.Attribute[i] is TSymbolicAttr
            then Result := ParseSymbolic(TSymbolicAttr(FDataSet.Attribute[i]), val);
          if Result then
            begin
              SetLength(FDataSet.fDataStore[i], succ(j));
              FDataSet.fDataStore[i, j] := val;
              if FDataSet.Attribute[i] is TNumericAttr then
                TNumericAttr(FDataSet.Attribute[i]).SetVal(val);
            end;
          if not Result then exit;
        end;
      inc(j);
    end;
  if Result then
    with FDataSet do
      begin
        fInstanceNumber := j;
        for i := 0 to pred(AttributesCount) do
          if Attribute[i] is TNumericAttr
            then TNumericAttr(Attribute[i]).SetInterval;
        Insufflated := true;
      end;
end;

function TParser.ParseIdent(var Ident: string): boolean;
begin
  if not EOF and not (Token[1] in Operators) then
    begin
      Ident := Token;
      NextToken;
      Result := true;
    end
    else Result := false;
end;

function TParser.ParseInteger(var fval: extended): boolean;
begin
  if Token = '?'
    then
      begin
        SetUnknown(fval);
        Result := true;
        NextToken;
      end
    else
      try
        fval := StrToInt(Token);
        Result := true;
        NextToken;
      except
        Result := false;
      end;
end;

function TParser.ParseReal(var fval: extended): boolean;
begin
  if Token = '?'
    then
       begin
        SetUnknown(fval);
        Result := true;
        NextToken;
      end
   else
      try
        fval := StrToFloat(Token);
        Result := true;
        NextToken;
      except
        Result := false;
      end;
end;

function TParser.ParseRelation: boolean;
var
  RelName: string;
begin
  if Lowercase(Token) = '@relation' then
    begin
      NextToken;
      Result := ParseIdent(RelName);
      DataSet.Name := RelName;
    end
    else Result := false;
end;

function TParser.ParseStrings(aText: TStrings): boolean;
begin
  Init(aText);
  NextToken;
 Result := ParseRelation and ParseAttributes and ParseArffData;
end;

function TParser.ParseSymbolic(Attr: TSymbolicAttr; var fval: extended): boolean;
begin
  if Token = '?'
    then
      begin
        SetUnknown(fval);
        Result := true;
        NextToken;
      end
    else
      begin
        fval := Attr.PosSymbol(Token);
        if fval = -1 then  // Tal vez el símbolo esté codificado en decimal
          try
            fval := Attr.PosSymbol(IntToStr(Trunc(StrToFloat(Token))))
          except
            fval := -1;
          end;
        if fval = -1
          then Result := false
          else
            begin
              Result := true;
              NextToken;
            end;
      end;
end;

procedure TParser.ParseSymbolList(const AName: string);
var
  Attr: TAttribute;
begin
  Attr := FDataSet.AddSymbolicAttribute;
  Attr.Name := AName;
  TSymbolicAttr(Attr).UseExplicitSymbols;
  TSymbolicAttr(Attr).AddSymbol(Token);
  while not LineBreak do
    begin
      NextToken;
      TSymbolicAttr(Attr).AddSymbol(Token);
    end;
end;

function TParser.ParseXData(aText: TStrings): boolean;
begin
  Text := aText;
  Restart;
  Result := ParseData;
end;

function TParser.ParseXNames(aText: TStrings): boolean;
var
  AttrName: string;
  Attr: TAttribute;
begin
  Init(aText);
  // Saltar la declaración de las clases
  repeat
    NextToken;
  until LineBreak;
  // Leer la declaración de los atributos
  Result := true;
  while Result and not EOF do
    begin
      NextToken;
      Result := ParseIdent(AttrName);
      if Result then
        if Token = 'continuous'
          then
            begin
              Attr := FDataSet.AddRealAttribute;
              Attr.Name := AttrName;
            end
          else ParseSymbolList(AttrName);
    end;
  // Regresar atrás para leer la declaración de las clases
  Restart;
  NextToken;
  ParseSymbolList('Class');
end;

procedure TParser.Restart;
begin
  Line      := 0;
  Ind       := 1;
  LineBreak := false;
  EOF       := false;
  FToken    := '';
  TextLine := Trim(Text[Line]);
end;

procedure TDataSetDesigner.ImportXDataFile(Parser: TParser);
var
  S: TStringList;
  fn: TFileName;
begin
  Parser.ParseXNames(MemData.Lines);
  fn := ChangeFileExt(OpenDialog1.FileName, '');
  S := TStringList.Create;
  try
    try
      S.LoadFromFile(fn);
    except
      fn := ChangeFileExt(OpenDialog1.FileName, '.data');
      S.LoadFromFile(fn);
    end;
    Parser.ParseXData(S);
    MemData.Lines.Add('');
    MemData.Lines.Add('-  Data  -');
    MemData.Lines.Add('');
    MemData.Lines.AddStrings(S);
  finally
    S.Free;
  end;
end;

procedure TDataSetDesigner.UnknownValExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      DataSet.UnknownValuesProcess;
      ExportDataSet(MemData.Lines);
    end;
end;

procedure TDataSetDesigner.BinarizationExecute(Sender: TObject);
begin
  if DataSet.Insufflated then
    begin
      DataSet.BinarizationProcess;
      ExportDataSet(MemData.Lines);
      UpdateDisplay;
      fFileModified := true;
    end;
end;

end.
