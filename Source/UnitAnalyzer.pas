unit UnitAnalyzer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Main, cMatrix, ComCtrls, ExtCtrls;

type
  TInteger = class
    private
      FValue: integer;
      procedure SetValue(const Value: integer);
    public
      constructor Create(const Value: integer);
      property Value: integer read FValue write SetValue;
  end;

  TIntSet = class
    private
      fList: TList;
      function GetCardinality: integer;
      function GetItems(const Index: integer): integer;
    public
      constructor Create;
      destructor  Destroy; override;
      procedure Add(const Item: integer);
      function Clone: TIntSet;
      function Contain(const Item: integer): boolean;
      function Empty: boolean;
      function SubSet(aSet: TIntSet): boolean;
      function Union(aSet: TIntSet): TIntSet;
      procedure Append(aSet: TIntSet);
      function Intersection(aSet: TIntSet): TIntSet;
      function Difference(aSet: TIntSet): TIntSet;
      function Print: string;
      property Cardinality: integer read GetCardinality;
      property Items[const Index: integer]: integer read GetItems;  default;
  end;

  TIntSetSet = class
    private
      fList: TList;
      function GetItems(const Index: integer): TIntSet;
      function GetCardinality: integer;
    public
      constructor Create;
      destructor  Destroy; override;
      procedure Add(const Item: TIntSet);
      procedure Delete(const Ind: integer);
      procedure Pack;
      function CrossProduct(aSet: TIntSetSet): TIntSetSet;
      function Clone: TIntSetSet;
      function Empty: boolean;
      function Print: string;
      function SetOfItem(const x: integer): TIntSet;
      property Cardinality: integer read GetCardinality;
      property Items[const Index: integer]: TIntSet read GetItems; default;
  end;

  TAnalysisThread = class;
  TVector = array of extended;
  TRoughSetAnalyzer = class
    private
      FDataSet: TDataSet;
      fRunningThread: TAnalysisThread;
      FEqRel: TList;
      fAttrSet: TIntSet;
      fClassSet: TIntSet;
      fUniverse: TIntSet;
      fPositive: TIntSetSet;
      fNegative: TIntSetSet;
      fBoundary: TIntSetSet;
      fDiscernability: TIntSetset;
      fUpperApprox: TIntSetset;
      fLowerApprox: TIntSetset;
      FTargetClass: integer;
      fWeights1: TVector;
      fTotalWeight1: extended;
      fWeights2: TVector;
      fTotalWeight2: extended;
      fApproxAccuracy: TVector;
      fApproxQuality: TVector;
      fDependency: TVector;
      fOnProgress: TNotifyEvent;
      procedure SetDataSet(const Value: TDataSet);
      function GetEqRelation(const i: integer): TIntSetSet;
      procedure SetTargetClass(const Value: integer);
      procedure ClearRelations;
      procedure ClearVars;
      procedure BuildEquivalenceRelationList;
      function EquivalenceRelation(const AttrIndex: integer): TIntSetSet;
      function InstancesXAttrVal(AttrIndex: integer; const ClassVal: string): TIntSet;
      function Discernability(AttrList: TIntSet): TIntSetSet;
      function UpperApprox(Attrs, Instances: TIntSet): TIntSet;
      function LowerApprox(Attrs, Instances: TIntSet): TIntSet;
      function AllAttrSet: TIntSet;
      function ClassSet: TIntSet;
      function Universe: TIntSet;
      function GetItemsOfClass(const i: integer): TIntSet;
      procedure ComputWeights;
    public
      constructor Create(aDataSet: TDataSet; A: TAnalysisThread);
      destructor  Destroy;  override;
      procedure Init;
      procedure Analyze;
      procedure ComputApproximations;
      procedure ComputMeasures;
      function GetMaxProg: integer;
      function PrintAttributeSet: string;
      function PrintClassSet: string;
      function PrintUniverse: string;
      function PrintDiscernability: string;
      function PrintLowerApprox: string;
      function PrintUpperApprox: string;
      function PrintPositive: string;
      function PrintNegative: string;
      function PrintBoundary: string;
      function PrintVector(Vector: TVector): string;
      property DataSet: TDataSet read FDataSet write SetDataSet;
      property EqRelation[const i: integer]: TIntSetSet read GetEqRelation;
      property TargetClass: integer read FTargetClass write SetTargetClass;
      property ItemsOfClass[const i: integer]: TIntSet read GetItemsOfClass;
      property OnProgress: TNotifyEvent read fOnProgress write fOnProgress;
    public  // Measures
      function ApproxAccuracy(const ClassIndex: integer): extended;  // Precisión de la Aproximación
      function ClassifAccuracy: extended;  // Precisión Media de la clasificación
      function WeightedClassifAccuracy: extended;  // Precisión ponderada de la clasificación
      function GenClassifAccuracy: extended;  // Precisión Generalizada de la clasificación
      function ApproxQuality(const ClassIndex: integer): extended;  // Calidad de la Aproximación
      function ClassifQuality: extended;  // Calidad Media de la clasificación
      function GenClassifQuality: extended;  // Calidad Generalizada de la clasificación
      function GenApproxRatio: extended;  // Coeficiente de Aproximación General
      function Dependency(const ClassIndex: integer): extended;
      function RoughMembership(const instance: integer; AClassSet: TIntSet): extended;
      function MeanClassRoughMembership(AClassSet: TIntSet): extended;  // media de la pertenencia aproximada por clases
      function RoughInvolvement(const instance: integer; AClassSet: TIntSet): extended;
      function MeanClassRoughInvolvement(AClassSet: TIntSet): extended; //media del compromiso aproximado por clases
  end;

  EMetric = class(Exception);
  T2DMatrixSingle = array of array of extended;
  TInformTheoryAnalyzer = class
    private
      FDataSet: TDataSet;
      fRunningThread: TAnalysisThread;
      fAMICalc: boolean;
      fOnProgress: TNotifyEvent;
      procedure SetDataSet(const Value: TDataSet);
      procedure InitPriorProbMatrix; // Try
      procedure InitJointProbMatrix;
      procedure InitEntropyMatrix;
      procedure InitJointEntropyMatrix;
      procedure InitMutualInfoMatrix;
      procedure FinalizePriorProbMatrix;
      procedure FinalizeJointProbMatrix;
      procedure FinalizeEntropyMatrix;
      procedure FinalizeJointEntropyMatrix;
      procedure FinalizeMutualInfoMatrix;
      procedure MakePriorProbMatrix;
      procedure MakeJointProbMatrix;
      procedure MakeEntropyMatrix;
      procedure MakeJointEntropyMatrix;
      procedure MakeMutualInfoMatrix;
    public
      fPriorProbMatrix: T2DMatrixSingle;
      fJointProbMatrix: array of array of array of array of extended;  // Attr1, Va1, Attr2, Val2
      fEntropyMatrix: array of extended;
      fJointEntropyMatrix: T2DMatrixSingle;
      fMutualInfoMatrix: T2DMatrixSingle;
      AMI: extended;
      constructor Create(aDataSet: TDataSet; A: TAnalysisThread);
      destructor  Destroy;  override;
      procedure Analyze;
      function GetMaxProg: integer;
      property OnProgress: TNotifyEvent read fOnProgress write fOnProgress;
      property DataSet: TDataSet read FDataSet write SetDataSet;
      function PriorProbability(const AttrIndex, ClassVal: integer): extended;
      function JointProbability(const Attr1, Value1, Attr2, Value2: integer): extended;
      function Entropy(const AttrIndex: integer): extended;
      function JointEntropy(const Attr1, Attr2: integer): extended;
      function ClassEntropy: extended;
      function AttrMeanEntropy: extended;
      function MutualInformation(const Attr1, Attr2: integer): extended;
      function AverageMutualInformation: extended;
      function ENAttr: extended;
      function Noisiness: extended;
  end;

  TStatisticalAnalizer = class
    private
      FDataSet: TDataSet;
      fRunningThread: TAnalysisThread;
      FAttrValMatrices: array of TMatrix;
      FCovarMatrices: array of TMatrix;
      N: array of integer;
      MeanAttrM: TMatrix;
      FPooledCovar: TMatrix;
      procedure SetDataSet(const Value: TDataSet);
      function GetInverter(const Dim: integer): TMatrix;
      function NiSum: extended;
    public
      constructor Create(aDataSet: TDataSet; A: TAnalysisThread);
      procedure SetParams;
      function MeanAttr(const Ai: integer): extended;
      function MeanAttrVector: TMatrix;
      function AttrValMatrix(const Ci: integer): TMatrix;
      procedure SetAttrValMatrices;
      function CovarianceMatrix(const Ci: integer): TMatrix;
      procedure SetCovarMatrices;
      function PooledCovarMatrix: TMatrix;
      function MStatistic: extended;
      function SDRatio: extended;
      property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

  TAnalysisType = (atBasic, atRoughSet, atInformationTheory, atStatistic, atAll);
  TFormAnalyzer = class;
  TAnalysisThread = class(TThread)
    private
      F: TFormAnalyzer;
      DataSet: TDataSet;
      MaxProg: integer;
      AnalysisType: TAnalysisType;
      fStop: boolean;
      procedure ProgressBar(Sender: TObject);
      procedure BasicMeasures(DataSet: TDataSet);
      procedure RoughSetAnalysis(DataSet: TDataSet);
      procedure InformationTheoryAnalysis(DataSet: TDataSet);
      procedure StatisticAnalysis(DataSet: TDataSet);
      procedure AllAnalysis(DataSet: TDataSet);
    public
      constructor Create(Form: TFormAnalyzer);
      procedure ExecuteAnalysis(D: TDataSet; A: TAnalysisType);
      procedure Execute; override;
      property Stop: boolean read fStop write fStop;
  end;

  TFormAnalyzer = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Panel2: TPanel;
    StopBtn: TButton;
    procedure FormHide(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    AnalysisThread: TAnalysisThread;
    procedure PrintMatrix(DataSet: TDataSet; const aTitle: string; aMatrix: T2DMatrixSingle);
  public
    procedure BasicMeasures(DataSet: TDataSet);
    procedure RoughSetAnalysis(DataSet: TDataSet);
    procedure InformationTheoryAnalysis(DataSet: TDataSet);
    procedure StatisticAnalysis(DataSet: TDataSet);
    procedure Execute(DataSet: TDataSet);
  end;

var
  FormAnalyzer: TFormAnalyzer;

implementation

uses Math;

{$R *.dfm}

{ TInformTheoryAnalyzer }

function TInformTheoryAnalyzer.AverageMutualInformation: extended;
var
  i: integer;
  Sum: extended;
begin
  if DataSet.AttributesCount > 1
    then
      begin
        Sum := 0;
        for i := 0 to DataSet.AttributesCount - 2 do
          Sum := Sum + fMutualInfoMatrix[pred(DataSet.AttributesCount), i];
        Result := Sum / pred(DataSet.AttributesCount);
      end
    else Result := 0;
  AMI := Result;
  fAMICalc := true;
end;

function TInformTheoryAnalyzer.ENAttr: extended;
begin
  if not fAMICalc
    then AverageMutualInformation;
  if AMI = 0
    then Result := 0
    else Result := ClassEntropy / AMI;
end;

function TInformTheoryAnalyzer.Entropy(const AttrIndex: integer): extended;
var
  i: integer;
  PriorProb: extended;
begin
  Result := 0;
  for i := 0 to pred(DataSet.AttrCardinality(AttrIndex)) do
    begin
      PriorProb := fPriorProbMatrix[AttrIndex, i];
      if PriorProb > 0
        then Result := Result - PriorProb * log2(PriorProb);
    end;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

function TInformTheoryAnalyzer.JointEntropy(const Attr1, Attr2: integer): extended;
var
  i, j: integer;
  PriorProb: extended;
begin
  Result := 0;
  for i := 0 to pred(DataSet.AttrCardinality(Attr1)) do
    for j := 0 to pred(DataSet.AttrCardinality(Attr2)) do
      begin
        PriorProb := fJointProbMatrix[Attr1, i, Attr2, j];
        if PriorProb > 0
          then Result := Result - PriorProb * log2(PriorProb);
      end;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

function TInformTheoryAnalyzer.JointProbability(const Attr1, Value1, Attr2, Value2: integer): extended;
var
  i, Count: integer;
begin
  Count := 0;
  for i := 0 to pred(DataSet.InstanceNumber) do
    if  (DataSet.DataClass(Attr1, i) = DataSet.ClassValue(Attr1, Value1))
    and (DataSet.DataClass(Attr2, i) = DataSet.ClassValue(Attr2, Value2))
      then inc(Count);
  Result := Count / DataSet.InstanceNumber;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

function TInformTheoryAnalyzer.AttrMeanEntropy: extended;
var
  i: integer;
  Sum: extended;
begin
  if DataSet.AttributesCount > 1
    then
      begin
        Sum := 0;
        for i := 0 to DataSet.AttributesCount - 2 do
          Sum := Sum + fEntropyMatrix[i];
        Result := Sum / pred(DataSet.AttributesCount);
      end
    else Result := 0;
  // MaxProg + DataSet.AttributesCount - 1
end;

function TInformTheoryAnalyzer.MutualInformation(const Attr1, Attr2: integer): extended;
var
  i, j: integer;
  JointProb: extended;
  MargA1, MargA2: extended;
begin
  Result := 0;
  for i := 0 to pred(DataSet.AttrCardinality(Attr1)) do
    for j := 0 to pred(DataSet.AttrCardinality(Attr2)) do
      begin
        JointProb := fJointProbMatrix[Attr1, i, Attr2, j];
        MargA1 := fPriorProbMatrix[Attr1, i];
        MargA2 := fPriorProbMatrix[Attr2, j];
        if (JointProb > 0) and (MargA1 > 0) and (MargA2 > 0)
          then Result := Result + JointProb * log2(JointProb /(MargA1 * MargA2));
      end;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

function TInformTheoryAnalyzer.Noisiness: extended;
begin
  if not fAMICalc
    then AverageMutualInformation;
  if AMI = 0
    then Result := 0
    else Result := (AttrMeanEntropy - AMI) / AMI;
end;

function TInformTheoryAnalyzer.PriorProbability(const AttrIndex, ClassVal: integer): extended;
var
  i, Count: integer;
begin
  Count := 0;
  for i := 0 to pred(DataSet.InstanceNumber) do
    if DataSet.DataClass(AttrIndex, i) = DataSet.ClassValue(AttrIndex, ClassVal) then inc(Count);
  Result := Count / DataSet.InstanceNumber;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

procedure TInformTheoryAnalyzer.SetDataSet(const Value: TDataSet);
begin
  // Verificar también que DataSet tiene atributos e instancias > 0
  if Assigned(DataSet) and (DataSet <> Value)
    then
      begin
      end;
  FDataSet := Value;
end;

procedure TFormAnalyzer.Execute(DataSet: TDataSet);
begin
  AnalysisThread := TAnalysisThread.Create(Self);
  AnalysisThread.ExecuteAnalysis(DataSet, atAll);
end;

function TInformTheoryAnalyzer.ClassEntropy: extended;
begin
  //Result := Entropy(pred(DataSet.AttributesCount))
  Result := fEntropyMatrix[pred(DataSet.AttributesCount)];
  // MaxProg + 1
end;

constructor TInformTheoryAnalyzer.Create(aDataSet: TDataSet; A: TAnalysisThread);
begin
  fDataSet := aDataSet;
  fRunningThread := A;
  InitPriorProbMatrix;
  InitJointProbMatrix;
  InitEntropyMatrix;
  InitJointEntropyMatrix;
  InitMutualInfoMatrix;
end;

procedure TInformTheoryAnalyzer.MakePriorProbMatrix;
var
  i, j: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    begin
      for j := 0 to pred(DataSet.AttrCardinality(i)) do
        fPriorProbMatrix[i, j] := PriorProbability(i, j);
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + DataSet.AttributesCount + DataSet.AttrCardinality(i)
end;

destructor TInformTheoryAnalyzer.Destroy;
begin
  FinalizePriorProbMatrix;
  FinalizeJointProbMatrix;
  FinalizeEntropyMatrix;
  FinalizeJointEntropyMatrix;
  FinalizeMutualInfoMatrix;
  inherited;
end;

procedure TInformTheoryAnalyzer.MakeJointProbMatrix;
var
  i, j, k, l: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    for j := 0 to pred(DataSet.AttrCardinality(i)) do
      for k := i to pred(DataSet.AttributesCount) do
        begin
          for l := 0 to pred(DataSet.AttrCardinality(k)) do
            begin
              fJointProbMatrix[i, j, k, l] := JointProbability(i, j, k, l);
              if k > i
                then fJointProbMatrix[k, l, i, j] := fJointProbMatrix[i, j, k, l];
            end;
          if fRunningThread.Stop then exit;
        end;
  // MaxProg + DataSet.AttributesCount * DataSet.AttrCardinality(i) *
  // * DataSet.AttributesCount * DataSet.AttrCardinality(i) / 2
end;

procedure TInformTheoryAnalyzer.InitPriorProbMatrix;
var
  i: integer;
begin
  SetLength(fPriorProbMatrix, DataSet.AttributesCount);
  for i := 0 to High(fPriorProbMatrix) do
    SetLength(fPriorProbMatrix[i], DataSet.AttrCardinality(i));
end;

procedure TInformTheoryAnalyzer.InitJointProbMatrix;
var
  i, j, k: integer;
begin
  SetLength(fJointProbMatrix, DataSet.AttributesCount);
  for i := 0 to High(fJointProbMatrix) do
    begin
      SetLength(fJointProbMatrix[i], DataSet.AttrCardinality(i));
      for j := 0 to High(fJointProbMatrix[i]) do
        begin
          SetLength(fJointProbMatrix[i, j], DataSet.AttributesCount);
          for k := 0 to High(fJointProbMatrix[i, j]) do
            SetLength(fJointProbMatrix[i, j, k], DataSet.AttrCardinality(k));
        end;
    end;
end;

procedure TInformTheoryAnalyzer.InitEntropyMatrix;
begin
  SetLength(fEntropyMatrix, DataSet.AttributesCount);
end;

procedure TInformTheoryAnalyzer.MakeEntropyMatrix;
var
  i: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    begin
      fEntropyMatrix[i] := Entropy(i);
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + DataSet.AttributesCount
end;

procedure TInformTheoryAnalyzer.InitJointEntropyMatrix;
var
  i: integer;
begin
  SetLength(fJointEntropyMatrix, DataSet.AttributesCount);
  for i := 0 to High(fJointEntropyMatrix) do
    SetLength(fJointEntropyMatrix[i], DataSet.AttributesCount);
end;

procedure TInformTheoryAnalyzer.MakeJointEntropyMatrix;
var
  i, j: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    for j := i to pred(DataSet.AttributesCount) do
      if i = j
        then fJointEntropyMatrix[i, j] := fEntropymatrix[i]
        else
          begin
            fJointEntropyMatrix[i, j] := JointEntropy(i, j);
            fJointEntropyMatrix[j, i] := fJointEntropyMatrix[i, j];
            if fRunningThread.Stop then exit;
          end;
  // MaxProg + DataSet.AttributesCount * DataSet.AttributesCount
end;

procedure TInformTheoryAnalyzer.InitMutualInfoMatrix;
var
  i: integer;
begin
  SetLength(fMutualInfoMatrix, DataSet.AttributesCount);
  for i := 0 to High(fMutualInfoMatrix) do
    SetLength(fMutualInfoMatrix[i], DataSet.AttributesCount);
end;

procedure TInformTheoryAnalyzer.MakeMutualInfoMatrix;
var
  i, j: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    for j := i to pred(DataSet.AttributesCount) do
      if i = j
        then fMutualInfoMatrix[i, j] := fEntropymatrix[i]      // MutualInformation(i, i);
        else
          begin
            fMutualInfoMatrix[i, j] := MutualInformation(i, j);
            fMutualInfoMatrix[j, i] := fMutualInfoMatrix[i, j];
            if fRunningThread.Stop then exit;
          end;
  // MaxProg + DataSet.AttributesCount * DataSet.AttributesCount
end;

procedure TInformTheoryAnalyzer.Analyze;
begin
  with fRunningThread do
    begin
      if not Stop then MakePriorProbMatrix;
      if not Stop then MakeJointProbMatrix;
      if not Stop then MakeEntropyMatrix;
      if not Stop then MakeJointEntropyMatrix;
      if not Stop then MakeMutualInfoMatrix;
    end;
  // MaxProg + DataSet.AttributesCount * DataSet.AttrCardinality(i)
  // MaxProg + DataSet.AttributesCount * DataSet.AttrCardinality(i) *
  // DataSet.AttributesCount * DataSet.AttrCardinality(i) / 2
  // MaxProg + DataSet.AttributesCount
  // MaxProg + DataSet.AttributesCount * (DataSet.AttributesCount - 1) / 2
  // MaxProg + DataSet.AttributesCount * (DataSet.AttributesCount - 1) / 2
end;

procedure TInformTheoryAnalyzer.FinalizeEntropyMatrix;
begin
  Finalize(fEntropyMatrix);
end;

procedure TInformTheoryAnalyzer.FinalizeJointEntropyMatrix;
var
  i: integer;
begin
  for i := 0 to High(fJointEntropyMatrix) do
    Finalize(fJointEntropyMatrix[i]);
  Finalize(fJointEntropyMatrix);
end;

procedure TInformTheoryAnalyzer.FinalizeJointProbMatrix;
var
  i, j, k: integer;
begin
  for i := pred(DataSet.AttributesCount) downto 0 do
    begin
      for j := pred(DataSet.AttrCardinality(i)) downto 0 do
        begin
          for k := pred(DataSet.AttributesCount) downto 0 do
            Finalize(fJointProbMatrix[i, j, k]);
          Finalize(fJointProbMatrix[i, j]);
        end;
      Finalize(fJointProbMatrix[i]);
    end;
  Finalize(fJointProbMatrix);
end;

procedure TInformTheoryAnalyzer.FinalizeMutualInfoMatrix;
var
  i: integer;
begin
  for i := 0 to High(fMutualInfoMatrix) do
    Finalize(fMutualInfoMatrix[i]);
  Finalize(fMutualInfoMatrix);
end;

procedure TInformTheoryAnalyzer.FinalizePriorProbMatrix;
var
  i: integer;
begin
  for i := 0 to High(fPriorProbMatrix) do
    Finalize(fPriorProbMatrix[i]);
  Finalize(fPriorProbMatrix);
end;

function TInformTheoryAnalyzer.GetMaxProg: integer;
var
  i, j, k: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.AttributesCount) do
    Result := Result + DataSet.AttrCardinality(i);
  for i := 0 to pred(DataSet.AttributesCount) do
    for j := 0 to pred(DataSet.AttrCardinality(i)) do
      for k := i to pred(DataSet.AttributesCount) do
        Result := Result + DataSet.AttrCardinality(k);
  Result := Result + DataSet.AttributesCount * DataSet.AttributesCount;
end;

{ TIntSet }

procedure TIntSet.Add(const Item: integer);
begin
  fList.Add(TInteger.Create(Item));
end;

procedure TIntSet.Append(aSet: TIntSet);
var
  i: integer;
  Item: integer;
begin
  for i := 0 to pred(aSet.Cardinality) do
    begin
      Item := aSet.Items[i];
      if not Contain(Item) then Add(Item);
    end;
end;

function TIntSet.Clone: TIntSet;
var
  i: integer;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(fList.Count) do
    Result.Add(Items[i]);
end;

function TIntSet.Contain(const Item: integer): boolean;
var
  i: integer;
begin
  if Cardinality > 0
    then
      begin
        i := 0;
        while (i < pred(Cardinality)) and (Items[i] <> Item) do inc(i);
        Result := Items[i] = Item;
      end
    else Result := false;
end;

constructor TIntSet.Create;
begin
  fList := TList.Create;
end;

destructor TIntSet.Destroy;
var
  i: integer;
begin
  for i := 0 to pred(FList.Count) do
    TInteger(fList.Items[i]).Free;
  fList.Free;
  inherited;
end;

function TIntSet.Difference(aSet: TIntSet): TIntSet;
var
  i: integer;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(Cardinality) do
    if not aSet.Contain(Items[i]) then Result.Add(Items[i]);
end;

function TIntSet.Empty: boolean;
begin
  Result := fList.Count = 0;
end;

function TIntSet.GetItems(const Index: integer): integer;
begin
  Result := TInteger(fList.Items[Index]).Value;
end;

function TIntSet.GetCardinality: integer;
begin
  Result := fList.Count;
end;

function TIntSet.Intersection(aSet: TIntSet): TIntSet;
var
  i: integer;
  Item: integer;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(aSet.Cardinality) do
    begin
      Item := aSet.Items[i];
      if Contain(Item) then Result.Add(Item);
    end;
end;

function TIntSet.Print: string;
var
  i: integer;
begin
  Result := '{';
  if Cardinality > 1 then
    for i := 0 to Cardinality - 2 do
      Result := Result + IntToStr(Items[i]) + ', ';
  if Cardinality > 0 then Result := Result + IntToStr(Items[pred(Cardinality)]);
  Result := Result + '}';
end;

function TIntSet.SubSet(aSet: TIntSet): boolean;
var
  i: integer;
begin
  if Cardinality = 0
    then Result := true
    else
      begin
        i := 0;
        repeat
          Result := aSet.Contain(Items[i]);
          inc(i);
        until (i = Cardinality) or not Result;
      end;
end;

function TIntSet.Union(aSet: TIntSet): TIntSet;
var
  i: integer;
  Item: integer;
begin
  Result := Clone;
  for i := 0 to pred(aSet.Cardinality) do
    begin
      Item := aSet.Items[i];
      if not Contain(Item) then Result.Add(Item);
    end;
end;

{ TInteger }

constructor TInteger.Create(const Value: integer);
begin
  fValue := Value;
end;

procedure TInteger.SetValue(const Value: integer);
begin
  FValue := Value;
end;

{ TIntSetSet }

procedure TIntSetSet.Add(const Item: TIntSet);
begin
  FList.Add(Item)
end;

function TIntSetSet.Clone: TIntSetSet;
var
  i: integer;
begin
  Result := TIntSetSet.Create;
  for i := 0 to pred(Cardinality) do
    Result.Add(Items[i].Clone);
end;

constructor TIntSetSet.Create;
begin
  fList := TList.Create;
end;

function TIntSetSet.CrossProduct(aSet: TIntSetSet): TIntSetSet;
var
  i, j: integer;
  Intersect: TIntSet;
begin
  Result := TIntSetSet.Create;
  for i := 0 to pred(Cardinality) do
    for j := 0 to pred(aSet.Cardinality) do
      begin
        Intersect := Items[i].Intersection(aSet.Items[j]);
        if not Intersect.Empty
          then Result.Add(Intersect);
      end;
end;

procedure TIntSetSet.Delete(const Ind: integer);
var
  Item: TIntSet;
begin
  if Ind < Cardinality then      //poner breakpoint
    begin
      Item := fList.Items[Ind];
      fList.Delete(Ind);
      Item.Free;
    end;
end;

destructor TIntSetSet.Destroy;
var
  i: integer;
begin
  for i := 0 to pred(FList.Count) do
    TIntSet(fList.Items[i]).Free;
  fList.Free;
  inherited;
end;

function TIntSetSet.Empty: boolean;
begin
  Result := fList.Count = 0;
end;

function TIntSetSet.GetItems(const Index: integer): TIntSet;
begin
  Result := fList.Items[Index];
end;

function TIntSetSet.GetCardinality: integer;
begin
  Result := fList.Count;
end;

procedure TIntSetSet.Pack;
var
  i: integer;
begin
  if Cardinality > 0 then
    begin
      i := 0;
      while i < Cardinality do
        if TIntSet(Items[i]).Empty
          then Delete(i)
          else inc(i);
    end;
end;

function TIntSetSet.Print: string;
var
  i: integer;
begin
  Result := '{';
  if Cardinality > 1 then
    for i := 0 to Cardinality - 2 do
      Result := Result + Items[i].Print + ', ';
  if Cardinality > 0 then Result := Result + Items[pred(Cardinality)].Print;
  Result := Result + '}';
end;

function TIntSetSet.SetOfItem(const x: integer): TIntSet;
var
  i: integer;
  Found: boolean;
begin
  if Cardinality > 0
    then
      begin
        i := 0;
        Found := false;
        while (i < pred(Cardinality)) and not Found do
          begin
            Found := Items[i].Contain(x);
            inc(i);
          end;
        if Found
          then Result := Items[i]
          else Result := nil;
      end
    else Result := nil;
end;

{ TRoughSetAnalyzer }

function TRoughSetAnalyzer.AllAttrSet: TIntSet;
var
  i: integer;
begin
  if DataSet.AttributesCount < 2
    then raise Exception.Create('Análisis Imposible: faltan atributos')
    else
      begin
        Result := TIntSet.Create;
        for i := 0 to DataSet.AttributesCount - 2 do
          Result.Add(i);
      end;
end;

procedure TRoughSetAnalyzer.Analyze;
begin
  ClearVars;
  ComputApproximations;
  if fRunningThread.Stop then exit;
  ComputWeights;
  if fRunningThread.Stop then exit;
  ComputMeasures;
  // MaxProg + DataSet.ClassCardinality * (2 * fDiscernability.Cardinality + 1)
  // MaxProg + 2 * DataSet.ClassCardinality * fDiscernability.Cardinality
  // MaxProg + 1
end;

function TRoughSetAnalyzer.ApproxAccuracy(const ClassIndex: integer): extended;
begin
  if fUpperApprox[ClassIndex].Cardinality = 0
    then Result := NAN
    else Result := fLowerApprox[ClassIndex].Cardinality / fUpperApprox[ClassIndex].Cardinality;
end;

function TRoughSetAnalyzer.ApproxQuality(const ClassIndex: integer): extended;
begin
  if ItemsOfClass[ClassIndex].Cardinality = 0
    then Result := NAN
    else Result := fPositive[ClassIndex].Cardinality / ItemsOfClass[ClassIndex].Cardinality;
end;

procedure TRoughSetAnalyzer.BuildEquivalenceRelationList;
var
  i: integer;
begin
  for i := 0 to pred(DataSet.AttributesCount) do
    begin
      FEqRel.Add(EquivalenceRelation(i));
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + DataSet.AttributesCount x DataSet.AttrCardinality
end;

function TRoughSetAnalyzer.ClassifAccuracy: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    if not IsNAN(fApproxAccuracy[i])
      then Result := Result + fApproxAccuracy[i];
  Result := Result / DataSet.ClassCardinality;
end;

function TRoughSetAnalyzer.ClassifQuality: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    Result := Result + fLowerApprox[i].Cardinality;
  Result := Result / Universe.Cardinality;
end;

function TRoughSetAnalyzer.ClassSet: TIntSet;
begin
  if DataSet.AttributesCount < 2
    then raise Exception.Create('Análisis Imposible: faltan atributos')
    else
      begin
        Result := TIntSet.Create;
        Result.Add(pred(DataSet.AttributesCount));
      end;
end;

procedure TRoughSetAnalyzer.ClearRelations;
var
  i: integer;
begin
  if Assigned(FEqRel) then
    begin
      for i := 0 to pred(FEqRel.Count) do
        TIntSetSet(FEqRel.Items[i]).Free;
      FEqRel.Free;
    end;
end;

procedure TRoughSetAnalyzer.ClearVars;
begin
  //if Assigned(fLowerApprox) then fLowerApprox.Free;
  //if Assigned(fUpperApprox) then fUpperApprox.Free;
  //if Assigned(fPositive) then fPositive.Free;
  //if Assigned(fNegative) then fNegative.Free;
  //if Assigned(fBoundary) then fBoundary.Free;
end;

procedure TRoughSetAnalyzer.ComputApproximations;
var
  i: integer;
begin
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      fUpperApprox.Add(UpperApprox(fAttrSet, ItemsOfClass[i]));
      fLowerApprox.Add(LowerApprox(fAttrSet, ItemsOfClass[i]));
      fPositive.Add(fLowerApprox[i].Clone);
      fNegative.Add(Universe.Difference(fUpperApprox[i]));
      fBoundary.Add(fUpperApprox[i].Difference(fLowerApprox[i]));
      if Assigned(fOnProgress) then fOnProgress(Self);
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + DataSet.ClassCardinality * (2 * fDiscernability.Cardinality + 1)
end;

procedure TRoughSetAnalyzer.ComputMeasures;
var
  i: integer;
begin
  Setlength(fApproxAccuracy, DataSet.ClassCardinality);
  Setlength(fApproxQuality, DataSet.ClassCardinality);
  Setlength(fDependency, DataSet.ClassCardinality);
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      fApproxAccuracy[i] := ApproxAccuracy(i);
      fApproxQuality[i] := ApproxQuality(i);
      fDependency[i] := Dependency(i);
      if fRunningThread.Stop then exit;
    end;
  if Assigned(fOnProgress) then fOnProgress(Self);
  // MaxProg + 1
end;

procedure TRoughSetAnalyzer.ComputWeights;
var
  i: integer;
begin
  fTotalWeight1 := 0;
  fTotalWeight2 := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      fWeights1[i] := MeanClassRoughMembership(ItemsOfClass[i]);
      fWeights2[i] := MeanClassRoughInvolvement(ItemsOfClass[i]);
      fTotalWeight1 := fTotalWeight1 + fWeights1[i];
      fTotalWeight2 := fTotalWeight2 + fWeights2[i];
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + 2 * DataSet.ClassCardinality * fDiscernability.Cardinality
end;

constructor TRoughSetAnalyzer.Create(aDataSet: TDataSet; A: TAnalysisThread);
begin
  fDataSet := aDataSet;
  fRunningThread := A;
  FEqRel := TList.Create;
  fPositive := TIntSetSet.Create;
  fNegative := TIntSetSet.Create;
  fBoundary := TIntSetSet.Create;
  fUniverse := Universe;
  fClassSet := ClassSet;
  fAttrSet := AllAttrSet;
end;

function TRoughSetAnalyzer.Dependency(const ClassIndex: integer): extended;
begin
  Result := fPositive[ClassIndex].Cardinality / Universe.Cardinality;
end;

destructor TRoughSetAnalyzer.Destroy;
begin
  ClearRelations;
  ClearVars;
  fUniverse.Free;
  fClassSet.Free;
  fAttrSet.Free;
  fDiscernability.Free;
  fUpperApprox.Free;
  fLowerApprox.Free;
  Finalize(fWeights1);
  Finalize(fWeights2);
  fPositive.Free;
  fNegative.Free;
  fBoundary.Free;
  inherited;
end;

function TRoughSetAnalyzer.Discernability(AttrList: TIntSet): TIntSetSet;
var
  temp: TIntSetSet;
  i: integer;
begin
  if AttrList.Cardinality > 1
    then
      begin
        i := 1;
        Result := EqRelation[AttrList.Items[0]];
        repeat
          temp := Result;
          Result := temp.CrossProduct(EqRelation[AttrList.Items[i]]);
          if i > 1 then temp.Free;
          inc(i);
          if Assigned(fOnProgress) then fOnProgress(Self);
          if fRunningThread.Stop then exit;
        until i = AttrList.Cardinality;
      end
    else
      if AttrList.Cardinality = 1
        then
          begin
            Result := EqRelation[AttrList.Items[0]].Clone;
            Result.Pack;
            if Assigned(fOnProgress) then fOnProgress(Self);
            if fRunningThread.Stop then exit;
          end
        else Result := nil;
  // MaxProg + AttrList.Cardinality
end;

function TRoughSetAnalyzer.EquivalenceRelation(
  const AttrIndex: integer): TIntSetSet;
var
  i: integer;
begin
  Result := TIntSetSet.Create;
  for i := 0 to pred(DataSet.AttrCardinality(AttrIndex)) do
    begin
      Result.Add(InstancesXAttrVal(AttrIndex, DataSet.ClassValue(AttrIndex, i)));
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + DataSet.AttrCardinality(AttrIndex)
end;

function TRoughSetAnalyzer.GenApproxRatio: extended;
var
  i: integer;
  Sup, Inf: extended;
begin
  {
  Inf := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    Inf := Inf + fLowerApprox[i].Cardinality;
  Sup := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    Sup := Sup + fUpperApprox[i].Cardinality;
  Result := Inf / Sup;
  }
  Inf := 0;
  Sup := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      Inf := Inf + fLowerApprox[i].Cardinality;
      Sup := Sup + fUpperApprox[i].Cardinality;
    end;
  Result := Inf / Sup;
end;

function TRoughSetAnalyzer.GenClassifAccuracy: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    if not IsNAN(fApproxAccuracy[i])
      then Result := Result + fApproxAccuracy[i] * fWeights1[i];
  Result := Result / fTotalWeight1;
end;

function TRoughSetAnalyzer.GenClassifQuality: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    if not IsNAN(fApproxQuality[i])
      then Result := Result + fApproxQuality[i] * fWeights2[i];
  Result := Result / fTotalWeight2;
end;

function TRoughSetAnalyzer.GetEqRelation(const i: integer): TIntSetSet;
begin
  Result := FEqRel.Items[i];
end;

function TRoughSetAnalyzer.GetItemsOfClass(const i: integer): TIntSet;
begin
  Result := EqRelation[pred(DataSet.AttributesCount)].Items[i];
end;

function TRoughSetAnalyzer.GetMaxProg: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(fDataSet.AttributesCount) do
    Result := Result + fDataSet.AttrCardinality(i);
  Result := Result + fAttrSet.Cardinality;
  Result := Result + 1;
  Result := Result + DataSet.ClassCardinality * (2 * fDiscernability.Cardinality + 1);
  Result := Result + 2 * DataSet.ClassCardinality * fDiscernability.Cardinality;
  Result := Result + 1;
end;

procedure TRoughSetAnalyzer.Init;
begin
  BuildEquivalenceRelationList;
  if fRunningThread.Stop then exit;
  fDiscernability := Discernability(fAttrSet);
  if fRunningThread.Stop then exit;
  fUpperApprox := TIntSetset.Create;
  fLowerApprox := TIntSetset.Create;
  SetLength(fWeights1, DataSet.ClassCardinality);
  SetLength(fWeights2, DataSet.ClassCardinality);
  if Assigned(fOnProgress) then fOnProgress(Self);
  // MaxProg + fDataSet.AttributesCount x fDataSet.AttrCardinality
  // MaxProg + fAttrSet.Cardinality
  // MaxProg + 1
end;

function TRoughSetAnalyzer.InstancesXAttrVal(AttrIndex: integer;
  const ClassVal: string): TIntSet;
var
  i: integer;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(DataSet.InstanceNumber) do
    if DataSet.DataClass(AttrIndex, i) = ClassVal
      then Result.Add(i);
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

function TRoughSetAnalyzer.LowerApprox(Attrs, Instances: TIntSet): TIntSet;
var
  i: integer;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(fDiscernability.Cardinality) do
    begin
      if fDiscernability.Items[i].SubSet(Instances)
        then Result.Append(fDiscernability.Items[i]);
      if Assigned(fOnProgress) then fOnProgress(Self);
      if fRunningThread.Stop then exit;
    end;
end;

function TRoughSetAnalyzer.MeanClassRoughInvolvement(
  AClassSet: TIntSet): extended;
var
  j: integer;
  R, I: TIntSet;
begin
  Result := 0;
  if fDiscernability.Cardinality = 0
    then raise EMetric.Create('Imposible aplicar la métrica "Media del compromiso aproximado por clases"')
    else
      begin
        for j := 0 to pred(fDiscernability.Cardinality) do
          begin
            R := fDiscernability[j];
            I := R.Intersection(AClassSet);
            if AClassSet.Cardinality > 0
              then Result := Result + I.Cardinality / AClassSet.Cardinality;
            I.Free;
            if Assigned(fOnProgress) then fOnProgress(Self);
            if fRunningThread.Stop then exit;
          end;
        Result := Result / fDiscernability.Cardinality;
      end;
  // MaxProg + fDiscernability.Cardinality
end;

function TRoughSetAnalyzer.MeanClassRoughMembership(AClassSet: TIntSet): extended;
var
  j: integer;
  R, I: TIntSet;
begin
  Result := 0;
  if fDiscernability.Cardinality = 0
    then raise EMetric.Create('Imposible aplicar la métrica "Media de la pertenencia aproximada por clases"')
    else
      begin
        for j := 0 to pred(fDiscernability.Cardinality) do
          begin
            R := fDiscernability[j];
            I := R.Intersection(AClassSet);
            Result := Result + I.Cardinality / R.Cardinality;
            I.Free;
            if Assigned(fOnProgress) then fOnProgress(Self);
            if fRunningThread.Stop then exit;
          end;
        Result := Result / fDiscernability.Cardinality;
      end;
  // MaxProg + fDiscernability.Cardinality
end;

function TRoughSetAnalyzer.PrintAttributeSet: string;
begin
  Result := fAttrSet.Print;
end;

function TRoughSetAnalyzer.PrintBoundary: string;
begin
  Result := fBoundary.Print;
end;

function TRoughSetAnalyzer.PrintClassSet: string;
begin
  Result := fClassSet.Print;
end;

function TRoughSetAnalyzer.PrintDiscernability: string;
begin
  Result := fDiscernability.Print;
end;

function TRoughSetAnalyzer.PrintLowerApprox: string;
var
  i: integer;
begin
  Result := 'Inf = < ';
  if DataSet.ClassCardinality > 1 then
    for i := 0 to DataSet.ClassCardinality - 2 do
      Result := Result + fLowerApprox[i].Print + ', ';
  if DataSet.ClassCardinality > 0 then
    Result := Result + fLowerApprox[pred(DataSet.ClassCardinality)].Print;
  Result := Result + ' >';
end;

function TRoughSetAnalyzer.PrintNegative: string;
begin
  Result := fNegative.Print;
end;

function TRoughSetAnalyzer.PrintPositive: string;
begin
  Result := fPositive.Print;
end;

function TRoughSetAnalyzer.PrintUniverse: string;
begin
  Result := fUniverse.Print;
end;

function TRoughSetAnalyzer.PrintUpperApprox: string;
var
  i: integer;
begin
  Result := 'Sup = <';
  if DataSet.ClassCardinality > 1 then
    for i := 0 to DataSet.ClassCardinality - 2 do
      Result := Result + fUpperApprox[i].Print + ', ';
  if DataSet.ClassCardinality > 0 then
    Result := Result + fUpperApprox[pred(DataSet.ClassCardinality)].Print;
  Result := Result + ' >';
end;

function TRoughSetAnalyzer.PrintVector(Vector: TVector): string;
var
  i: integer;
begin
  Result := '< ';
  if Length(Vector) > 1 then
    for i := 0 to pred(High(Vector)) do
      Result := Result + FloatToStrF(Vector[i], ffGeneral, 2, 2) + ',   ';
  if Length(Vector) > 0 then
    Result := Result + FloatToStrF(Vector[High(Vector)], ffGeneral, 2, 2);
  Result := Result + ' >';
end;

function TRoughSetAnalyzer.RoughInvolvement(const instance: integer;
  AClassSet: TIntSet): extended;
var
  R, I: TIntSet;
begin
  R := fDiscernability.SetOfItem(instance);
  I := R.Intersection(AClassSet);
  Result := I.Cardinality / AClassSet.Cardinality;
  I.Free;
end;

function TRoughSetAnalyzer.RoughMembership(const instance: integer;
  AClassSet: TIntSet): extended;
var
  R, I: TIntSet;
begin
  R := fDiscernability.SetOfItem(instance);
  I := R.Intersection(AClassSet);
  Result := I.Cardinality / R.Cardinality;
  I.Free;
end;

procedure TRoughSetAnalyzer.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TFormAnalyzer.InformationTheoryAnalysis(DataSet: TDataSet);
begin
  AnalysisThread := TAnalysisThread.Create(Self);
  AnalysisThread.ExecuteAnalysis(DataSet, atInformationTheory);
end;

procedure TFormAnalyzer.PrintMatrix(DataSet: TDataSet; const aTitle: string; aMatrix: T2DMatrixSingle);
var
  i, j: integer;
  S: string;
begin
  Memo.Lines.Add('');
  Memo.Lines.Add(Format('*** %s ***', [aTitle]));
  Memo.Lines.Add('');
  S := chr(9);
  for i := 0 to pred(DataSet.AttributesCount) do
    S := S + DataSet.Attribute[i].Name + chr(9);
  Memo.Lines.Add(S);
  for i := 0 to pred(DataSet.AttributesCount) do
    begin
      S := DataSet.Attribute[i].Name + chr(9);
      for j := 0 to pred(DataSet.AttributesCount) do
        S := S + Format('%f', [aMatrix[i, j]]) + chr(9);
      Memo.Lines.Add(S);
    end;
end;

procedure TFormAnalyzer.RoughSetAnalysis(DataSet: TDataSet);
begin
  AnalysisThread := TAnalysisThread.Create(Self);
  AnalysisThread.ExecuteAnalysis(DataSet, atRoughSet);
end;

procedure TRoughSetAnalyzer.SetTargetClass(const Value: integer);
begin
  if (Value < 0) or (Value >= DataSet.ClassCardinality)
    then raise Exception.Create(Format('Inválido índice de clase "%d"', [Value]))
    else FTargetClass := Value;
end;

function TRoughSetAnalyzer.Universe: TIntSet;
var
  i: integer;
begin
  if DataSet.InstanceNumber = 0
    then raise Exception.Create('Análisis imposible: base de datos sin instancias')
    else
      begin
        Result := TIntSet.Create;
        for i := 0 to pred(DataSet.InstanceNumber) do
          Result.Add(i);
      end;
end;

function TRoughSetAnalyzer.UpperApprox(Attrs, Instances: TIntSet): TIntSet;
var
  i: integer;
  Inter: TIntSet;
begin
  Result := TIntSet.Create;
  for i := 0 to pred(fDiscernability.Cardinality) do
    begin
      Inter := fDiscernability.Items[i].Intersection(Instances);
      if not Inter.Empty
        then Result.Append(fDiscernability.Items[i]);
      Inter.Free;
      if Assigned(fOnProgress) then fOnProgress(Self);
      if fRunningThread.Stop then exit;
    end;
  // MaxProg + fDiscernability.Cardinality
end;

procedure TFormAnalyzer.FormHide(Sender: TObject);
begin
  Main.DataSetDesigner.ViewAnalysisSheet.Checked := false;
end;

procedure TFormAnalyzer.FormDeactivate(Sender: TObject);
begin
  Main.DataSetDesigner.ViewAnalysisSheet.Checked := false;
end;

function TRoughSetAnalyzer.WeightedClassifAccuracy: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    if not IsNAN(fApproxAccuracy[i])
      then Result := Result + fApproxAccuracy[i] * ItemsOfClass[i].Cardinality;
  Result := Result / Universe.Cardinality;
end;

procedure TFormAnalyzer.BasicMeasures(DataSet: TDataSet);
begin
  AnalysisThread := TAnalysisThread.Create(Self);
  AnalysisThread.ExecuteAnalysis(DataSet, atBasic);
end;

{ TStatisticalAnalizer }

function TStatisticalAnalizer.AttrValMatrix(const Ci: integer): TMatrix;
var
  i, j: integer;
  ClassID: integer;
begin
  ClassID := pred(DataSet.AttributesCount);
  Result := TMatrix.Create;
  Result.ColCount := pred(DataSet.AttributesCount);
  for i := 0 to pred(DataSet.InstanceNumber) do
    if DataSet.DataClass(ClassID, i) = DataSet.ClassValue(ClassID, Ci)
      then
        begin
          Result.RowCount := Result.RowCount + 1;
          for j := 0 to DataSet.AttributesCount - 2 do
            Result[pred(Result.RowCount), j] := DataSet.DataStore[j, i];
        end;
end;

function TStatisticalAnalizer.CovarianceMatrix(const Ci: integer): TMatrix;
var
  AttrValT : TMatrix;
  K: extended;
begin
  if N[Ci] < 2
    then raise EMetric.Create('Imposible aplicar la métrica SD_Ratio: Existen clases con menos de dos instancias')
    else K := 1 / (N[Ci] - 1);
  AttrValT := FAttrValMatrices[Ci].Transpose;
  Result := AttrValT.Multiply(FAttrValMatrices[Ci]);
  Result.Subtract(MeanAttrM);
  Result.Multiply(K);
  AttrValT.Free;
end;

constructor TStatisticalAnalizer.Create(aDataSet: TDataSet; A: TAnalysisThread);
begin
  FDataSet := aDataSet;
  fRunningThread := A;
  SetParams;
end;

function TStatisticalAnalizer.GetInverter(const Dim: integer): TMatrix;
var
  i: integer;
begin
  Result := TMatrix.CreateSquare(Dim);
  for i := 0 to pred(Dim) do
    Result[i, i] := 0.001;
end;

function TStatisticalAnalizer.MeanAttr(const Ai: integer): extended;
var
  i: integer;
  S: extended;
begin
  S := 0;
  // chequear que DataSet.InstanceNumber > 0
  for i := 0 to pred(DataSet.InstanceNumber) do
    S := S + DataSet.DataStore[Ai, i];
  Result := S / DataSet.InstanceNumber;
end;

function TStatisticalAnalizer.MeanAttrVector: TMatrix;
var
  i: integer;
begin
  Result := TMatrix.CreateSize(1, pred(DataSet.AttributesCount));
  for i := 0 to DataSet.AttributesCount - 2 do
    Result[0, i] := MeanAttr(i);
end;

function TStatisticalAnalizer.MStatistic: extended;
var
  Gamma: extended;
  T1, S, K: extended;
  P, Q, i: integer;
  M, Y: TMatrix;
begin
  SetCovarMatrices;
  FPooledCovar := PooledCovarMatrix;
  Q := DataSet.ClassCardinality;
  P := DataSet.AttributesCount - 1;
  T1 := 1 / (DataSet.InstanceNumber - Q);
  S := 0;
  for i := 0 to pred(Q) do
    begin
      K := 1 / (N[i] - 1);
      S := S + (K - T1);
    end;
  Gamma := 1-(2*P*P+3*P-1)*S/(6*(P+1)*(Q-1));
  Result := 0;
  for i := 0 to pred(Q) do
    begin
      M := FCovarMatrices[i].Duplicate;
      try
        M.InverseInPlace;             // Problemas con la invertibilidad
        M.MultiplyInPlace(FPooledCovar);
        Result := Result + (N[i] - 1)*log10(abs(M.Determinant));
        M.Free;
      except
        Y := GetInverter(P);
        M.Add(Y);
        Y.Free;
        try
          M.InverseInPlace;             // Problemas con la invertibilidad
          M.MultiplyInPlace(FPooledCovar);
          Result := Result + (N[i] - 1)*log10(abs(M.Determinant));
        finally
          M.Free;
        end;
      end;
    end;
  Result := Gamma * Result;
end;

function TStatisticalAnalizer.NiSum: extended;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(DataSet.ClassCardinality) do
    Result := Result + (N[i] - 1);
end;

function TStatisticalAnalizer.PooledCovarMatrix: TMatrix;
var
  i: integer;
  Z: TMatrix;
begin
  Result := TMatrix.CreateSquare(pred(DataSet.AttributesCount));
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      Z := FCovarmatrices[i].Duplicate;
      Z.Multiply(N[0] - 1);
      Result.Add(Z);
    end;
  Result.Multiply(1 / (DataSet.InstanceNumber - DataSet.ClassCardinality));
end;

function TStatisticalAnalizer.SDRatio: extended;
var
  p: integer;
begin
  p := DataSet.AttributesCount;
  Result := exp(MStatistic / (p * NiSum));
end;

procedure TStatisticalAnalizer.SetAttrValMatrices;
var
  i: integer;
begin
  Setlength(FAttrValMatrices, DataSet.ClassCardinality);
  SetLength(N,DataSet.ClassCardinality);
  for i := 0 to pred(DataSet.ClassCardinality) do
    begin
      FAttrValMatrices[i] := AttrValMatrix(i);
      N[i] := FAttrValMatrices[i].RowCount;
    end;
end;

procedure TStatisticalAnalizer.SetCovarMatrices;
var
  i: integer;
begin
  Setlength(FCovarMatrices, DataSet.ClassCardinality);
  for i := 0 to pred(DataSet.ClassCardinality) do
    FCovarMatrices[i] := CovarianceMatrix(i);
end;

procedure TStatisticalAnalizer.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TFormAnalyzer.StatisticAnalysis(DataSet: TDataSet);
begin
  AnalysisThread := TAnalysisThread.Create(Self);
  AnalysisThread.ExecuteAnalysis(DataSet, atStatistic);
end;

procedure TStatisticalAnalizer.SetParams;
var
  MeanAttr, MeanAttrT: TMatrix;
begin
  MeanAttr := MeanAttrVector;
  MeanAttrT := MeanAttr.Transpose;
  MeanAttrM := MeanAttrT.Multiply(MeanAttr);
  SetAttrValMatrices;
  MeanAttr.Free;
  MeanAttrT.Free;
end;

{ TAnalysisThread }

procedure TAnalysisThread.AllAnalysis(DataSet: TDataSet);
begin
  BasicMeasures(DataSet);
  RoughSetAnalysis(DataSet);
  InformationTheoryAnalysis(DataSet);
  StatisticAnalysis(DataSet);
end;

procedure TAnalysisThread.BasicMeasures(DataSet: TDataSet);
begin
  with F do
    begin
      if MaxProg = 0
        then ProgressBar1.Max := 1;
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('   MÉTRICAS BÁSICAS');
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('');
      Memo.Lines.Add(Format('Número de Instancias: %d', [DataSet.InstanceNumber]));
      Memo.Lines.Add('');
      Memo.Lines.Add(Format('Número de Atributos de Decisión: %d', [pred(DataSet.AttributesCount)]));
      Memo.Lines.Add('');
      Memo.Lines.Add(Format('Número de Clases: %d', [DataSet.ClassCardinality]));
      Memo.Lines.Add('');
      Memo.Lines.Add(Format('Número de Atributos de Decisión Binarios: %d', [DataSet.BinAttrNumber]));
      ProgressBar1.Position := 1;
    end;
end;

constructor TAnalysisThread.Create(Form: TFormAnalyzer);
begin
  inherited Create(true);
  F := Form;
end;

procedure TAnalysisThread.Execute;
begin
  inherited;
  F.ProgressBar1.Position := 0;
  F.StopBtn.Enabled := true;
  case AnalysisType of
    atBasic: BasicMeasures(DataSet);
    atRoughSet: RoughSetAnalysis(DataSet);
    atInformationTheory: InformationTheoryAnalysis(DataSet);
    atStatistic: StatisticAnalysis(DataSet);
    atAll: AllAnalysis(DataSet);
  end;
  if Stop then
    begin
      F.Memo.Lines.Add('');
      F.Memo.Lines.Add('Proceso cancelado por el usuario');
      F.ProgressBar1.Position := 0;
    end;
  F.StopBtn.Enabled := false;
end;

procedure TAnalysisThread.ExecuteAnalysis(D: TDataSet; A: TAnalysisType);
begin
  DataSet := D;
  AnalysisType := A;
  Suspended := false;
end;

procedure TAnalysisThread.InformationTheoryAnalysis(DataSet: TDataSet);
var
  i, j: integer;
  ITA: TInformTheoryAnalyzer;
  S: string;
begin
  with F do
    begin
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('   MÉTRICAS DE TEORÍA DE LA INFORMACIÓN');
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('');
      ITA := TInformTheoryAnalyzer.Create(DataSet, Self);
      try
        ProgressBar1.Max := ITA.GetMaxProg;
        ITA.OnProgress := ProgressBar;
        ITA.Analyze;
        if Stop then exit;
        Memo.Lines.Add('*** Matriz de probabilidades previas ***');
        Memo.Lines.Add('');
        for i := 0 to pred(DataSet.AttributesCount) do
          begin
            S := DataSet.Attribute[i].Name + chr(9);
            for j := 0 to pred(DataSet.AttrCardinality(i)) do
              S := S + DataSet.ClassValue(i, j) + ':  ' + FloatToStrf(ITA.fPriorProbMatrix[i, j], ffGeneral, 10, 2) + chr(9);
            Memo.Lines.Add(S);
          end;
        PrintMatrix(DataSet, 'Matriz de entropia conjunta', ITA.fJointEntropyMatrix);
        PrintMatrix(DataSet, 'Matriz de información mutua', ITA.fMutualInfoMatrix);
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Entropía de Clase ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('H(C) = %.4F', [ITA.ClassEntropy]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Entropía Media ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('H(X) = %.4F', [ITA.AttrMeanEntropy]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Información Mutua Media ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('M(C, X) = %.4F', [ITA.AverageMutualInformation]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Número Equivalente de Atributos ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('EN.Attr = %.4F', [ITA.ENAttr]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Tasa de Ruido ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Tasa de Ruido = %.4F', [ITA.Noisiness]));
      except
        on E: Exception do
          Memo.Lines.Add(E.Message);
      end;
      ITA.Free;
    end;
end;

procedure TAnalysisThread.ProgressBar(Sender: TObject);
begin
  F.ProgressBar1.Position := F.ProgressBar1.Position + 1;
end;

procedure TAnalysisThread.RoughSetAnalysis(DataSet: TDataSet);
var
  RSA: TRoughSetAnalyzer;
  i: integer;
begin
  {
  if MaxProg = 0
    then F.ProgressBar1.Max := 1;
  }
  with F do
    begin
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('   MÉTRICAS DE LA TEORÍA DE CONJUNTOS APROXIMADOS');
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      RSA := TRoughSetAnalyzer.Create(DataSet, Self);
      try
        //ProgressBar1.Max := RSA.GetFirstMaxProg;
        RSA.Init;
        if Stop then exit;
        ProgressBar1.Max := RSA.GetMaxProg;
        RSA.OnProgress := ProgressBar;
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Relaciones de Equivalencias ***');
        Memo.Lines.Add('');
        RSA.ComputApproximations;
        if Stop then exit;
        for i := 0 to pred(DataSet.AttributesCount) do
          Memo.Lines.Add(Format('R(%S) = %S',
            [DataSet.Attribute[i].Name, RSA.EqRelation[i].Print]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Discernibilidad de todos los atributos ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Dis = %S', [RSA.PrintDiscernability]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Vector de Aproximación Inferior ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(RSA.PrintLowerApprox);
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Vector de Aproximación Superior ***');
        Memo.Lines.Add('');
        Memo.Lines.Add(RSA.PrintUpperApprox);
        Memo.Lines.Add('');
        RSA.ComputWeights;
        if Stop then exit;
        RSA.ComputMeasures;
        if Stop then exit;
        Memo.Lines.Add('*** Vector de Precisión de la Aproximación de Clases ( |R-| / |R+| ) ***');
        Memo.Lines.Add(RSA.PrintVector(RSA.fApproxAccuracy));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Precisión media de la clasificación = %f', [RSA.ClassifAccuracy]));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Precisión ponderada de la clasificación = %f', [RSA.WeightedClassifAccuracy]));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Precisión generalizada de la clasificación = %f', [RSA.GenClassifAccuracy]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Media de la pertenencia aproximada por clases (pesos de la precisión generalizada) ***');
        Memo.Lines.Add(RSA.PrintVector(RSA.fWeights1));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Vector de Calidad de la Aproximación de Clases ( |R-| / |X| ) ***');
        Memo.Lines.Add(RSA.PrintVector(RSA.fApproxQuality));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Calidad media de la clasificación = %f', [RSA.ClassifQuality]));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Calidad generalizada de la clasificación = %f', [RSA.GenClassifQuality]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Media del compromiso aproximado por clases (pesos de la calidad generalizada) ***');
        Memo.Lines.Add(RSA.PrintVector(RSA.fWeights2));
        Memo.Lines.Add('');
        Memo.Lines.Add(Format('Coeficiente de Aproximación General = %f', [RSA.GenApproxRatio]));
        Memo.Lines.Add('');
        Memo.Lines.Add('*** Vector de Grado de Dependencia de Clases ( |R-| / |U| ) ***');
        Memo.Lines.Add(RSA.PrintVector(RSA.fDependency));
      except
        on E: Exception do
          Memo.Lines.Add(E.Message);
      end;
    end;
  RSA.Free;
end;

procedure TAnalysisThread.StatisticAnalysis(DataSet: TDataSet);
var
  SA: TStatisticalAnalizer;
begin
  SA := TStatisticalAnalizer.Create(DataSet, Self);
  with F do
    begin
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('   MÉTRICAS DE LA TEORÍA ESTADÍSTICA ');
      Memo.Lines.Add('');
      Memo.Lines.Add('OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO');
      Memo.Lines.Add('');
      Memo.Lines.Add('');
      try
        Memo.Lines.Add(Format('SD_Ratio: %.4f', [SA.SDRatio]));
      except
        on E: Exception do
          Memo.Lines.Add(E.Message);
      end;
      SA.Free;
    end;
end;

procedure TFormAnalyzer.StopBtnClick(Sender: TObject);
begin
  AnalysisThread.Stop := true;
end;

end.
