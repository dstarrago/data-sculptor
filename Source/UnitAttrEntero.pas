unit UnitAttrEntero;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin, ExtCtrls, ActnList;

type
  TFormEntero = class(TForm)
    PanelDesde: TGroupBox;
    PanelHasta: TGroupBox;
    RadioMasInf: TRadioButton;
    EditEnteroValMax: TSpinEdit;
    RadioMenosInf: TRadioButton;
    EditEnteroValMin: TSpinEdit;
    RadioMinVal: TRadioButton;
    RadioMaxVal: TRadioButton;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    ComboDistribution: TComboBox;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    DepenList: TListBox;
    GroupBox2: TGroupBox;
    AddBtn: TSpeedButton;
    RemoveBtn: TSpeedButton;
    VarList: TListBox;
    WeightEdit: TSpinEdit;
    Label1: TLabel;
    SetBtn: TSpeedButton;
    RuidoEdit: TSpinEdit;
    Label2: TLabel;
    procedure EditEnteroValMaxClick(Sender: TObject);
    procedure EditEnteroValMinClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure VarListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure DepenListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetTitle(const VarName: string);
  end;

var
  FormEntero: TFormEntero;

implementation

uses Main;

{$R *.dfm}

const
  FTitle = 'Atributo Entero ';

procedure TFormEntero.EditEnteroValMaxClick(Sender: TObject);
begin
  RadioMaxVal.Checked := true;
end;

procedure TFormEntero.EditEnteroValMinClick(Sender: TObject);
begin
  RadioMinVal.Checked := true;
end;

procedure TFormEntero.BitBtnOkClick(Sender: TObject);
begin
  if RadioMaxVal.Checked and RadioMinVal.Checked and
    (StrToInt(EditEnteroValMin.Text) > StrToInt(EditEnteroValMax.Text))
    then
      begin
        ShowMessage('El valor mínimo no puede superar al valor máximo');
        ModalResult := mrNone;
      end;
end;

procedure TFormEntero.AddBtnClick(Sender: TObject);
begin
  if (WeightEdit.Value >= 0) and (WeightEdit.Value <= 100)
    then
      begin
        DepenList.Items.AddObject(WeightEdit.Text + '    ' + VarList.Items.Strings[VarList.ItemIndex],
          VarList.Items.Objects[VarList.ItemIndex]);
        VarList.Items.Delete(VarList.ItemIndex);
        AddBtn.Enabled := false;
      end
    else ShowMessage('El peso debe estar entre 0 y 100');
end;

procedure TFormEntero.RemoveBtnClick(Sender: TObject);
begin
  VarList.Items.AddObject(TAttribute(DepenList.Items.Objects[DepenList.ItemIndex]).Name,
    DepenList.Items.Objects[DepenList.ItemIndex]);
  DepenList.Items.Delete(DepenList.ItemIndex);
  RemoveBtn.Enabled := false;
  SetBtn.Enabled := false;
end;

procedure TFormEntero.VarListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if VarList.ItemIndex >= 0
    then AddBtn.Enabled := true;
end;

procedure TFormEntero.FormShow(Sender: TObject);
begin
  AddBtn.Enabled := false;
  RemoveBtn.Enabled := false;
  SetBtn.Enabled := false;
end;

procedure TFormEntero.DepenListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DepenList.ItemIndex >= 0
    then
      begin
        RemoveBtn.Enabled := true;
        SetBtn.Enabled := true;
      end;
end;

procedure TFormEntero.SetBtnClick(Sender: TObject);
var
  S: string;
begin
  if (WeightEdit.Value >= 0) and (WeightEdit.Value <= 100)
    then
      begin
        S := DepenList.Items.Strings[DepenList.ItemIndex];
        Delete(S, 1, pred(Pos(' ', S)));
        S := WeightEdit.Text + S;
        DepenList.Items.Strings[DepenList.ItemIndex] := S;
      end
    else ShowMessage('El peso debe estar entre 0 y 100');
end;

procedure TFormEntero.SetTitle(const VarName: string);
begin
  Caption := FTitle + VarName;
end;

end.
