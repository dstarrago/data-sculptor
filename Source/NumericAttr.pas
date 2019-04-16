unit NumericAttr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, Spin;

type
  TNumericForm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    GroupBox2: TGroupBox;
    VarList: TListBox;
    GroupBox1: TGroupBox;
    DepenList: TListBox;
    AddBtn: TSpeedButton;
    RemoveBtn: TSpeedButton;
    Label1: TLabel;
    WeightEdit: TSpinEdit;
    RuidoEdit: TSpinEdit;
    Label2: TLabel;
    NormalDistButton: TRadioButton;
    Panel1: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Mean: TEdit;
    StdDev: TEdit;
    UniformDistButton: TRadioButton;
    Panel2: TPanel;
    Label5: TLabel;
    attrName: TEdit;
    MinVal: TEdit;
    MaxVal: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure BitBtnOkClick(Sender: TObject);
    procedure SetBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure VarListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DepenListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetTitle(const VarName: string);
  end;

var
  NumericForm: TNumericForm;

implementation

uses Main;

const
  FTitle = 'Atributo Real ';

{$R *.dfm}

procedure TNumericForm.BitBtnOkClick(Sender: TObject);
begin
  if (StrToFloat(MinVal.Text) > StrToFloat(MaxVal.Text))
    then
      begin
        ShowMessage('El valor mínimo no puede superar al valor máximo');
        ModalResult := mrNone;
      end;
end;

procedure TNumericForm.SetBtnClick(Sender: TObject);
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

procedure TNumericForm.RemoveBtnClick(Sender: TObject);
begin
  VarList.Items.AddObject(TAttribute(DepenList.Items.Objects[DepenList.ItemIndex]).Name,
    DepenList.Items.Objects[DepenList.ItemIndex]);
  DepenList.Items.Delete(DepenList.ItemIndex);
  RemoveBtn.Enabled := false;
//  SetBtn.Enabled := false;
end;

procedure TNumericForm.AddBtnClick(Sender: TObject);
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

procedure TNumericForm.VarListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if VarList.ItemIndex >= 0
    then AddBtn.Enabled := true;
end;

procedure TNumericForm.DepenListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DepenList.ItemIndex >= 0
    then
      begin
        RemoveBtn.Enabled := true;
//        SetBtn.Enabled := true;
      end;
end;

procedure TNumericForm.FormShow(Sender: TObject);
begin
  AddBtn.Enabled := false;
  RemoveBtn.Enabled := false;
//  SetBtn.Enabled := false;
end;

procedure TNumericForm.SetTitle(const VarName: string);
begin
  Caption := FTitle + VarName;
end;

end.
