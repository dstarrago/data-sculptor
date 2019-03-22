unit UnitAttrSymbolic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Spin;

type
  TFormSimbolico = class(TForm)
    EditSimbolico: TSpinEdit;
    Label1: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    GroupBox2: TGroupBox;
    VarList: TListBox;
    GroupBox1: TGroupBox;
    DepenList: TListBox;
    AddBtn: TSpeedButton;
    RemoveBtn: TSpeedButton;
    Label2: TLabel;
    WeightEdit: TSpinEdit;
    SetBtn: TSpeedButton;
    RuidoEdit: TSpinEdit;
    Label3: TLabel;
    procedure EditSimbolicoChange(Sender: TObject);
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
  FormSimbolico: TFormSimbolico;

implementation

uses Main;

const
  FTitle = 'Atributo Simbólico ';

{$R *.dfm}

procedure TFormSimbolico.EditSimbolicoChange(Sender: TObject);
begin
  if (EditSimbolico.Text <> '') and (EditSimbolico.Value < EditSimbolico.MinValue)
    then EditSimbolico.Value := EditSimbolico.MinValue;
end;

procedure TFormSimbolico.SetBtnClick(Sender: TObject);
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

procedure TFormSimbolico.RemoveBtnClick(Sender: TObject);
begin
  VarList.Items.AddObject(TAttribute(DepenList.Items.Objects[DepenList.ItemIndex]).Name,
    DepenList.Items.Objects[DepenList.ItemIndex]);
  DepenList.Items.Delete(DepenList.ItemIndex);
  RemoveBtn.Enabled := false;
  SetBtn.Enabled := false;
end;

procedure TFormSimbolico.AddBtnClick(Sender: TObject);
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

procedure TFormSimbolico.VarListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if VarList.ItemIndex >= 0
    then AddBtn.Enabled := true;
end;

procedure TFormSimbolico.DepenListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DepenList.ItemIndex >= 0
    then
      begin
        RemoveBtn.Enabled := true;
        SetBtn.Enabled := true;
      end;
end;

procedure TFormSimbolico.FormShow(Sender: TObject);
begin
  AddBtn.Enabled := false;
  RemoveBtn.Enabled := false;
  SetBtn.Enabled := false;
end;

procedure TFormSimbolico.SetTitle(const VarName: string);
begin
  Caption := FTitle + VarName;
end;

end.
