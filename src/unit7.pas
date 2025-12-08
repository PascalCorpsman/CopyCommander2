(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of CopyCommander2                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit7;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  IniFiles;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    StringGrid1: TStringGrid;
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure LoadFrom(Const Inifile: TIniFile);
    Procedure SaveTo(Const Inifile: TIniFile);

  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

Uses unit8;

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  caption := 'Settings';
End;

Procedure TForm7.LoadFrom(Const Inifile: TIniFile);
Var
  i: Integer;
Begin
  StringGrid1.RowCount := 1 + IniFile.ReadInteger('FileAssociations', 'Count', 0);
  For i := 0 To IniFile.ReadInteger('FileAssociations', 'Count', 0) - 1 Do Begin
    StringGrid1.Cells[0, i + 1] := inifile.ReadString('FileAssociations', 'ext' + inttostr(i), '');
    StringGrid1.Cells[1, i + 1] := inifile.ReadString('FileAssociations', 'cmd' + inttostr(i), '');
    StringGrid1.Cells[2, i + 1] := inifile.ReadString('FileAssociations', 'Params' + inttostr(i), '');
  End;
  StringGrid1.AutoSizeColumns;
  CheckBox1.Checked := Inifile.ReadBool('General', 'Show_Hidden', false);
End;

Procedure TForm7.SaveTo(Const Inifile: TIniFile);
Var
  i: Integer;
Begin
  IniFile.WriteInteger('FileAssociations', 'Count', StringGrid1.RowCount - 1);
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    inifile.WriteString('FileAssociations', 'ext' + inttostr(i - 1), StringGrid1.Cells[0, i]);
    inifile.WriteString('FileAssociations', 'cmd' + inttostr(i - 1), StringGrid1.Cells[1, i]);
    inifile.WriteString('FileAssociations', 'Params' + inttostr(i - 1), StringGrid1.Cells[2, i]);
  End;
  Inifile.WriteBool('General', 'Show_Hidden', CheckBox1.Checked);
End;

Procedure TForm7.Button3Click(Sender: TObject);
Var
  index: Integer;
Begin
  // ADD
{$IFDEF LINUX}
  form8.InitWith('.txt', 'xed', '%f');
{$ELSE}
{$IFDEF WINDOWS}
  form8.InitWith('.txt', 'notepad.exe', '%f');
{$ELSE}
  // Unknown OS
  form8.InitWith('', '', '');
{$ENDIF}
{$ENDIF}
  If form8.ShowModal = mrOK Then Begin
    index := StringGrid1.RowCount;
    StringGrid1.RowCount := StringGrid1.RowCount + 1;
    StringGrid1.Cells[0, index] := form8.Edit1.Text;
    StringGrid1.Cells[1, index] := form8.Edit2.Text;
    StringGrid1.Cells[2, index] := form8.Edit3.Text;
    StringGrid1.AutoSizeColumns;
  End;
End;

Procedure TForm7.Button4Click(Sender: TObject);
Begin
  // Edit
  If StringGrid1.Selection.Top > 0 Then Begin
    form8.InitWith(StringGrid1.Cells[0, StringGrid1.Selection.Top],
      StringGrid1.Cells[1, StringGrid1.Selection.Top],
      StringGrid1.Cells[2, StringGrid1.Selection.Top]
      );
    If form8.ShowModal = mrOK Then Begin
      StringGrid1.Cells[0, StringGrid1.Selection.Top] := form8.Edit1.Text;
      StringGrid1.Cells[1, StringGrid1.Selection.Top] := form8.Edit2.Text;
      StringGrid1.Cells[2, StringGrid1.Selection.Top] := form8.Edit3.Text;
      StringGrid1.AutoSizeColumns;
    End;
  End;
End;

Procedure TForm7.Button5Click(Sender: TObject);
Begin
  If StringGrid1.Selection.Top > 0 Then
    StringGrid1.DeleteRow(StringGrid1.Selection.Top);
End;

End.

