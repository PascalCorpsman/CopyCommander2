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
Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ucopycommander;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
  private
    Procedure Clear;
  public
    Procedure AddErrorJob(EJ: TErrorJob);

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses Unit1;

{ TForm4 }

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  Clear;
  Close;
End;

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Errorlog..';
End;

Procedure TForm4.FormDestroy(Sender: TObject);
Begin
  Clear;
End;

Procedure TForm4.MenuItem1Click(Sender: TObject);
Begin
  If listbox1.ItemIndex <> -1 Then Begin
    form1.AddToJobQueue(TJob(ListBox1.Items.Objects[listbox1.ItemIndex]));
    ListBox1.Items.Delete(listbox1.ItemIndex);
  End;
End;

Procedure TForm4.Clear;
Var
  i: Integer;
Begin
  // Alle Fehler Jobs Frei geben
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    ListBox1.Items.Objects[i].Free;
  End;
  ListBox1.Clear;
End;

Procedure TForm4.AddErrorJob(EJ: TErrorJob);
Begin
  ListBox1.AddItem(ej.ErrorMessage + ': ' + ej.Job.Source, ej.Job);
  If Visible Then Begin
    BringToFront;
  End
  Else Begin
    Show;
  End;
End;

End.

