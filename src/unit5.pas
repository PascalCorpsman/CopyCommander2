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
Unit Unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ucopycommander;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Answer: TJobAnswers;
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Question';
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  // replace
  answer := jaReplace;
  close;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Begin
  // Skip File
  answer := jaSkip;
  close;
End;

Procedure TForm5.Button3Click(Sender: TObject);
Begin
  // Cancel all
  form1.fWorkThread.CancelAllJobs();
  CheckBox1.Checked := false;
  answer := jaSkip;
  close;
End;

End.

