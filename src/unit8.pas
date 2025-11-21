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
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure InitWith(ext, cmd, params: String);

  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

{ TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'File ext association editor';
End;

Procedure TForm8.InitWith(ext, cmd, params: String);
Begin
  edit1.text := ext; // ; liste erlaubt
  edit2.text := cmd;
  edit3.text := params; // Formatstring der "%f" durch den Dateinamen ersetzt
End;

End.

