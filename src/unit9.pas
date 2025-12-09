Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private

  public
    Procedure Start;
    Procedure Stop;
  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Shutdown..';
End;

Procedure TForm9.Button1Click(Sender: TObject);
Begin
  Stop;
End;

Procedure TForm9.Timer1Timer(Sender: TObject);
Begin
  ProgressBar1.Position := ProgressBar1.Position + 1;
  If ProgressBar1.Position = ProgressBar1.Max Then Begin
    Timer1.Enabled := false;
    ModalResult := mrOK;
  End;
End;

Procedure TForm9.Start;
Begin
  ProgressBar1.Max := 15;
  ProgressBar1.Position := 0;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
End;

Procedure TForm9.Stop;
Begin
  Timer1.Enabled := false;
  ModalResult := mrCancel;
End;

End.

