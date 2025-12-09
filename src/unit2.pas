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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, TAGraph, TASeries, ucopycommander;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Chart1AxisList0MarkToText(Var AText: String; AMark: Double);
    Procedure Chart1AxisList1MarkToText(Var AText: String; AMark: Double);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
  private
    fTPBufferSum: QWord;
    fTPBuffer: Array[0..9] Of QWord;
    fTPBuffer_ptr: integer;
  public
    Procedure AddNewData(Const Statistic: TTransfereStatistic);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  ProgressBar1.Position := 0;
  Label2.caption := '';
  label4.caption := '';
  Panel1.Caption := '';
  Panel2.Caption := '';
  TreeView1.Align := alClient;
  caption := 'Job Progress.';
  Chart1LineSeries1.SeriesColor := clGreen;
  Chart1LineSeries2.SeriesColor := $00C000;
  For i := 0 To high(fTPBuffer) Do Begin
    fTPBuffer[i] := 0;
  End;
  fTPBuffer_ptr := 0;
  fTPBufferSum := 0;
  Splitter1.MinSize := Panel1.Height;
End;

Procedure TForm2.AddNewData(Const Statistic: TTransfereStatistic);
Var
  TimeInmS, AvgPerS: Int64;
  totalpercent: UInt64;
Begin
  Chart1LineSeries1.Add(Statistic.TransferedBytesInLast1000ms);

  // Tiefpass über die letzten 10s
  fTPBufferSum := fTPBufferSum + Statistic.TransferedBytesInLast1000ms - fTPBuffer[fTPBuffer_ptr];
  fTPBuffer[fTPBuffer_ptr] := Statistic.TransferedBytesInLast1000ms;
  fTPBuffer_ptr := (fTPBuffer_ptr + 9) Mod length(fTPBuffer);
  Chart1LineSeries2.Add(fTPBufferSum / length(fTPBuffer));
  AvgPerS := fTPBufferSum Div length(fTPBuffer);
  TimeInmS := 0;
  If AvgPerS <> 0 Then Begin
    TimeInmS := 1000; // Force Calculations to be done in uint64
    TimeInmS := (Statistic.BytesToCopyToFinishJobs * TimeInmS) Div AvgPerS;
  End;
  TimeInmS := TimeInmS - (TimeInmS Mod 1000); // die ms 0en das macht so eigentlich keinen Sinn.
  Label4.Caption := 'Average: ' + FileSizeToString(AvgPerS) + '/s, actual: ' + FileSizeToString(Statistic.TransferedBytesInLast1000ms) + '/s';
  label5.caption := 'Progress: ' + FileSizeToString(Statistic.BytesToCopyToFinishJobs) + ' to copy, will take aprox: ' + PrettyTime(TimeInmS);
  // max 100 Datenpunkte
  If Chart1Lineseries1.Count > 100 Then Begin
    Chart1Lineseries1.Delete(0);
    Chart1Lineseries2.Delete(0);
  End;
  If Statistic.TotalJobBytes <> 0 Then Begin
    totalpercent := 100; // Force Calculations to be done in uint64
    totalpercent := (Statistic.BytesCopiedInJobs * totalpercent) Div Statistic.TotalJobBytes;
    ProgressBar2.Position := totalpercent;
  End
  Else Begin
    ProgressBar2.Position := 0;
  End;
  StatusBar1.Panels[0].Text := format('Pending jobs (subjobs): %d (%d)', [Statistic.JobsToDo, Statistic.SubJobsTodo]);
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  If Button3.Caption = 'Pause' Then Begin
    Button3.Caption := 'Continue';
    form1.fWorkThread.JobPause := true;
  End
  Else Begin
    Button3.Caption := 'Pause';
    form1.fWorkThread.JobPause := false;
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  form1.fWorkThread.CancelAllJobs;
  ProgressBar1.Position := 0;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  // Delete Job
  If TreeView1.Selected <> Nil Then Begin
    form1.fWorkThread.CancelJob(TJob(TJob(TreeView1.Selected.Data)));
  End;
End;

Procedure TForm2.Chart1AxisList0MarkToText(Var AText: String; AMark: Double);
Begin
  AText := FileSizeToString(trunc(AMark));
End;

Procedure TForm2.Chart1AxisList1MarkToText(Var AText: String; AMark: Double);
Var
  x: integer;
Begin
  // TODO: evtl könnte man hier die "Komma" werte die ganz am Anfang entstehen noch auf '' setzen
  x := Chart1LineSeries1.count - trunc(aMark - Chart1LineSeries1.XValue[0]);
  atext := inttostr(-x);
End;

Procedure TForm2.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  Form2.CheckBox1.Checked := false;
End;

End.

