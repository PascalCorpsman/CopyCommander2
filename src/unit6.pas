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
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, udirsync,
  ucopycommander;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    SourceFiles, DestFiles: TFileList;

    RenameList: TRenameList; // Umbenennungen in Destfiles
    CopyList, // Kopieen von Source nach Dest
    DelList: TFileList; // Löschungen in DestFiles

  public
    Procedure Init(Source, Target: String);

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses unit1, LazFileUtils, LCLType;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Sync';
End;

Procedure TForm6.Init(Source, Target: String);
Begin
  Label3.Caption := Source;
  Label4.Caption := Target;
  Memo1.Clear;

  SourceFiles := Nil;
  DestFiles := Nil;

  RenameList := Nil;
  CopyList := Nil;
  DelList := Nil;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Begin
  // Exchange Target / Source
  Init(Label4.Caption, Label3.Caption);
End;

Procedure TForm6.Button2Click(Sender: TObject);
Var
  s: String;
  oldN, N: QWord;

  Procedure AppendLog(aText: String);
  Begin
    memo1.Append(aText);
    Application.ProcessMessages;
  End;

  Procedure AppendTimeDelta(aText: String);
  Begin
    n := GetTickCount64;
    If CheckBox1.Checked Then Begin
      AppendLog(aText + ', ' + PrettyTime(n - oldN));
    End;
    oldN := n;
  End;
Begin
  If Not DirectoryExistsutf8(label3.Caption) Then Begin
    ShowMessage('Error: "' + label3.Caption + '" is not a valid source directory.');
    exit;
  End;
  If Not DirectoryExistsutf8(label4.Caption) Then Begin
    ShowMessage('Error: "' + label4.Caption + '" is not a valid target directory.');
    exit;
  End;
  Memo1.clear;
  oldN := GetTickCount64;
  AppendLog('Start: ' + FormatDateTime('YYYY.MM.DD, HH:NN:SS', now));
  SourceFiles := Nil;
  DestFiles := Nil;
  // Scan Source and Sort
  ScanDirToBuffer(label3.Caption, SourceFiles);
  AppendTimeDelta('finished scaning source');
  SortFileList(SourceFiles);
  AppendTimeDelta('finished sorting source');
  AppendLog(format('Found %d files in %s', [length(SourceFiles), label3.Caption]));
  // Scan Dest and Sort
  ScanDirToBuffer(label4.Caption, DestFiles);
  AppendTimeDelta('finished scaning target');
  SortFileList(DestFiles);
  AppendTimeDelta('finished sorting target');
  AppendLog(format('Found %d files in %s', [length(DestFiles), label4.Caption]));
  RenameList := Nil;
  CopyList := Nil;
  DelList := Nil;
  GenerateJobLists(label3.Caption, label4.Caption, SourceFiles, DestFiles, RenameList, CopyList, DelList, CheckBox2.Checked);
  AppendTimeDelta('finished generating jobs');
  s := format(
    LineEnding +
    '%d files of size %s can be renamed' + LineEnding +
    '%d files of size %s need to be copied' + LineEnding +
    '%d files of size %s need to be deleted' + LineEnding +
    '%d files are already in sync' + LineEnding
    , [
    length(RenameList), FileSizeToString(RenameFileListToSize(RenameList))
      , length(CopyList), FileSizeToString(FileListToSize(CopyList))
      , length(DelList), FileSizeToString(FileListToSize(DelList))
      , (length(SourceFiles) - (length(CopyList) + length(RenameList)))
      ]);
  AppendLog(s);
  AppendLog('Finished: ' + FormatDateTime('YYYY.MM.DD, HH:NN:SS', now));
  If (length(CopyList) = 0) And (length(DelList) = 0) And (length(RenameList) = 0) Then Begin
    showmessage('Both folders are in sync.');
  End;
End;

Procedure TForm6.Button3Click(Sender: TObject);
Begin
  // Export Lists
  If SaveDialog1.Execute Then Begin
    CreateReportFile(SaveDialog1.FileName, label3.Caption, label4.Caption, RenameList, CopyList, DelList);
  End;
End;

Procedure TForm6.Button4Click(Sender: TObject);
Var
  i: Integer;
  Res: String;
  j: TJob;
Begin
  // Apply Lists
  Res := '';
  // 1. Renamings
  For i := 0 To high(RenameList) Do Begin
    If Not RenameFileUTF8(label4.Caption + RenameList[i].SourceFile, label4.Caption + RenameList[i].DestFile) Then Begin
      res := res + LineEnding + label4.Caption + RenameList[i].SourceFile + ' -> ' + label4.Caption + RenameList[i].DestFile;
    End;
  End;
  If res <> '' Then Begin
    showmessage('Error, could not rename ' + res);
    exit;
  End;
  res := '';
  // 2. Deletions -- Das muss vorher gemacht werden
  //    1. Dass auch genug platz frei wird zum Kopieren
  //    2. Weil es sein kann, dass ein "abgebrochener Kopiervorgang" Reste übrig gelassen hat die bereinigt werden müssen.
  For i := 0 To high(DelList) Do Begin
    j := TJob.Create();
    j.Source := label4.Caption + DelList[i].FileName;
    j.Dest := '';
    j.JobType := jtDelFile;
    form1.addJob(j);
  End;
  // 3. Copy Jobs
  For i := 0 To high(CopyList) Do Begin
    // Die Zieldatei gibt es schon, vorher löschen, sonst kommen mitten drin lauter abgefragen ob oder ob nicht gelöscht werden soll..
    If FileExistsUTF8(label4.Caption + CopyList[i].FileName) Then Begin
      j := TJob.Create();
      j.Source := label4.Caption + CopyList[i].FileName;
      j.Dest := '';
      j.JobType := jtDelFile;
      form1.addJob(j);
    End;
    j := TJob.Create();
    j.Source := label3.Caption + CopyList[i].FileName;
    j.Dest := label4.Caption + CopyList[i].FileName;
    j.JobType := jtCopyFile;
    form1.addJob(j);
  End;
  // TODO: Leere Verzeichnise die durchs Löschen oder Verschieben entstanden sind -> Löschen, nur wie ?
  ModalResult := mrOK;
End;

Procedure TForm6.Button6Click(Sender: TObject);
Begin
  showmessage(
    'With the sync tool you can easily create any jobs to' + LineEnding +
    'hard sync target folder to source folder.' + LineEnding + LineEnding +
    'This means, that all files in target folder that are not existing' + LineEnding +
    'in source are automatically be deleted.' + LineEnding +
    'Renamed / moved files where recognices and handled accordingly.' + LineEnding +
    'When finished, both folders contain the same content.'
    );
End;

Procedure TForm6.CheckBox2Click(Sender: TObject);
Begin
  If CheckBox2.Checked Then Begin
    showmessage('Enabling this feature is more acurate, but can be really slow.');
  End;
End;

End.

