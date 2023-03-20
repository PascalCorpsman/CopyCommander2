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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus, Grids, Types;

Type

  tlistViewData = Record
    Left, Right: String;
    Direction: integer;
  End;

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure StringGrid1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
  private
    fLeftRootDirectory, fRightRootDirectory: String;
    fListViewData: Array Of tlistViewData;
    Procedure UpdatePanelInfo;

    Procedure IterThroughAllSelected(aDirection: String);
  public
    Function LoadDirectories(LeftDir, RightDir: String): String;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses LazFileUtils, ucopycommander, Unit1, lclintf, LCLType;

Const
  IndexDoNothing = 7;
  IndexLeftToRight = 6;
  IndexRightToLeft = 3;

Type
  TccFile = Record
    FileName: String;
    //LastModifiedTimeStamp: Longint; -- Das wäre Cool aber das erzeugt nur schrott, ggf kann uCopyCommander.pas (GetFileModifiedTime) das besser ?
    FileSize: int64;
  End;

  TccFileList = Array Of TccFile;

  TFilecontainer = Record
    Files: TccFileList;
    FilesPtr: Integer; // Anzahl der Gültigen Einträge in Files
  End;


Procedure Scan(Var FileContainer: TFilecontainer; Const ScanRoot: String; adirectory: String);

  Procedure ProceedFind(Filename: String; FileSize: int64);
  Begin
    delete(Filename, 1, length(ScanRoot));
    FileContainer.Files[FileContainer.FilesPtr].FileName := Filename;
    FileContainer.Files[FileContainer.FilesPtr].FileSize := FileSize;
    FileContainer.FilesPtr := FileContainer.FilesPtr + 1;
    If FileContainer.FilesPtr > high(FileContainer.Files) Then Begin
      setlength(FileContainer.Files, length(FileContainer.Files) + 1024);
    End;
  End;

Var
  SR: TSearchRec;
Begin
  adirectory := IncludeTrailingPathDelimiter(adirectory);
  If (FindFirstUTF8(aDirectory + '*', faAnyFile, SR) = 0) Then Begin
    Repeat
      // Dank dieser Variante sind wir case insensitiv, obwohl es das Betriebsystem eventuell ist !
      If (SR.Name <> '.') And (SR.Name <> '..') And (SR.Attr And FaDirectory <> FaDirectory) Then Begin
        ProceedFind(aDirectory + SR.Name, sr.Size);
      End;
      (*
       * Rekursiver Abstieg
       *)
      If (SR.Name <> '.') And (SR.Name <> '..') And (SR.Attr And FaDirectory = FaDirectory) Then
        Scan(FileContainer, ScanRoot, aDirectory + SR.Name);
    Until (FindNextUTF8(SR) <> 0);
    FindCloseUTF8(SR);
  End;
End;

Procedure Quick(Var arr: TccFileList; li, re: integer);
Var
  l, r: integer;
  h: TccFile;
  p: String;
Begin
  If Li < Re Then Begin
    // Achtung, das Pivotelement darf nur einmal vor den While schleifen ausgelesen werden, danach nicht mehr !!
    p := arr[Trunc((li + re) / 2)].Filename; // Auslesen des Pivo Elementes
    l := Li;
    r := re;
    While l < r Do Begin
      While CompareStr(arr[l].Filename, p) < 0 Do
        inc(l);
      While CompareStr(arr[r].Filename, p) > 0 Do
        dec(r);
      If L <= R Then Begin
        h := arr[l];
        arr[l] := arr[r];
        arr[r] := h;
        inc(l);
        dec(r);
      End;
    End;
    quick(arr, li, r);
    quick(arr, l, re);
  End;
End;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Caption := 'Synchronise..';
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  // Close
  Close;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Var
  i: integer;
  sFile: String;
  job: TJob;
Begin
  // OK -> Generieren der Jobs und Los gehts ;)
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    job := Nil;
    Case StringGrid1.Cells[1, i] Of
      chr(IndexLeftToRight + ord('0')): Begin
          sFile := fLeftRootDirectory + fListViewData[i - 1].Left;
          If FileExistsUTF8(sFile) Then Begin
            job := TJob.Create;
            job.JobType := jtCopyFile;
            job.Source := sFile;
            job.Dest := fRightRootDirectory + fListViewData[i - 1].Left;
          End;
        End;
      chr(IndexRightToLeft + ord('0')): Begin
          sFile := fRightRootDirectory + fListViewData[i - 1].Right;
          If FileExistsUTF8(sFile) Then Begin
            job := TJob.Create;
            job.JobType := jtCopyFile;
            job.Source := sFile;
            job.Dest := fLeftRootDirectory + fListViewData[i - 1].Right;
          End;
        End;
    End;
    If assigned(job) Then Begin
      (*
       * Da der User Explizit gewünscht hat hier zu überschreiben, brauchen
       * wir nicht nachher nicht mehr zu fragen und können die Zieldatei
       * jetzt schon löschen
       *)
      If FileExistsUTF8(job.Dest) Then Begin
        If Not DeleteFileUTF8(job.Dest) Then Begin
          Showmessage('Unable to delete: ' + job.Dest);
        End;
      End;
      form1.AddJob(job);
    End;
  End;
  close;
End;

Procedure TForm3.FormResize(Sender: TObject);
Begin
  StringGrid1.Columns[0].Width := width Div 2 - 40;
  StringGrid1.Columns[1].Width := 20;
  StringGrid1.Columns[2].Width := width Div 2 - 40;
End;

Procedure TForm3.FormShow(Sender: TObject);
Var
  i: Integer;
Begin
  If StringGrid1.RowCount <> length(fListViewData) Then Begin
    StringGrid1.BeginUpdate;
    StringGrid1.Clear;
    StringGrid1.RowCount := length(fListViewData) + 1;
    For i := 0 To high(fListViewData) Do Begin
      StringGrid1.cells[0, i + 1] := fListViewData[i].Left;
      StringGrid1.cells[1, i + 1] := inttostr(fListViewData[i].Direction);
      StringGrid1.cells[2, i + 1] := fListViewData[i].Right;
    End;
    StringGrid1.EndUpdate(true);
    UpdatePanelInfo;
  End;
End;

Procedure TForm3.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Begin
  If (aCol = 1) And (arow <> 0) Then Begin
    StringGrid1.Canvas.Rectangle(aRect.Left - 1, aRect.Top - 1, aRect.Right, aRect.Bottom);
    Case StringGrid1.Cells[aCol, aRow] Of
      chr(IndexDoNothing + ord('0')): Begin
          form1.AppIcons.Draw(StringGrid1.Canvas, arect.Left + 2, aRect.Top + 2, IndexDoNothing);
        End;
      chr(IndexLeftToRight + ord('0')): Begin
          form1.AppIcons.Draw(StringGrid1.Canvas, arect.Left + 2, aRect.Top + 2, IndexLeftToRight);
        End;
      chr(IndexRightToLeft + ord('0')): Begin
          form1.AppIcons.Draw(StringGrid1.Canvas, arect.Left + 2, aRect.Top + 2, IndexRightToLeft);
        End;
    End;
  End;
End;

Procedure TForm3.StringGrid1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  r: TRect;
Begin
  // STRG + A = Alles Markieren
  If (ssCtrl In shift) And (key = ord('A')) Then Begin
    r.top := 0;
    r.Left := 0;
    r.Right := 2;
    r.Bottom := StringGrid1.RowCount - 1;
    StringGrid1.Selection := r;
    exit;
  End;
  If key = ord('N') Then Begin
    IterThroughAllSelected(IntToStr(IndexDoNothing));
  End;
  If key = ord('R') Then Begin
    IterThroughAllSelected(IntToStr(IndexLeftToRight));
  End;
  If key = ord('L') Then Begin
    IterThroughAllSelected(IntToStr(IndexRightToLeft));
  End;
  If key = VK_ESCAPE Then Begin
    close;
    exit;
  End;
  UpdatePanelInfo;
End;

Procedure TForm3.MenuItem1Click(Sender: TObject);
Var
  key: Word;
Begin
  // Copy ->
  key := Ord('R');
  StringGrid1KeyDown(Nil, key, []);
End;

Procedure TForm3.MenuItem2Click(Sender: TObject);
Var
  key: Word;
Begin
  // Copy <-
  key := Ord('L');
  StringGrid1KeyDown(Nil, key, []);
End;

Procedure TForm3.MenuItem3Click(Sender: TObject);
Var
  key: Word;
Begin
  // Do Nothing
  key := Ord('N');
  StringGrid1KeyDown(Nil, key, []);
End;

Procedure TForm3.MenuItem4Click(Sender: TObject);
Var
  j, i, k: Integer;
  sel: TGridRect;
  fn: String;
Begin
  // Del Left
  For i := StringGrid1.SelectedRangeCount - 1 Downto 0 Do Begin
    sel := StringGrid1.SelectedRange[i];
    For j := sel.Bottom Downto sel.Top Do Begin
      fn := fLeftRootDirectory + fListViewData[j - 1].Left;
      If DeleteFileUTF8(fn) Then Begin
        fListViewData[j - 1].Left := '';
        // Rechts ists auch Leer -> Die Zeile kann Weg
        If fListViewData[j - 1].Right = '' Then Begin
          StringGrid1.DeleteRow(j);
          For k := j - 1 To high(fListViewData) - 1 Do Begin
            fListViewData[k] := fListViewData[k + 1];
          End;
          setlength(fListViewData, high(fListViewData));
        End
        Else Begin
          // Rechts gibt es -> die Zeile bleibt da
          StringGrid1.Cells[0, j] := '';
          StringGrid1.Cells[1, j] := inttostr(IndexRightToLeft);
        End;
      End;
    End;
  End;
  StringGrid1.ClearSelections;
  UpdatePanelInfo;
End;

Procedure TForm3.MenuItem5Click(Sender: TObject);
Var
  j, i, k: Integer;
  sel: TGridRect;
  fn: String;
Begin
  // Del Right
  For i := StringGrid1.SelectedRangeCount - 1 Downto 0 Do Begin
    sel := StringGrid1.SelectedRange[i];
    For j := sel.Bottom Downto sel.Top Do Begin
      fn := fRightRootDirectory + fListViewData[j - 1].Right;
      If DeleteFileUTF8(fn) Then Begin
        fListViewData[j - 1].Right := '';
        // Links ists auch Leer -> Die Zeile kann Weg
        If fListViewData[j - 1].Left = '' Then Begin
          StringGrid1.DeleteRow(j);
          For k := j - 1 To high(fListViewData) - 1 Do Begin
            fListViewData[k] := fListViewData[k + 1];
          End;
          setlength(fListViewData, high(fListViewData));
        End
        Else Begin
          // Links gibt es -> die Zeile bleibt da
          StringGrid1.Cells[2, j] := '';
          StringGrid1.Cells[1, j] := inttostr(IndexLeftToRight);
        End;
      End;
    End;
  End;
  StringGrid1.ClearSelections;
  UpdatePanelInfo;
End;

Procedure TForm3.MenuItem7Click(Sender: TObject);
Var
  folder, filename: String;
Begin
  // Openleft Folder
  filename := fLeftRootDirectory + fListViewData[StringGrid1.Selection.Top].Left;
  folder := ExtractFileDir(filename);
  If DirectoryExistsUTF8(folder) And (folder <> '') Then Begin
    OpenURL(folder);
  End;
End;

Procedure TForm3.MenuItem9Click(Sender: TObject);
Var
  folder, filename: String;
Begin
  // Open Right Folder
  filename := fLeftRootDirectory + fListViewData[StringGrid1.Selection.Top].Right;
  folder := ExtractFileDir(filename);
  If DirectoryExistsUTF8(folder) And (folder <> '') Then Begin
    OpenURL(folder);
  End;
End;

Procedure TForm3.UpdatePanelInfo;
Var
  i: integer;
  ltor, rtol: integer;
Begin
  ltor := 0;
  rtol := 0;
  For i := 0 To StringGrid1.RowCount - 1 Do Begin
    If StringGrid1.Cells[1, i] = chr(IndexRightToLeft + ord('0')) Then inc(rtol);
    If StringGrid1.Cells[1, i] = chr(IndexLeftToRight + ord('0')) Then inc(ltor);
  End;
  StatusBar1.Panels[0].Text := format('%d files ->, %d files <-', [ltor, rtol]);
End;

Procedure TForm3.IterThroughAllSelected(aDirection: String);
Var
  j, i: Integer;
  sel: TGridRect;
Begin
  For i := 0 To StringGrid1.SelectedRangeCount - 1 Do Begin
    sel := StringGrid1.SelectedRange[i];
    For j := sel.Top To sel.Bottom Do Begin
      StringGrid1.Cells[1, j] := aDirection;
    End;
  End;
End;

Function TForm3.LoadDirectories(LeftDir, RightDir: String): String;
Var
  ListViewItemsCount: Integer;

  Procedure Add(aLeft, ARight: String; aDir: Integer);
  Begin
    fListViewData[ListViewItemsCount].Left := aLeft;
    fListViewData[ListViewItemsCount].Right := ARight;
    fListViewData[ListViewItemsCount].Direction := aDir;
    inc(ListViewItemsCount);
    If ListViewItemsCount >= high(fListViewData) Then Begin
      SetLength(fListViewData, length(fListViewData) + 1024);
    End;
  End;

Var
  LeftFiles, RightFiles: TFilecontainer;
  i, j: integer;
Begin
  (*
   * Der Unten stehende Code ansich ist eigentlich recht schnell,
   * das Problem ist aber, dass wenn man in die Listview richtig viele Elemente rein
   * lädt, dann dauert das Showmodal ewig zum Anzeigen :-\
   *)
  result := '';
  If LeftDir = RightDir Then Begin
    result := 'Error, same path on both sides.';
    exit;
  End;
  If Not DirectoryExistsUTF8(LeftDir) Then Begin
    result := 'Error, left directory does not exist.';
    exit;
  End;
  If Not DirectoryExistsUTF8(RightDir) Then Begin
    result := 'Error, right directory does not exist.';
    exit;
  End;
  ListViewItemsCount := 0;
  SetLength(fListViewData, 1024);
  StringGrid1.Columns[0].Title.Caption := ExcludeTrailingPathDelimiter(LeftDir);
  StringGrid1.Columns[2].Title.Caption := ExcludeTrailingPathDelimiter(RightDir);
  LeftDir := IncludeTrailingPathDelimiter(LeftDir);
  RightDir := IncludeTrailingPathDelimiter(RightDir);
  fLeftRootDirectory := LeftDir;
  fRightRootDirectory := RightDir;
  // Zusammen suchen der Dateien
  LeftFiles.Files := Nil;
  SetLength(LeftFiles.Files, 1024);
  LeftFiles.FilesPtr := 0;
  scan(LeftFiles, fLeftRootDirectory, fLeftRootDirectory);
  setlength(LeftFiles.Files, LeftFiles.FilesPtr);
  RightFiles.Files := Nil;
  SetLength(RightFiles.Files, 1024);
  RightFiles.FilesPtr := 0;
  scan(RightFiles, fRightRootDirectory, fRightRootDirectory);
  setlength(RightFiles.Files, RightFiles.FilesPtr);
  // Scan findet zwar alles aber "unordentlich" ohne Sortierung geht das nicht..
  Quick(LeftFiles.Files, 0, high(LeftFiles.Files));
  Quick(RightFiles.Files, 0, high(RightFiles.Files));
  // Nun da wir alles haben kann Verglichen werden
  i := 0;
  j := 0;
  While (i <= high(LeftFiles.Files)) And (j <= high(RightFiles.Files)) Do Begin
{$IFDEF Windows}
    If lowercase(LeftFiles.Files[i].Filename) = lowercase(RightFiles.Files[j].Filename) Then Begin
{$ELSE}
    If LeftFiles.Files[i].Filename = RightFiles.Files[j].Filename Then Begin
{$ENDIF}
      If LeftFiles.Files[i].FileSize <> RightFiles.Files[j].FileSize Then Begin
        // Die Datei existiert auf beiden Seiten hat aber unterschiedliche "Größen"
        // Die Bevorzugte Kopierrichtung ist Groß Überlebt weil wird wohl neuer sein.
        If LeftFiles.Files[i].FileSize < RightFiles.Files[j].FileSize Then Begin
          Add(LeftFiles.Files[i].FileName, RightFiles.Files[j].FileName, IndexRightToLeft);
        End
        Else Begin
          Add(LeftFiles.Files[i].FileName, RightFiles.Files[j].FileName, IndexLeftToRight);
        End;
      End;
      inc(i);
      inc(j);
    End
    Else Begin
      // Das Alpha Numerisch "Kleinere" wird behandelt und weiter gezählt
      If CompareStr(LeftFiles.Files[i].Filename, RightFiles.Files[j].Filename) < 0 Then Begin
        Add(LeftFiles.Files[i].FileName, '', IndexLeftToRight);
        inc(i);
      End
      Else Begin
        Add('', RightFiles.Files[j].FileName, IndexRightToLeft);
        inc(j);
      End;
    End;
  End;
  // Liste der neu Hinzu gekommen Dateien
  While i <= High(LeftFiles.Files) Do Begin
    Add(LeftFiles.Files[i].FileName, '', IndexLeftToRight);
    inc(i);
  End;
  // Liste der Gelöschten Dateien
  While j <= High(RightFiles.Files) Do Begin
    Add('', RightFiles.Files[j].FileName, IndexRightToLeft);
    inc(j);
  End;
  SetLength(fListViewData, ListViewItemsCount);
  SetLength(RightFiles.Files, 0);
  SetLength(LeftFiles.Files, 0);
  If ListViewItemsCount = 0 Then Begin
    result := 'Folders are equal.';
  End;
End;

End.

