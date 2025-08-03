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
Unit udirsync;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  TRenameEntry = Record
    SourceFile: String;
    DestFile: String;
    FileSize: Int64;
  End;

  TRenameList = Array Of TRenameEntry;

  TFileEntry = Record
    FileName: String;
    FileSize: Int64;
  End;

  TFileList = Array Of TFileEntry;

  TReportInfos = Record
    RenameInfo: String;
    CopyInfo: String;
    DelInfo: String;
  End;

  (*
   * List alle Dateien in und unter aDir ein und gibt deren Relativen Pfad zu aDir als Buffer zurück
   *)
Procedure ScanDirToBuffer(Const aDir: String; Var buffer: TFileList);

(*
 * Sortiert die Dateiliste (eigentlich egal wonach, hauptsache sortiert..
 *)
Procedure SortFileList(Var aList: TFileList);

(*
 * Erzeugt aus SourceDir, TargetDir die Joblisten, mit dem Ziel möglichst wenig kopieren, möglichst viel Umbenennen.
 * Die Grundidee ist dass es keine 2 Dateien gibt die Gleich sind, dabei wird folgt vorgegangen:
 * 1. Dateinamen Vergleich
 * 2. Dateigrößen Vergleich
 *  => beides Gleich -> ignorieren
 *     sonst -> Target -> Delliste, Source -> Copyliste
 * 3. Versuch aus der Delliste Dateien in die Rename Liste zu bekommen
 *  => Dazu Vergleich Copy Liste mit Delliste.
 *     Dateien die Kopiert werden sollen, aber "gleich" (Größe / md5) sind mit Dateien in der Delliste
 *     werden aus beiden listen entfernt und stattdessen umbenannt.
 *)
Procedure GenerateJobLists(SourceDir, TargetDir: String; Const SourceFiles, DestFiles: TFileList; Var RenameList: TRenameList; Var CopyList, DelList: TFileList; MD5Comparing: Boolean);

(*
 * Summiert alle aList.SourceFileSize
 *)
Function FileListToSize(Const aList: TFileList): int64;
Function RenameFileListToSize(Const aList: TRenameList): int64;

(*
 * Wandelt eine Dateigröße in Bytes um in einen "pretty" Printed String
 *)
Function FileSizeToString(Value: int64): String;

Function PrettyTime(Time_in_ms: UInt64): String;

Procedure CreateReportFile(FileName, SourceDir, TargetDir: String; Const RenameList: TRenameList; Const CopyList, DelList: TFileList; Const Info: TReportInfos);

Implementation

Uses LazFileUtils, md5, Dialogs;

Const
  BufferBlockSize = 1024;

Procedure ScanDirToBuffer(Const aDir: String; Var buffer: TFileList);
Var
  BufferCnt: integer;
  CropLen: integer;

  Procedure AddFileToBuffer(Const aSubDir: String; Const aFile: TSearchRec);
  Begin
    If BufferCnt >= Length(buffer) Then Begin
      setlength(buffer, length(buffer) + BufferBlockSize);
    End;
    buffer[BufferCnt].FileName := aSubDir + aFile.Name;
    delete(buffer[BufferCnt].FileName, 1, CropLen);
    buffer[BufferCnt].FileSize := aFile.Size;
    inc(BufferCnt);
  End;

  Procedure Scan(aSubDir: String);
  Var
    Info: TSearchRec;
  Begin
    aSubDir := IncludeTrailingPathDelimiter(aSubDir);
    If FindFirstUTF8(aSubDir + '*', faAnyFile, Info) = 0 Then Begin
      Repeat
        If (info.Attr And faDirectory) = faDirectory Then Begin
          If (info.Name <> '.') And (info.Name <> '..') Then Begin
            Scan(aSubDir + info.Name);
          End;
        End
        Else Begin
          AddFileToBuffer(aSubDir, info);
        End;
      Until FindNextUTF8(info) <> 0;
      FindCloseUTF8(Info);
    End;
  End;
Begin
  setlength(Buffer, BufferBlockSize);
  BufferCnt := 0;
  CropLen := length(IncludeTrailingPathDelimiter(aDir));
  Scan(aDir);
  setlength(buffer, BufferCnt);
End;

Procedure SortFileList(Var aList: TFileList);
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: TFileEntry;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einmal vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := aList[Trunc((li + re) / 2)].FileName; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(aList[l].FileName, p) < 0 Do
          inc(l);
        While CompareStr(aList[r].FileName, p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := aList[l];
          aList[l] := aList[r];
          aList[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(aList));
End;

Procedure GenerateJobLists(SourceDir, TargetDir: String; Const SourceFiles,
  DestFiles: TFileList; Var RenameList: TRenameList; Var CopyList,
  DelList: TFileList; MD5Comparing: Boolean);

Var
  RenameListCnt, CopyListCnt, DelListCnt: integer;

  Procedure AddToCopyList(SourceFilesIndex: integer);
  Begin
    If CopyListCnt >= Length(CopyList) Then Begin
      setlength(CopyList, length(CopyList) + BufferBlockSize);
    End;
    CopyList[CopyListCnt].FileName := SourceFiles[SourceFilesIndex].FileName;
    CopyList[CopyListCnt].FileSize := SourceFiles[SourceFilesIndex].FileSize;
    inc(CopyListCnt);
  End;

  Procedure AddToMoveList(DelListIndex, CopyListIndex: integer);
  Begin
    If RenameListCnt >= Length(RenameList) Then Begin
      setlength(RenameList, length(RenameList) + BufferBlockSize);
    End;
    RenameList[RenameListCnt].SourceFile := DelList[DelListIndex].FileName;
    RenameList[RenameListCnt].DestFile := CopyList[CopyListIndex].FileName;
    RenameList[RenameListCnt].FileSize := CopyList[CopyListIndex].FileSize;
    inc(RenameListCnt);
  End;

  Procedure AddToDelList(DestFilesIndex: integer);
  Begin
    If DelListCnt >= Length(DelList) Then Begin
      setlength(DelList, length(DelList) + BufferBlockSize);
    End;
    DelList[DelListCnt].FileName := DestFiles[DestFilesIndex].FileName;
    DelList[DelListCnt].FileSize := DestFiles[DestFilesIndex].FileSize;
    inc(DelListCnt);
  End;

Var
  s, d, i, j, k: Integer;
  CopyHash, DelHash, tmp: String;
Begin
  SourceDir := IncludeTrailingPathDelimiter(SourceDir);
  TargetDir := IncludeTrailingPathDelimiter(TargetDir);
  setlength(RenameList, BufferBlockSize);
  setlength(CopyList, BufferBlockSize);
  setlength(DelList, BufferBlockSize);
  RenameListCnt := 0;
  CopyListCnt := 0;
  DelListCnt := 0;
  // 1. Naives erstellen der Listen
  s := 0;
  d := 0;
  While (s < length(SourceFiles)) And (d < length(DestFiles)) Do Begin
    If SourceFiles[s].FileName = DestFiles[d].FileName Then Begin
      If SourceFiles[s].FileSize = DestFiles[d].FileSize Then Begin
        // Beide sind Identisch ;)
        inc(s);
        inc(d);
      End
      Else Begin
        // Unterschiedlich -> In die CopyList
        AddToCopyList(s);
        inc(s);
        inc(d);
      End;
    End
    Else Begin
      // Schauen, welche der Beiden Listen "weiter" gezählt werden muss
      If CompareStr(SourceFiles[s].FileName, DestFiles[d].FileName) > 0 Then Begin
        AddToDelList(d);
        inc(d);
      End
      Else Begin
        // Die Sourcefile gibt es in Dest nicht
        AddToCopyList(s);
        inc(s);
      End;
    End;
  End;
  // Nun noch die "Reste" einsammeln
  While d < length(DestFiles) Do Begin
    AddToDelList(d);
    inc(d);
  End;
  While s < length(SourceFiles) Do Begin
    AddToCopyList(s);
    inc(s);
  End;
  // Schauen ob wir ohne MD5 comparing Eindeutigkeit haben ..
  If Not MD5Comparing Then Begin
    tmp := '';
    For j := 0 To DelListCnt - 1 Do Begin
      For i := j + 1 To DelListCnt - 1 Do Begin
        If DelList[i].FileSize = DelList[j].FileSize Then Begin
          tmp := tmp + LineEnding + DelList[i].FileName + ' <-> ' + DelList[j].FileName;
        End;
      End;
    End;
    If tmp <> '' Then Begin
      showmessage('Warning, file rename heuristic will fail on the following files (this could be avoided, if md5 comparing is enabled): ' + tmp);
    End;
  End;
  // 2. Versuch die Listen zu "optimieren"
  //    \-> Ist ein Dellist Eintrag in der Copy Liste -> RenameListe
  For j := DelListCnt - 1 Downto 0 Do Begin
    DelHash := '';
    For i := 0 To CopyListCnt - 1 Do Begin
      If CopyList[i].FileSize = DelList[j].FileSize Then Begin
        If MD5Comparing Then Begin
          If DelHash = '' Then Begin
            DelHash := MD5Print(MD5File(TargetDir + DelList[j].FileName));
          End;
          CopyHash := MD5Print(MD5File(SourceDir + CopyList[i].FileName));
        End
        Else Begin
          // Egal was, hauptsache Gleich ..
          DelHash := 'Match';
          CopyHash := 'Match';
        End;
        // Eine zu Löschende Datei wurde nur "umbenannt"
        If DelHash = CopyHash Then Begin
          // 1. Den Move Auftrag Generieren
          AddToMoveList(j, i);
          // 2. die Delliste Kürzen
          For k := j To DelListCnt - 2 Do Begin
            DelList[k] := DelList[k + 1];
          End;
          dec(DelListCnt);
          // 3. die Copy Liste kürzen
          For k := i To CopyListCnt - 2 Do Begin
            CopyList[k] := CopyList[k + 1];
          End;
          dec(CopyListCnt);
          break;
        End;
      End;
    End;
  End;
  // Kürzen der Listen auf das was es tatsächlich geworden ist.
  setlength(RenameList, RenameListCnt);
  setlength(CopyList, CopyListCnt);
  setlength(DelList, DelListCnt);
End;

Function FileListToSize(Const aList: TFileList): int64;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(aList) Do Begin
    result := result + aList[i].FileSize;
  End;
End;

Function RenameFileListToSize(Const aList: TRenameList): int64;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(aList) Do Begin
    result := result + aList[i].FileSize;
  End;
End;

Function FileSizeToString(Value: int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

Function PrettyTime(Time_in_ms: UInt64): String;
Var
  hs, digits, sts, sep, s: String;
  st, i: integer;
  b: Boolean;
Begin
  s := 'ms';
  hs := '';
  sep := DefaultFormatSettings.DecimalSeparator;
  st := 0;
  b := false;
  digits := '3';
  // [0 .. 60[ s
  If Time_in_ms >= 1000 Then Begin
    st := Time_in_ms Mod 1000;
    Time_in_ms := Time_in_ms Div 1000;
    s := 's';
    b := true;
  End;
  // [1 .. 60[ min
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'min';
    sep := DefaultFormatSettings.TimeSeparator;
    digits := '2';
  End
  Else
    b := false;
  // [1 .. 24[ h
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'h';
  End
  Else
    b := false;
  // [1 ..  d
  If (Time_in_ms >= 24) And b Then Begin
    st := Time_in_ms Mod 24;
    Time_in_ms := Time_in_ms Div 24;
    hs := 'd';
    If st <> 0 Then s := 'h';
    sep := ' ';
    digits := '1';
  End
  Else
    b := false;
  // Ausgabe mit oder ohne Nachkomma
  If st <> 0 Then Begin
    sts := format('%0.' + digits + 'd', [st]);
    If (s = 's') Then Begin // Bei Sekunden die endenden 0-en löschen
      For i := length(sts) Downto 1 Do Begin
        If sts[i] = '0' Then Begin
          delete(sts, i, 1);
        End
        Else Begin
          break;
        End;
      End;
    End;
    result := inttostr(Time_in_ms) + hs + sep + sts + s;
  End
  Else Begin
    result := inttostr(Time_in_ms) + s;
  End;
End;

Procedure CreateReportFile(FileName, SourceDir, TargetDir: String;
  Const RenameList: TRenameList; Const CopyList, DelList: TFileList;
  Const Info: TReportInfos);
Var
  sl: TStringList;
  i: Integer;
Begin
  SourceDir := IncludeTrailingPathDelimiter(SourceDir);
  TargetDir := IncludeTrailingPathDelimiter(TargetDir);
  sl := TStringList.create;
  If length(RenameList) <> 0 Then Begin
    sl.add('Renamelist;"' + info.RenameInfo + '"');
    sl.add('From;To;');
    For i := 0 To high(RenameList) Do Begin
      sl.add(
        '"' + TargetDir + RenameList[i].SourceFile + '";' +
        '"' + TargetDir + RenameList[i].DestFile + '"'
        );
    End;
  End;
  If length(CopyList) <> 0 Then Begin
    sl.add('Copylist;"' + info.CopyInfo + '"');
    sl.add('From;To;');
    For i := 0 To high(CopyList) Do Begin
      sl.add(
        '"' + SourceDir + CopyList[i].FileName + '";' +
        '"' + TargetDir + CopyList[i].FileName + '"'
        );
    End;
  End;
  If length(DelList) <> 0 Then Begin
    sl.add('DelList;"' + Info.DelInfo + '"');
    sl.add('File');
    For i := 0 To high(DelList) Do Begin
      sl.add(
        '"' + TargetDir + DelList[i].FileName + '"'
        );
    End;
  End;
  sl.SaveToFile(FileName);
  sl.free;
End;

End.

